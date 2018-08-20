
# load libraries and db connection
library(odbc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forecast, lib.loc = "C:/Program Files/R/R-3.5.0/library")
library(feather)
library(survival) #for KM estimates
library(ggfortify) #for plots

con <- DBI::dbConnect(odbc::odbc(), "POC")

# Base Table

DBI::dbSendQuery(conn = con
,"
if object_id('tempdb..##court_episodes') is not null
    drop table ##court_episodes;

select 
  * 
into ##court_episodes
from base.rptPlacement rp
where rp.tx_county in ('Grant', 'Lewis')
  and dbo.fnc_datediff_yrs(birthdate, removal_dt) < 18
  and (tx_lgl_stat in 
    ('Closed - Adoption'
    ,'Closed - Dependency Guardianship No Supv'
    ,'Closed - Superior Court Guardianship'
    ,'Closed - Title 13 Guardianship'
    ,'Closed -Non-parental (3rd party) custody'
    ,'Dependency Guardianship'
    ,'Dependent'
    ,'Dependent - Legally Free'
    ,'EFC Dependent'
    ,'Non-parental (3rd Party) custody'
    ,'Parental Custody -Continued Court Action'
    ,'Protective Custody'
    ,'Shelter Care'
    ,'Superior Court Guardianship')
  or tx_dsch_rsn = 'Returned to Custody of Parents - Dependency Dismissed'
  or fl_dep_exist = 1)")

# Event Table

dat_historical_events <- DBI::dbGetQuery(conn = con
,"select 
  rp.child id_prsn_child
  ,rp.removal_dt
	,iif(rp.discharge_dt = '9999-12-31', '2018-04-27', rp.discharge_dt) discharge_dt
	,iif(rp.discharge_dt = '9999-12-31', null, rp.discharge_dt) discharge_dt_null
	,iif(rp.discharge_dt = '9999-12-31', 0, 1) fl_discharge
	,datediff(dd, rp.removal_dt, 
   iif(rp.discharge_dt = '9999-12-31', '2018-04-27', rp.discharge_dt)) + 1 los
  ,rp.id_case
  ,rp.tx_county 
from ##court_episodes rp
where rp.removal_dt between '2010-01-01' and '2018-04-27'

")

# KM Estimators

historical_surv_grant <- survival::survfit(Surv(los, fl_discharge) ~ 1
                  ,data = filter(dat_historical_events, tx_county == "Grant")) 


historical_surv_lewis <- survival::survfit(Surv(los, fl_discharge) ~ 1
                  ,data = filter(dat_historical_events, tx_county == "Lewis")) 

write_feather(data_frame(time = historical_surv_grant$time
                         ,surv = historical_surv_grant$surv)
              ,"S:/Data Portal/legal_rep_planning/historical_surv_grant.feather")

write_feather(data_frame(time = historical_surv_lewis$time
                         ,surv = historical_surv_lewis$surv)
              ,"S:/Data Portal/legal_rep_planning/historical_surv_lewis.feather")


# Stock and Flow Data

dat_stock_and_flow <- DBI::dbGetQuery(conn = con
,"with calendar_dates as
(
select 
	cd.*
	,iif(i = 1, 'Lewis', 'Grant') tx_county 
from calendar_dim cd, dbo.NumbersTable(1, 2, 1) cty
where cd.calendar_date between '2000-01-01' and '2018-04-27'
), placements as
(
select 
	rp.removal_dt
	,id_removal_episode_fact
	,iif(rp.discharge_dt = '9999-12-31', '2018-04-27', rp.discharge_dt) discharge_dt
	,iif(rp.discharge_dt = '9999-12-31', null, rp.discharge_dt) discharge_dt_null
  ,rp.tx_county 
from ##court_episodes rp
  where rp.tx_county in ('Grant', 'Lewis')
), inflow_counts as
(
select distinct 
  calendar_date 
  ,cd.tx_county 
  ,count(distinct id_removal_episode_fact) inflow_of_placements
from calendar_dates cd
  left join placements p
    on cd.calendar_date = removal_dt
		and cd.tx_county = p.tx_county 
group by 
  calendar_date 
  ,cd.tx_county 
), outflow_counts as
(
select distinct 
  calendar_date
  ,cd.tx_county 
  ,count(distinct id_removal_episode_fact) outflow_of_placements
from calendar_dates cd
  left join placements p
    on cd.calendar_date = discharge_dt_null
		and cd.tx_county = p.tx_county 
group by 
  calendar_date 
  ,cd.tx_county 
), daily_counts as
(
select distinct 
  ic.calendar_date
  ,ic.tx_county
  ,isnull(ic.inflow_of_placements, 0) total_inflow_of_placements
  ,isnull(oc.outflow_of_placements, 0) total_outflow_of_placements
from inflow_counts ic
  left join outflow_counts oc
    on ic.calendar_date = oc.calendar_date 
      and ic.tx_county = oc.tx_county
)

select 
	sum(dc.total_inflow_of_placements) total_inflow_of_placements
	,sum(dc.total_outflow_of_placements) total_outflow_of_placements 
	,cd.month
	,cd.tx_county 
from calendar_dates cd
	join daily_counts dc 
		on cd.calendar_date = dc.calendar_date 
			and cd.tx_county = dc.tx_county
where cd.month < '2018-04-01'
group by 
	cd.month
	,cd.tx_county")


write_feather(dat_stock_and_flow
              ,"S:/Data Portal/legal_rep_planning/dat_stock_and_flow.feather")


# Grant Entries Forecast

ts_entries_grant <- filter(dat_stock_and_flow, tx_county == "Grant") %>%
  .$total_inflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_entries_grant <- stl(na.omit(ts_entries_grant)
                            ,s.window="periodic")

deseasonal_entries_grant <- seasadj(decomp_entries_grant)

deseasonal_entries_grant_fit <- auto.arima(deseasonal_entries_grant, seasonal=FALSE)

tsdisplay(residuals(deseasonal_entries_grant_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_entries_grant_fit2 <- arima(deseasonal_entries_grant, order=c(1,1,27))

tsdisplay(residuals(deseasonal_entries_grant_fit2), lag.max=45, main='(1,1,27) Model Residuals')

deseasonal_entries_grant_fcast <- forecast(deseasonal_entries_grant_fit2, h=63)

# Grant Exits Forecast

ts_exits_grant <- filter(dat_stock_and_flow, tx_county == "Grant") %>%
  .$total_outflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_exits_grant <- stl(na.omit(ts_exits_grant), s.window="periodic")

deseasonal_exits_grant <- seasadj(decomp_exits_grant)

deseasonal_exits_grant_fit <- auto.arima(deseasonal_exits_grant, seasonal=FALSE)

tsdisplay(residuals(deseasonal_exits_grant_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_exits_grant_fit2 <- arima(deseasonal_exits_grant, order=c(1,1,21))

tsdisplay(residuals(deseasonal_exits_grant_fit2), lag.max=45, main='(1,1,21) Model Residuals')

deseasonal_exits_grant_fcast <- forecast(deseasonal_exits_grant_fit2, h=63)

# Lewis Entries Forecast

ts_entries_lewis <- filter(dat_stock_and_flow, tx_county == "Lewis") %>%
  .$total_inflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_entries_lewis <- stl(na.omit(ts_entries_lewis), s.window="periodic")

deseasonal_entries_lewis <- seasadj(decomp_entries_lewis)

deseasonal_entries_lewis_fit <- auto.arima(deseasonal_entries_lewis, seasonal=FALSE)

tsdisplay(residuals(deseasonal_entries_lewis_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_entries_lewis_fit2 <- arima(deseasonal_entries_lewis, order=c(1,1,19))

tsdisplay(residuals(deseasonal_entries_lewis_fit2), lag.max=45, main='(1,1,19) Model Residuals')

deseasonal_entries_lewis_fcast <- forecast(deseasonal_entries_lewis_fit2, h=63)

# Lewis Exits Forecast

ts_exits_lewis <- filter(dat_stock_and_flow, tx_county == "Lewis") %>%
  .$total_outflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_exits_lewis <- stl(na.omit(ts_exits_lewis), s.window="periodic")

deseasonal_exits_lewis <- seasadj(decomp_exits_lewis)

deseasonal_exits_lewis_fit <- auto.arima(deseasonal_exits_lewis, seasonal=FALSE)

tsdisplay(residuals(deseasonal_exits_lewis_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_exits_lewis_fit2 <- arima(deseasonal_exits_lewis, order=c(1,1,36))

tsdisplay(residuals(deseasonal_exits_lewis_fit2), lag.max=45, main='(1,1,36) Model Residuals')

deseasonal_exits_lewis_fcast <- forecast(deseasonal_exits_lewis_fit2, h=63)


write_feather(as_data_frame(deseasonal_exits_lewis_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_exits_lewis_fcast.feather")
write_feather(as_data_frame(deseasonal_entries_lewis_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_entries_lewis_fcast.feather")


write_feather(as_data_frame(deseasonal_exits_grant_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_exits_grant_fcast.feather")
write_feather(as_data_frame(deseasonal_entries_grant_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_entries_grant_fcast.feather")


