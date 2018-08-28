
# load libraries and db connection
library(odbc)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forecast)
library(feather)
library(survival) #for KM estimates
library(ggfortify) #for plots

con <- DBI::dbConnect(odbc::odbc(), "POC")

# Base Table

DBI::dbSendQuery(conn = con
,"
if object_id('tempdb..##court_episodes') is not null
    drop table ##court_episodes;

select distinct 
	rp.latest_removal_dt
,id_removal_episode_fact
,rp.child
,case
when ljd.cd_jurisdiction = 9
then 'Douglas'
else rp.tx_county 
end tx_county_mod
,rp.discharge_dt
,rp.id_case
,case
when lf_dt is not null
then lf_dt
else discharge_dt
end discharge_from_pilot
into ##court_episodes
from base.rptPlacement rp
left join dbo.legal_fact lf
on lf.id_case = rp.id_case
left join dbo.legal_jurisdiction_dim ljd 
on lf.id_legal_jurisdiction_dim = ljd.id_legal_jurisdiction_dim
where (rp.tx_county in ('Grant', 'Lewis', 'Whatcom') or ljd.cd_jurisdiction = 9)
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
or fl_dep_exist = 1)
")

# Event Table

dat_historical_events <- DBI::dbGetQuery(conn = con
,"
with surv_prep as
(
select 
  rp.child id_prsn_child
,rp.latest_removal_dt
,iif(rp.discharge_from_pilot = '9999-12-31', '2018-04-27', rp.discharge_from_pilot) discharge_dt
,iif(rp.discharge_from_pilot = '9999-12-31', null, rp.discharge_from_pilot) discharge_dt_null
,iif(rp.discharge_from_pilot = '9999-12-31', 0, 1) fl_discharge
,datediff(dd, rp.latest_removal_dt, 
iif(rp.discharge_dt = '9999-12-31', '2018-04-27', rp.discharge_from_pilot)) + 1 los
,rp.id_case
,rp.tx_county_mod 
from ##court_episodes rp
where rp.latest_removal_dt between '2010-01-01' and '2018-04-27'
)
select * from surv_prep where los >= 1

")

# KM Estimators


historical_surv_whatcom <- survival::survfit(Surv(los, fl_discharge) ~ 1
                                           ,data = filter(dat_historical_events, tx_county_mod == "Whatcom")) 


historical_surv_douglas <- survival::survfit(Surv(los, fl_discharge) ~ 1
                                           ,data = filter(dat_historical_events, tx_county_mod == "Douglas")) 

historical_surv_grant <- survival::survfit(Surv(los, fl_discharge) ~ 1
                  ,data = filter(dat_historical_events, tx_county_mod == "Grant")) 


historical_surv_lewis <- survival::survfit(Surv(los, fl_discharge) ~ 1
                  ,data = filter(dat_historical_events, tx_county_mod == "Lewis")) 

write_feather(data_frame(time = historical_surv_grant$time
                         ,surv = historical_surv_grant$surv)
              ,"S:/Data Portal/legal_rep_planning/historical_surv_grant.feather")

write_feather(data_frame(time = historical_surv_lewis$time
                         ,surv = historical_surv_lewis$surv)
              ,"S:/Data Portal/legal_rep_planning/historical_surv_lewis.feather")

write_feather(data_frame(time = historical_surv_whatcom$time
                         ,surv = historical_surv_whatcom$surv)
              ,"S:/Data Portal/legal_rep_planning/historical_surv_whatcom.feather")

write_feather(data_frame(time = historical_surv_douglas$time
                         ,surv = historical_surv_douglas$surv)
              ,"S:/Data Portal/legal_rep_planning/historical_surv_douglas.feather")


# Stock and Flow Data

dat_stock_and_flow <- DBI::dbGetQuery(conn = con
,"with calendar_dates as
(
  select 
  cd.*
  ,case 
  when cty.i = 1
  then 'Lewis'
  when cty.i = 2
  then 'Grant'
  when cty.i = 3
  then 'Douglas'
  when cty.i = 4
  then 'Whatcom'
  else null 
  end tx_county_mod
  from calendar_dim cd, dbo.NumbersTable(1, 4, 1) cty
  where cd.calendar_date between '2000-01-01' and '2018-04-27'
), placements as
  (
  select 
  rp.latest_removal_dt
  ,id_removal_episode_fact
  ,iif(rp.discharge_from_pilot = '9999-12-31'
  , '2018-04-27', rp.discharge_from_pilot) discharge_dt
  ,iif(rp.discharge_from_pilot = '9999-12-31'
  , null, rp.discharge_from_pilot) discharge_dt_null
  ,rp.tx_county_mod 
  from ##court_episodes rp
  ), inflow_counts as
  (
  select distinct 
  calendar_date 
  ,p.tx_county_mod 
  ,count(distinct id_removal_episode_fact) inflow_of_placements
  from calendar_dates cd
  left join placements p
  on cd.calendar_date = latest_removal_dt
  and cd.tx_county_mod = p.tx_county_mod 
  group by 
  calendar_date 
  ,p.tx_county_mod
  ), outflow_counts as
  (
  select distinct 
  calendar_date
  ,p.tx_county_mod 
  ,count(distinct id_removal_episode_fact) outflow_of_placements
  from calendar_dates cd
  left join placements p
  on cd.calendar_date = discharge_dt_null
  and cd.tx_county_mod = p.tx_county_mod 
  group by 
  calendar_date 
  ,p.tx_county_mod 
  ), daily_counts as
  (
  select distinct 
  ic.calendar_date
  ,ic.tx_county_mod
  ,isnull(ic.inflow_of_placements, 0) total_inflow_of_placements
  ,isnull(oc.outflow_of_placements, 0) total_outflow_of_placements
  from inflow_counts ic
  left join outflow_counts oc
  on ic.calendar_date = oc.calendar_date 
  and ic.tx_county_mod = oc.tx_county_mod
  )
  
  select 
  sum(dc.total_inflow_of_placements) total_inflow_of_placements
  ,sum(dc.total_outflow_of_placements) total_outflow_of_placements 
  ,cd.month
  ,cd.tx_county_mod
  from calendar_dates cd
  join daily_counts dc 
  on cd.calendar_date = dc.calendar_date 
  and cd.tx_county_mod = dc.tx_county_mod
  where cd.month < '2018-04-01'
  group by 
  cd.month
  ,cd.tx_county_mod")


write_feather(dat_stock_and_flow
              ,"S:/Data Portal/legal_rep_planning/dat_stock_and_flow.feather")


# Grant Entries Forecast

ts_entries_grant <- filter(dat_stock_and_flow, tx_county_mod == "Grant") %>%
  .$total_inflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_entries_grant <- stl(na.omit(ts_entries_grant)
                            ,s.window="periodic")

deseasonal_entries_grant <- seasadj(decomp_entries_grant)

deseasonal_entries_grant_fit <- auto.arima(deseasonal_entries_grant, seasonal=FALSE)

tsdisplay(residuals(deseasonal_entries_grant_fit), lag.max=45, main='(1,1,1) Model Residuals')

#fdeseasonal_entries_grant_fit2 <- arima(deseasonal_entries_grant, order=c(1,1,43))

#tsdisplay(residuals(deseasonal_entries_grant_fit2), lag.max=45, main='(1,1,43) Model Residuals')

deseasonal_entries_grant_fcast <- forecast(deseasonal_entries_grant_fit, h=63)

# Grant Exits Forecast

ts_exits_grant <- filter(dat_stock_and_flow, tx_county_mod == "Grant") %>%
  .$total_outflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_exits_grant <- stl(na.omit(ts_exits_grant), s.window="periodic")

deseasonal_exits_grant <- seasadj(decomp_exits_grant)

deseasonal_exits_grant_fit <- auto.arima(deseasonal_exits_grant, seasonal=FALSE)

tsdisplay(residuals(deseasonal_exits_grant_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_exits_grant_fcast <- forecast(deseasonal_exits_grant_fit, h=63)

# Lewis Entries Forecast

ts_entries_lewis <- filter(dat_stock_and_flow, tx_county_mod == "Lewis") %>%
  .$total_inflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_entries_lewis <- stl(na.omit(ts_entries_lewis), s.window="periodic")

deseasonal_entries_lewis <- seasadj(decomp_entries_lewis)

deseasonal_entries_lewis_fit <- auto.arima(deseasonal_entries_lewis, seasonal=FALSE)

tsdisplay(residuals(deseasonal_entries_lewis_fit), lag.max=45, main='(1,1,1) Model Residuals')

#deseasonal_entries_lewis_fit2 <- arima(deseasonal_entries_lewis, order=c(1,1,19))

#tsdisplay(residuals(deseasonal_entries_lewis_fit2), lag.max=45, main='(1,1,19) Model Residuals')

deseasonal_entries_lewis_fcast <- forecast(deseasonal_entries_lewis_fit, h=63)

# Lewis Exits Forecast

ts_exits_lewis <- filter(dat_stock_and_flow, tx_county_mod == "Lewis") %>%
  .$total_outflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_exits_lewis <- stl(na.omit(ts_exits_lewis), s.window="periodic")

deseasonal_exits_lewis <- seasadj(decomp_exits_lewis)

deseasonal_exits_lewis_fit <- auto.arima(deseasonal_exits_lewis, seasonal=FALSE)

tsdisplay(residuals(deseasonal_exits_lewis_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_exits_lewis_fcast <- forecast(deseasonal_exits_lewis_fit, h=63)


# Whatcom Entries Forecast

ts_entries_whatcom <- filter(dat_stock_and_flow, tx_county_mod == "Whatcom") %>%
  .$total_inflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_entries_whatcom <- stl(na.omit(ts_entries_whatcom)
                            ,s.window="periodic")

deseasonal_entries_whatcom <- seasadj(decomp_entries_whatcom)

deseasonal_entries_whatcom_fit <- auto.arima(deseasonal_entries_whatcom, seasonal=FALSE)

tsdisplay(residuals(deseasonal_entries_whatcom_fit), lag.max=45, main='(1,1,1) Model Residuals')

#deseasonal_entries_whatcom_fit2 <- arima(deseasonal_entries_whatcom, order=c(1,1,27))

#tsdisplay(residuals(deseasonal_entries_whatcom_fit2), lag.max=45, main='(1,1,27) Model Residuals')

deseasonal_entries_whatcom_fcast <- forecast(deseasonal_entries_whatcom_fit, h=63)

# Whatcom Exits Forecast

ts_exits_whatcom <- filter(dat_stock_and_flow, tx_county_mod == "Whatcom") %>%
  .$total_outflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_exits_whatcom <- stl(na.omit(ts_exits_whatcom), s.window="periodic")

deseasonal_exits_whatcom <- seasadj(decomp_exits_whatcom)

deseasonal_exits_whatcom_fit <- auto.arima(deseasonal_exits_whatcom, seasonal=FALSE)

tsdisplay(residuals(deseasonal_exits_whatcom_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_exits_whatcom_fcast <- forecast(deseasonal_exits_whatcom_fit, h=63)

# Douglas Entries Forecast

ts_entries_douglas <- filter(dat_stock_and_flow, tx_county_mod == "Douglas") %>%
  .$total_inflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_entries_douglas <- stl(na.omit(ts_entries_douglas)
                              ,s.window="periodic")

deseasonal_entries_douglas <- seasadj(decomp_entries_douglas)

deseasonal_entries_douglas_fit <- auto.arima(deseasonal_entries_douglas, seasonal=FALSE)

tsdisplay(residuals(deseasonal_entries_douglas_fit), lag.max=45, main='(1,1,1) Model Residuals')

#deseasonal_entries_douglas_fit2 <- arima(deseasonal_entries_douglas, order=c(1,1,27))

#tsdisplay(residuals(deseasonal_entries_douglas_fit2), lag.max=45, main='(1,1,27) Model Residuals')

deseasonal_entries_douglas_fcast <- forecast(deseasonal_entries_douglas_fit, h=63)

# Douglas Exits Forecast

ts_exits_douglas <- filter(dat_stock_and_flow, tx_county_mod == "Douglas") %>%
  .$total_outflow_of_placements %>%
  ts(frequency = 12, start = c(2000, 1)) %>%
  window(end=c(2018,3))

decomp_exits_douglas <- stl(na.omit(ts_exits_douglas), s.window="periodic")

deseasonal_exits_douglas <- seasadj(decomp_exits_douglas)

deseasonal_exits_douglas_fit <- auto.arima(deseasonal_exits_douglas, seasonal=FALSE)

tsdisplay(residuals(deseasonal_exits_douglas_fit), lag.max=45, main='(1,1,1) Model Residuals')

deseasonal_exits_douglas_fcast <- forecast(deseasonal_exits_douglas_fit, h=63)

write_feather(as_data_frame(deseasonal_exits_lewis_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_exits_lewis_fcast.feather")
write_feather(as_data_frame(deseasonal_entries_lewis_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_entries_lewis_fcast.feather")


write_feather(as_data_frame(deseasonal_exits_grant_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_exits_grant_fcast.feather")
write_feather(as_data_frame(deseasonal_entries_grant_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_entries_grant_fcast.feather")


write_feather(as_data_frame(deseasonal_exits_douglas_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_exits_douglas_fcast.feather")
write_feather(as_data_frame(deseasonal_entries_douglas_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_entries_douglas_fcast.feather")


write_feather(as_data_frame(deseasonal_exits_whatcom_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_exits_whatcom_fcast.feather")
write_feather(as_data_frame(deseasonal_entries_whatcom_fcast)
              ,"S:/Data Portal/legal_rep_planning/deseasonal_entries_whatcom_fcast.feather")
