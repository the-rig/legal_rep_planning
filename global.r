library(shiny)
library(htmltools)
library(shinyWidgets)
library(feather)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(stringi)
library(stringr)
library(scales)

paramNames <- c("salary", "caseload", "stop_date")

feather_file_list_full <- list.files(pattern = ".*feather"
                                     ,full.names = TRUE)

feather_file_list_part <- list.files(pattern = ".*feather"
                                     ,full.names = FALSE)

all_data <- lapply(feather_file_list_full
                   ,read_feather)

names(all_data) <- feather_file_list_part

median_lewis_loc <- which.min(abs(.5-all_data$historical_surv_lewis.feather$surv))

median_grant_loc <- which.min(abs(.5-all_data$historical_surv_grant.feather$surv))

median_lewis <- all_data$historical_surv_lewis.feather$time[median_lewis_loc]

median_grant <- all_data$historical_surv_grant.feather$time[median_grant_loc]

ninetile_lewis_loc <- which.min(abs(.1-all_data$historical_surv_lewis.feather$surv))

ninetile_grant_loc <- which.min(abs(.1-all_data$historical_surv_grant.feather$surv))

ninetile_lewis <- all_data$historical_surv_lewis.feather$time[ninetile_lewis_loc]

ninetile_grant <- all_data$historical_surv_grant.feather$time[ninetile_grant_loc]

next_month_name <- ymd(paste0(year(today())
           ,str_pad(string = month(today())
                    ,width = 2
                    ,side = "left"
                    ,pad = "0")
           ,"01"))

#vector of months 
months_into_pilot <- 1:70

#vector of month names all
month_name <- seq(lubridate::ymd(20170901), by = "month", length.out = 70)

#vector of month names for predictions
month_name_pred <- seq(from = as.Date("2018/4/1"), to = as.Date("2023/6/1"), by = "month")




#survival time vector search function
vector_search <- function(n, times){
  which.min(abs(n-times/30))
}

build_point_estimates_lewis <- function(stop_date = 20180901, caseload = 60, salary = 120000) {
  
  historical_survival_locations <- as.numeric(lapply(X = 1:70
                                                     ,FUN = vector_search
                                                     ,times = all_data %>%
                                                       .$historical_surv_lewis.feather %>%
                                                       .$time)
  )
  
  historical_survival_all <- all_data %>%
    .$historical_surv_lewis.feather %>%
    .$surv
  
  historical_survival <- historical_survival_all[historical_survival_locations]
  
  entries_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                           ,tx_county_mod == "Lewis"
                           ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_inflow_of_placements)
  
  exits_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                         ,tx_county_mod == "Lewis"
                         ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_outflow_of_placements)
  
  #vector of all exits, predicted and actual
  exits <- bind_rows(
    exits_actual
    ,as_data_frame(all_data$deseasonal_exits_lewis_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_outflow_of_placements = `Point Forecast`) %>%
      select(month, total_outflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_outflow_of_placements
  
  #vector of all entries, predicted and actual, as filtered by stop-date
  entries <- bind_rows(
    entries_actual
    ,as_data_frame(all_data$deseasonal_entries_lewis_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_inflow_of_placements = `Point Forecast`
             ,total_inflow_of_placements = ifelse(month > lubridate::ymd(stop_date)
                                                  ,0
                                                  ,total_inflow_of_placements)) %>%
      select(month, total_inflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_inflow_of_placements
  
  expected_study_exits <- (1-historical_survival)*exits
  
  data_frame(months_into_pilot
             ,month_name
             ,historical_survival
             ,exits 
             ,entries
             ,expected_study_exits)  %>%
    mutate(entries = ifelse(is.na(entries), 0, entries)
           ,entries_minus_exits = entries - expected_study_exits
           ,study_stock = ifelse(cumsum(entries_minus_exits) < 0
                                 ,0
                                 ,cumsum(entries_minus_exits))
           ,attorneys_needed = ifelse(month_name < lubridate::today()
                                      ,ceiling(study_stock/60)
                                      ,ceiling(study_stock/caseload)
           )
           ,attorney_cost = ifelse(month_name < lubridate::today()
                                   ,attorneys_needed*(120000/12)
                                   ,attorneys_needed*(salary/12)
           )
           ,cumsum_atty_cost = cumsum(attorney_cost)
    )
  
}


build_point_estimates_grant <- function(stop_date = 20180901, caseload = 60, salary = 120000) {
  
  historical_survival_locations <- as.numeric(lapply(X = 1:70
                                                     ,FUN = vector_search
                                                     ,times = all_data %>%
                                                       .$historical_surv_grant.feather %>%
                                                       .$time)
  )
  
  historical_survival_all <- all_data %>%
    .$historical_surv_grant.feather %>%
    .$surv
  
  historical_survival <- historical_survival_all[historical_survival_locations]
  
  entries_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                           ,tx_county_mod == "Grant"
                           ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_inflow_of_placements)
  
  exits_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                         ,tx_county_mod == "Grant"
                         ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_outflow_of_placements)
  
  #vector of all exits, predicted and actual
  exits <- bind_rows(
    exits_actual
    ,as_data_frame(all_data$deseasonal_exits_grant_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_outflow_of_placements = `Point Forecast`) %>%
      select(month, total_outflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_outflow_of_placements
  
  #vector of all entries, predicted and actual, as filtered by stop-date
  entries <- bind_rows(
    entries_actual
    ,as_data_frame(all_data$deseasonal_entries_grant_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_inflow_of_placements = `Point Forecast`
             ,total_inflow_of_placements = ifelse(month > lubridate::ymd(stop_date)
                                                  ,0
                                                  ,total_inflow_of_placements)) %>%
      select(month, total_inflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_inflow_of_placements
  
  expected_study_exits <- (1-historical_survival)*exits
  
  data_frame(months_into_pilot
             ,month_name
             ,historical_survival
             ,exits 
             ,entries
             ,expected_study_exits)  %>%
    mutate(entries = ifelse(is.na(entries), 0, entries)
           ,entries_minus_exits = entries - expected_study_exits
           ,study_stock = ifelse(cumsum(entries_minus_exits) < 0
                                 ,0
                                 ,cumsum(entries_minus_exits))
           ,attorneys_needed = ifelse(month_name < lubridate::today()
                                      ,ceiling(study_stock/60)
                                      ,ceiling(study_stock/caseload)
           )
           ,attorney_cost = ifelse(month_name < lubridate::today()
                                   ,attorneys_needed*(120000/12)
                                   ,attorneys_needed*(salary/12)
           )
           ,cumsum_atty_cost = cumsum(attorney_cost)
    )
  
}


build_point_estimates_douglas <- function(stop_date = 20180901, caseload = 60, salary = 120000) {
  
  historical_survival_locations <- as.numeric(lapply(X = 1:70
                                                     ,FUN = vector_search
                                                     ,times = all_data %>%
                                                       .$historical_surv_douglas.feather %>%
                                                       .$time)
  )
  
  historical_survival_all <- all_data %>%
    .$historical_surv_douglas.feather %>%
    .$surv
  
  historical_survival <- historical_survival_all[historical_survival_locations]
  
  entries_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                                  ,tx_county_mod == "Grant"
                                  ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_inflow_of_placements)
  
  exits_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                                ,tx_county_mod == "Grant"
                                ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_outflow_of_placements)
  
  #vector of all exits, predicted and actual
  exits <- bind_rows(
    exits_actual
    ,as_data_frame(all_data$deseasonal_exits_douglas_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_outflow_of_placements = `Point Forecast`) %>%
      select(month, total_outflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_outflow_of_placements
  
  #vector of all entries, predicted and actual, as filtered by stop-date
  entries <- bind_rows(
    entries_actual
    ,as_data_frame(all_data$deseasonal_entries_douglas_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_inflow_of_placements = `Point Forecast`
             ,total_inflow_of_placements = ifelse(month > lubridate::ymd(stop_date)
                                                  ,0
                                                  ,total_inflow_of_placements)) %>%
      select(month, total_inflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_inflow_of_placements
  
  expected_study_exits <- (1-historical_survival)*exits
  
  data_frame(months_into_pilot
             ,month_name
             ,historical_survival
             ,exits 
             ,entries
             ,expected_study_exits)  %>%
    mutate(entries = ifelse(is.na(entries), 0, entries)
           ,entries_minus_exits = entries - expected_study_exits
           ,study_stock = ifelse(cumsum(entries_minus_exits) < 0
                                 ,0
                                 ,cumsum(entries_minus_exits))
           ,attorneys_needed = ifelse(month_name < lubridate::today()
                                      ,ceiling(study_stock/60)
                                      ,ceiling(study_stock/caseload)
           )
           ,attorney_cost = ifelse(month_name < lubridate::today()
                                   ,attorneys_needed*(120000/12)
                                   ,attorneys_needed*(salary/12)
           )
           ,cumsum_atty_cost = cumsum(attorney_cost)
    )
  
}

build_point_estimates_whatcom <- function(stop_date = 20180901, caseload = 60, salary = 120000) {
  
  historical_survival_locations <- as.numeric(lapply(X = 1:70
                                                     ,FUN = vector_search
                                                     ,times = all_data %>%
                                                       .$historical_surv_whatcom.feather %>%
                                                       .$time)
  )
  
  historical_survival_all <- all_data %>%
    .$historical_surv_whatcom.feather %>%
    .$surv
  
  historical_survival <- historical_survival_all[historical_survival_locations]
  
  entries_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                                  ,tx_county_mod == "Grant"
                                  ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_inflow_of_placements)
  
  exits_actual <- dplyr::filter(all_data$dat_stock_and_flow.feather
                                ,tx_county_mod == "Grant"
                                ,month >= lubridate::ymd(20170901)) %>%
    mutate(month = lubridate::as_date(month, tz = "UTC")) %>%
    select(month, total_outflow_of_placements)
  
  #vector of all exits, predicted and actual
  exits <- bind_rows(
    exits_actual
    ,as_data_frame(all_data$deseasonal_exits_whatcom_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_outflow_of_placements = `Point Forecast`) %>%
      select(month, total_outflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_outflow_of_placements
  
  #vector of all entries, predicted and actual, as filtered by stop-date
  entries <- bind_rows(
    entries_actual
    ,as_data_frame(all_data$deseasonal_entries_whatcom_fcast.feather) %>%
      mutate(month = lubridate::as_date(month_name_pred)
             ,total_inflow_of_placements = `Point Forecast`
             ,total_inflow_of_placements = ifelse(month > lubridate::ymd(stop_date)
                                                  ,0
                                                  ,total_inflow_of_placements)) %>%
      select(month, total_inflow_of_placements)
  ) %>%
    arrange(month) %>%
    .$total_inflow_of_placements
  
  expected_study_exits <- (1-historical_survival)*exits
  
  data_frame(months_into_pilot
             ,month_name
             ,historical_survival
             ,exits 
             ,entries
             ,expected_study_exits)  %>%
    mutate(entries = ifelse(is.na(entries), 0, entries)
           ,entries_minus_exits = entries - expected_study_exits
           ,study_stock = ifelse(cumsum(entries_minus_exits) < 0
                                 ,0
                                 ,cumsum(entries_minus_exits))
           ,attorneys_needed = ifelse(month_name < lubridate::today()
                                      ,ceiling(study_stock/60)
                                      ,ceiling(study_stock/caseload)
           )
           ,attorney_cost = ifelse(month_name < lubridate::today()
                                   ,attorneys_needed*(120000/12)
                                   ,attorneys_needed*(salary/12)
           )
           ,cumsum_atty_cost = cumsum(attorney_cost)
    )
  
}
