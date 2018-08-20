library(ggplot2)
library(feather)
library(tidyr)
library(lubridate)
library(stringi)
library(stringr)
library(scales)
source("helpers.R")

function(input, output, session) {
  
  getParams <- function(prefix) {
    input[[paste0(prefix, "_recalc")]]
    
    params <- lapply(paramNames, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
    names(params) <- paramNames
    params
  }
  
  # Function that generates scenarios and computes projections. The expression
  # is wrapped in a call to reactive to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically
  #     re-executed when inputs change
  #
  proj_grant <- reactive(do.call(build_point_estimates_grant, getParams("Grant")))
  proj_lewis <- reactive(do.call(build_point_estimates_lewis, getParams("Lewis")))
  
  update_grant_costs <- reactive({
    proj_grant() %>% 
      .$cumsum_atty_cost %>% 
      max
  })
  
  update_grant_cost_date <- reactive({
    filter(proj_grant()
           ,cumsum_atty_cost == update_grant_costs()) %>% 
      .$month_name %>% 
      min
  })
  
  update_lewis_costs <- reactive({
    proj_lewis() %>% 
      .$cumsum_atty_cost %>% 
      max
  })
  
  update_lewis_cost_date <- reactive({
    filter(proj_lewis()
           ,cumsum_atty_cost == update_lewis_costs()) %>% 
      .$month_name %>% 
      min
  })
  
  costs_as_of_month_end <- reactive({
    proj_lewis() %>% 
      filter(month_name == next_month_name) %>%
    .$cumsum_atty_cost +
    proj_grant() %>% 
      filter(month_name == next_month_name) %>%
      .$cumsum_atty_cost 
      
  })
  
  
  metric_names <- list(
    "attorneys_needed"="Attorney Need",
    "cumsum_atty_cost"="Cummulative Costs (M$)",
    "study_stock" = "Caseload"
  )
  
  metric_labeller <- function(variable,value){
    return(metric_names[value])
  }
  
  update_grant_graphs <- reactive({
    proj_grant() %>% 
      mutate(cumsum_atty_cost = cumsum_atty_cost/1000000) %>%
      select(month_name, study_stock, attorneys_needed, cumsum_atty_cost) %>%
      gather(key = measure, value = value, -month_name) %>%
      ggplot(aes(x = month_name, y = value, colour = measure)) +
      geom_line() + 
      facet_grid(measure ~ ., scales = "free_y", labeller = metric_labeller) + 
      ylab("") + 
      xlab("") + 
      ggtitle("Grant Projections") + 
      theme_bw() + 
      theme(legend.position="none")
  })
  
  update_lewis_graphs <- reactive({
    proj_lewis() %>% 
      mutate(cumsum_atty_cost = cumsum_atty_cost/1000000) %>%
      select(month_name, study_stock, attorneys_needed, cumsum_atty_cost) %>%
      gather(key = measure, value = value, -month_name) %>%
      ggplot(aes(x = month_name, y = value, colour = measure)) +
      geom_line() + 
      facet_grid(measure ~ ., scales = "free_y", labeller = metric_labeller) + 
      ylab("") + 
      xlab("") + 
      ggtitle("Lewis Projections") + 
      theme_bw() + 
      theme(legend.position="none")
  })
  
  output$overall_text <- renderText({
    
    total_current_budget <- 648000*2
    total_estimated_costs <- update_grant_costs() + update_lewis_costs()
    delta_budget_cost <- total_estimated_costs - total_current_budget
    
    paste0("The total budget of the pilot is currently <b>"
           ,scales::dollar(total_current_budget)
           ,"</b>; <b>$648,000</b> for each half of the current biennium."
           ," By the end of the current month, we estimate that as much as <b>"
           ,scales::dollar(costs_as_of_month_end())
           ,"</b> will have been expended. "
           ,"<p><p>"
           ,"With the assuptions below, we estimate the total cost of the pilot "
           ,"to be <b>" 
           ,scales::dollar(total_estimated_costs)
           ,"</b>, not including the costs of administration or evaluation." 
           ,"<p><p>"
           ," In order to complete the pilot under these assumptions, "
           ,"an additional <b>"
           ,scales::dollar(ifelse(delta_budget_cost<0, 0, delta_budget_cost))
           ,"</b> will likely be required in order to maintain legal representation "
           ,"for the duration of all dependencies inlcuded in the pilot until the date(s)"
           ," chosen below."
           ,"<p><p>"
           ,"The assumptions for the pilot can be adjusted for each county below. "
           ,"Once and assumption changes, the values and graphs in this application "
           ,"will be automatically updated.")
    
  })
  
  output$grant_text <- renderText({
    
    
    paste0("In Grant county, based on the historical timing of permanency outcomes, "
           ,"we estimate that it will take <b>"
           ,median_grant
           ," days </b>for <b>50%</b> of the pilot cases to exit to permanency, and <b>"
           ,scales::comma(ninetile_grant)
           ," days </b>for <b>90%</b> of the pilot cases to exit. Thus, in order to maintain "
           ," legal representation for the entire duration of dependencies in the pilot "
           ," locations, we need to plan to provide for such representation beyond "
           ," a given biennium. "
           ,"<p><p>"
           ,"If the Grant pilot stops taking cases in <b>"
           ,lubridate::month(input$Grant_stop_date
                             ,label = TRUE
                             ,abbr = FALSE)
           ," of "
           ,lubridate::year(input$Grant_stop_date)   
           ,"</b>, we estimate that at least some children served"
           ," in the pilot will remain in care (and thus require an attorney) through <b>"
           ,lubridate::month(update_grant_cost_date()
                             ,label = TRUE
                             ,abbr = FALSE)
           ," of "
           ,lubridate::year(update_grant_cost_date())
           ,"</b>. "
           ,"The estimated total cost for the Grant portion of the pilot"
           ," with these parameters is <b>"
           ,scales::dollar(update_grant_costs())
           ,"</b>.")
    
  })
  output$lewis_text <- renderText({
    
    paste0("In Lewis county, based on the historical timing of permanency outcomes, "
           ,"we estimate that it will take <b>"
           ,median_lewis
           ," days </b>for <b>50%</b> of the pilot cases to exit to permanency, and <b>"
           ,scales::comma(ninetile_lewis)
           ," days </b>for <b>90%</b> of the pilot cases to exit. Thus, in order to maintain "
           ," legal representation for the entire duration of dependencies in the pilot "
           ," locations, we need to plan to provide for such representation beyond "
           ," a given biennium. "
           ,"<p><p>"
           ,"If the Lewis pilot stops taking cases in <b>"
           ,lubridate::month(input$Lewis_stop_date
                             ,label = TRUE
                             ,abbr = FALSE)
           ," of "
           ,lubridate::year(input$Lewis_stop_date)   
           ,"</b>, we estimate that at least some children served"
           ," in the pilot will remain in care (and thus require an attorney) through <b>"
           ,lubridate::month(update_lewis_cost_date()
                             ,label = TRUE
                             ,abbr = FALSE)
           ," of "
           ,lubridate::year(update_lewis_cost_date())
           ,"</b>. "
           ,"The estimated total cost for the Lewis portion of the pilot"
           ," with these parameters is <b>"
           ,scales::dollar(update_lewis_costs())
           ,"</b>.")
    
  })
  
  output$grant_plot <- renderPlot({
    update_grant_graphs()
  })
  
  output$lewis_plot <- renderPlot({
    update_lewis_graphs()
  })
  
}