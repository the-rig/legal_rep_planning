
renderInputs <- function(prefix) {
  wellPanel(tags$h3(paste0(prefix, " County Assumptions")),
    fluidRow(
      column(12,
             airDatepickerInput(inputId = paste0(prefix, "_", "stop_date")
                                ,label = "Month of last New Pilot Case : "
                                ,value = lubridate::today()
                                ,dateFormat = "yyyy-mm-01"
                                ,minDate = lubridate::today()
                                ,maxDate = "2023-06-01"
                                ,view = "months"
                                ,minView = "months"),
             sliderInput(inputId = paste0(prefix, "_", "caseload")
                         ,"Maximum caseload per attorney :"
                         ,min = 1
                         ,max = 300
                         ,value = 60
                         ,step = 1),
             sliderInput(inputId = paste0(prefix, "_", "salary")
                         ,"Annual salary per attorney :"
                         ,min = 100000
                         ,max = 200000
                         ,value = 120000
                         ,step = 10000
                         ,pre = "$"
                         ,sep = ",")           
      )
    )#,
    # p(actionButton(paste0(prefix, "_", "recalc"),
    #                "Re-run simulation", icon("random")
    # ))
  )
}

# Define UI for application that plots projections
fluidPage(theme="simplex.min.css",
          tags$style(type="text/css",
                     "label {font-size: 12px;}",
                     ".recalculating {opacity: 1.0;}"
          ),
          
          # Application title
          tags$h2("When to stop the pilot?"),
          p("Internal planning application - Please do not distribute"),
          hr(),
          p(htmlOutput("overall_text")),
          hr(),
          
          fluidRow(
            column(6, renderInputs("Grant")),
            column(6, renderInputs("Lewis"))
          ),
          fluidRow(
            column(6,
                   plotOutput("grant_plot")
                   ,htmlOutput("grant_text")
                   ,p("")
                   ,p("")      
                   ,hr()                 

            ),
            column(6,
                   plotOutput("lewis_plot")
                   ,htmlOutput("lewis_text")
                   ,p("")
                   ,p("")    
                   ,hr()                   
            )
          )
)