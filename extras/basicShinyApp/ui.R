library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel("Evidence Explorer"),
            tags$head(tags$style(type = "text/css", "
                                 #loadmessage {
                                 position: fixed;
                                 top: 0px;
                                 left: 0px;
                                 width: 100%;
                                 padding: 5px 0px 5px 0px;
                                 text-align: center;
                                 font-weight: bold;
                                 font-size: 100%;
                                 color: #000000;
                                 background-color: #ADD8E6;
                                 z-index: 105;
                                 }
                                 ")),
            conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                             tags$div("Processing...",id = "loadmessage")),
            fluidRow(
              column(3,
                     selectInput("exposuresOutcome", "Exposures-outcome", exposuresOutcomeNames$name),
                     checkboxGroupInput("database", "Data source", databases$cdmSourceAbbreviation, selected = databases$cdmSourceAbbreviation),
                     checkboxGroupInput("analysis", "Analysis", sccsAnalyses$description,  selected = sccsAnalyses$description)
              ),
              column(9,
                     dataTableOutput("mainTable"),
                     conditionalPanel("output.rowIsSelected == true",
                                      tabsetPanel(id = "detailsTabsetPanel",
                                                  tabPanel("Power",
                                                           div(strong("Table 1."), "For each variable of interest: the number of cases (people with at least one outcome), the number of years those people were observed, the number of outcomes, the number of subjects with at least one exposure, the number of patient-years exposed, the number of outcomes while exposed, and the minimum detectable relative risk (MDRR)."),
                                                           tableOutput("powerTable")
                                                  ),
                                                  tabPanel("Model",
                                                           div(strong("Table 2."), "The fitted non-zero coefficent (incidence rate ratio) and 95 percent confidence interval for all variables in the model."),
                                                           tableOutput("modelTable")
                                                  ),
                                                  tabPanel("Time trend",
                                                           plotOutput("timeTrendPlot", height = 600),
                                                           div(strong("Figure 1."), "Per calendar month the number of people observed, the unadjusted rate of the outcome, and the rate of the outcome after adjusting for age, season, and calendar time, if specified in the model. Red indicates months where the adjusted rate was significantly different from the mean adjusted rate."),
                                                  ),
                                                  tabPanel("Time to event",
                                                           plotOutput("timeToEventPlot"),
                                                           div(strong("Figure 2."), "The number of events and subjects observed per week relative to the start of the first exposure (indicated by the thick vertical line)."),
                                                  )
                                      )
                     )
              )
            )
  )
)
