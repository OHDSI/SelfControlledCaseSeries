library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel("Evidence Explorer"),
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
                                                           uiOutput("powerTableCaption"),
                                                           tableOutput("powerTable"),
                                                           uiOutput("timeAtRiskTableCaption"),
                                                           tableOutput("timeAtRiskTable")
                                                  )
                                      )
                     )
              )
            )
  )
)
