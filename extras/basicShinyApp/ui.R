library(shiny)
library(DT)

shinyUI(
  fluidPage(style = "width:1500px;",
            titlePanel("SCCS Evidence Explorer"),
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
                                                  tabPanel("Attrition",
                                                           plotOutput("attritionPlot", width = 600, height = 500),
                                                           div(strong("Figure 1."), "Attrition, showing the number of cases (number of subjects with at least one outcome), and number of outcomes (number of ocurrences of the outcome) after each step in the study.")
                                                  ),
                                                  tabPanel("Model",
                                                           tabsetPanel(id = "modelTabsetPanel",
                                                                       tabPanel("Model coefficients",
                                                                                div(strong("Table 2."), "The fitted non-zero coefficent (incidence rate ratio) and 95 percent confidence interval for all variables in the model."),
                                                                                tableOutput("modelTable")
                                                                       ),
                                                                       tabPanel("Age spline",
                                                                                plotOutput("ageSplinePlot"),
                                                                                div(strong("Figure 2a."), "Spline fitted for age.")
                                                                       ),
                                                                       tabPanel("Season spline",
                                                                                plotOutput("seasonSplinePlot"),
                                                                                div(strong("Figure 2b."), "Spline fitted for season")
                                                                       ),
                                                                       tabPanel("Calendar time spline",
                                                                                plotOutput("calendarTimeSplinePlot"),
                                                                                div(strong("Figure 2c."), "Spline fitted for calendar time")
                                                                       )
                                                           )
                                                  ),
                                                  tabPanel("Spanning",
                                                           radioButtons("spanningType", label = "Type:", choices = c("Age", "Calendar time")),
                                                           plotOutput("spanningPlot"),
                                                           div(strong("Figure 3."), "Number of subjects observed for 3 consecutive months, centered on the indicated month.")
                                                  ),
                                                  tabPanel("Time trend",
                                                           plotOutput("timeTrendPlot", height = 600),
                                                           div(strong("Figure 4."), "Per calendar month the number of people observed, the unadjusted rate of the outcome, and the rate of the outcome after adjusting for age, season, and calendar time, if specified in the model. Red indicates months where the adjusted rate was significantly different from the mean adjusted rate.")
                                                  ),
                                                  tabPanel("Time to event",
                                                           plotOutput("timeToEventPlot"),
                                                           div(strong("Figure 5."), "The number of events and subjects observed per week relative to the start of the first exposure (indicated by the thick vertical line).")
                                                  ),
                                                  tabPanel("Event dep. observation",
                                                           plotOutput("eventDepObservationPlot"),
                                                           div(strong("Figure 6."), "Histograms for the number of months between the first occurrence of the outcome and the end of observation, stratified by whether the end of observation was censored (inferred as not being equal to the end of database time), or uncensored (inferred as having the subject still be observed at the end of database time).")
                                                  ),
                                                  tabPanel("Systematic error",
                                                           plotOutput("controlEstimatesPlot"),
                                                           div(strong("Figure 7."),"Systematic error. Effect size estimates for the negative controls (true incidence rate ratio = 1)
                                                                                    and positive controls (true incidence rate ratio > 1), before and after calibration. Estimates below the diagonal dashed
                                                                                    lines are statistically significant (alpha = 0.05) different from the true effect size. A well-calibrated
                                                                                    estimator should have the true effect size within the 95 percent confidence interval 95 percent of times.")
                                                  ),
                                                  tabPanel("Diagnostics summary",
                                                           tableOutput("diagnosticsSummary")

                                                  )
                                      )
                     )
              )
            )
  )
)
