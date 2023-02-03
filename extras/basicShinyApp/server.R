library(shiny)
library(DT)

onStop(function() {
  message("Closing connection pool")
  pool::poolClose(connectionPool)
})

shinyServer(function(input, output, session) {

  resultSubset <- reactive({
    exposuresOutcomeSetId <- exposuresOutcomeNames$exposuresOutcomeSetId [exposuresOutcomeNames$name == input$exposuresOutcome]
    analysisIds <- sccsAnalyses$analysisId[sccsAnalyses$description %in% input$analysis]
    databaseIds <- input$database
    if (length(analysisIds) == 0) {
      analysisIds <- -1
    }
    if (length(databaseIds) == 0) {
      databaseIds <- "none"
    }
    results <- getSccsResults(connectionPool = connectionPool,
                              resultsDatabaseSchema = resultsDatabaseSchema,
                              exposuresOutcomeSetId = exposuresOutcomeSetId,
                              databaseIds = databaseIds,
                              analysisIds = analysisIds)
    results <- results[order(results$analysisId), ]

    idx <- (results$unblind == 0)
    if (any(idx)) {
      results$rr[idx] <- rep(NA, length(idx))
      results$ci95Ub[idx] <- rep(NA, length(idx))
      results$ci95Lb[idx] <- rep(NA, length(idx))
      results$logRr[idx] <- rep(NA, length(idx))
      results$seLogRr[idx] <- rep(NA, length(idx))
      results$p[idx] <- rep(NA, length(idx))
      results$calibratedRr[idx] <- rep(NA, length(idx))
      results$calibratedCi95Ub[idx] <- rep(NA, length(idx))
      results$calibratedCi95Lb[idx] <- rep(NA, length(idx))
      results$calibratedLogRr[idx] <- rep(NA, length(idx))
      results$calibratedSeLogRr[idx] <- rep(NA, length(idx))
      results$calibratedP[idx] <- rep(NA, length(idx))
    }
    return(results)
  })

  output$mainTable <- renderDataTable({
    table <- resultSubset()
    if (is.null(table) || nrow(table) == 0) {
      return(NULL)
    }
    table$description <- sccsAnalyses$description[match(table$analysisId, sccsAnalyses$analysisId)]
    table <- table %>%
      select("description",
             "databaseId",
             "rr",
             "ci95Lb",
             "ci95Ub",
             "p",
             "calibratedRr",
             "calibratedCi95Lb",
             "calibratedCi95Ub",
             "calibratedP")

    table$rr <- prettyHr(table$rr)
    table$ci95Lb <- prettyHr(table$ci95Lb)
    table$ci95Ub <- prettyHr(table$ci95Ub)
    table$p <- prettyHr(table$p)
    table$calibratedRr <- prettyHr(table$calibratedRr)
    table$calibratedCi95Lb <- prettyHr(table$calibratedCi95Lb)
    table$calibratedCi95Ub <- prettyHr(table$calibratedCi95Ub)
    table$calibratedP <- prettyHr(table$calibratedP)
    colnames(table) <- c("<span title=\"Analysis\">Analysis</span>",
                         "<span title=\"Data source\">Data source</span>",
                         "<span title=\"Incidence rate ratio (uncalibrated)\">IRR</span>",
                         "<span title=\"Lower bound of the 95 percent confidence interval (uncalibrated)\">LB</span>",
                         "<span title=\"Upper bound of the 95 percent confidence interval (uncalibrated)\">UB</span>",
                         "<span title=\"Two-sided p-value (uncalibrated)\">P</span>",
                         "<span title=\"Incidence rate ratio (calibrated)\">Cal.IRR</span>",
                         "<span title=\"Lower bound of the 95 percent confidence interval (calibrated)\">Cal.LB</span>",
                         "<span title=\"Upper bound of the 95 percent confidence interval (calibrated)\">Cal.UB</span>",
                         "<span title=\"Two-sided p-value (calibrated)\">Cal.P</span>")
    options = list(pageLength = 15,
                   searching = FALSE,
                   lengthChange = TRUE,
                   ordering = TRUE,
                   paging = TRUE)
    selection = list(mode = "single", target = "row")
    table <- datatable(table,
                       options = options,
                       selection = selection,
                       rownames = FALSE,
                       escape = FALSE,
                       class = "stripe nowrap compact")
    return(table)
  })

  selectedRow <- reactive({
    idx <- input$mainTable_rows_selected
    if (is.null(idx)) {
      return(NULL)
    } else {
      subset <- resultSubset()
      if (nrow(subset) == 0) {
        return(NULL)
      }
      row <- subset[idx, ]
      return(row)
    }
  })

  output$rowIsSelected <- reactive({
    return(!is.null(selectedRow()))
  })
  outputOptions(output, "rowIsSelected", suspendWhenHidden = FALSE)

  output$powerTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      table <- row %>%
        mutate(outcomeEvents = ifelse(unblind == 1, outcomeEvents, NA)) %>%
        select(
          "covariateName",
          "outcomeSubjects",
          "observedDays",
          "outcomeEvents",
          "covariateSubjects",
          "covariateDays",
          "covariateOutcomes",
          "mdrr"
        ) %>%
        mutate(observedDays = observedDays / 365.25,
               covariateDays = covariateDays / 365.25)
      colnames(table) <- c("Variable",
                           "Cases",
                           "Years observed",
                           "Outcomes",
                           "Persons exposed",
                           "Years exposed",
                           "Outcomes while exposed",
                           "MDRR")
      return(table)
    }
  })

  output$attritionPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      attrition <- getAttrition(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId,
        covariateId = row$covariateId
      )
      drawAttritionDiagram(attrition)
    }
  })

  output$modelTable <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      table <- getModel(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )

      table <- table %>%
        arrange(covariateId) %>%
        select(-"covariateId")

      colnames(table) <- c("Variable",
                           "IRR",
                           "LB",
                           "UB")
      return(table)
    }
  })

  output$timeTrendPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      timeTrend <- getTimeTrend(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )
      plotTimeTrend(timeTrend)
    }
  })

  output$timeToEventPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      timeToEvent <- getTimeToEvent(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        eraId = row$eraId,
        covariateId = row$covariateId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )
      plotTimeToEvent(timeToEvent)
    }
  })

  output$eventDepObservationPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      eventDepObservation <- getEventDepObservation(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )
      plotEventDepObservation(eventDepObservation)
    }
  })

  output$spanningPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      if (input$spanningType == "Age") {
        ageSpanning <- getAgeSpanning(
          connectionPool = connectionPool,
          resultsDatabaseSchema = resultsDatabaseSchema,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        plotSpanning(ageSpanning, type = "age")
      } else {
        calendarTimeSpanning <- getCalendarTimeSpanning(
          connectionPool = connectionPool,
          resultsDatabaseSchema = resultsDatabaseSchema,
          exposuresOutcomeSetId = row$exposuresOutcomeSetId,
          databaseId = row$databaseId,
          analysisId = row$analysisId
        )
        plotSpanning(calendarTimeSpanning, type = "calendar time")
      }
    }
  })

  output$ageSplinePlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      ageSpline <- getSpline(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId,
        splineType = "age"
      )
      if (nrow(ageSpline) == 0) {
        return(NULL)
      }
      plotAgeSpline(ageSpline)
    }
  })

  output$seasonSplinePlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      seasonSpline <- getSpline(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId,
        splineType = "season"
      )
      if (nrow(seasonSpline) == 0) {
        return(NULL)
      }
      plotSeasonSpline(seasonSpline)
    }
  })

  output$calendarTimeSplinePlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      calendarTimeSpline <- getSpline(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        databaseId = row$databaseId,
        analysisId = row$analysisId,
        splineType = "calendar time"
      )
      if (nrow(calendarTimeSpline) == 0) {
        return(NULL)
      }
      plotCalendarTimeSpline(calendarTimeSpline)
    }
  })

  output$controlEstimatesPlot <- renderPlot({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      diagnosticsSummary <- getDiagnosticsSummary(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        covariateId = row$covariateId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )

      controlEstimates <- getControlEstimates(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        covariateId = row$covariateId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )
      plotControlEstimates(controlEstimates, diagnosticsSummary$ease)
    }
  })

  output$diagnosticsSummary <- renderTable({
    row <- selectedRow()
    if (is.null(row)) {
      return(NULL)
    } else {
      diagnosticsSummary <- getDiagnosticsSummary(
        connectionPool = connectionPool,
        resultsDatabaseSchema = resultsDatabaseSchema,
        exposuresOutcomeSetId = row$exposuresOutcomeSetId,
        covariateId = row$covariateId,
        databaseId = row$databaseId,
        analysisId = row$analysisId
      )
      table <- renderDiagnosticsSummary(diagnosticsSummary)
      return(table)
    }
  })

})
