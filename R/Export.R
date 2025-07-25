# Copyright 2025 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCaseSeries
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# options(andromedaTempFolder = "d:/andromedaTemp")
# library(dplyr)

#' Export SCCSresults to CSV files
#'
#' @details
#' This requires that [runSccsAnalyses()] has been executed first. It exports
#' all the results in the `outputFolder` to CSV files for sharing with other
#' sites.
#'
#' @param outputFolder  The folder where runCmAnalyses() generated all results.
#' @param exportFolder  The folder where the CSV files will written.
#' @param databaseId    A unique ID for the database. This will be appended to
#'                      most tables.
#' @param minCellCount  To preserve privacy: the minimum number of subjects contributing
#'                      to a count before it can be included in the results. If the
#'                      count is below this threshold, it will be set to `-minCellCount`.
#' @param maxCores  Maximum number of CPU cores to use.
#'
#' @return
#' Does not return anything. Is called for the side-effect of populating the `exportFolder`
#' with CSV files.
#'
#' @export
exportToCsv <- function(outputFolder,
                        exportFolder = file.path(outputFolder, "export"),
                        databaseId = 1,
                        minCellCount = 5,
                        maxCores = 1) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::assertDirectoryExists(outputFolder, add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "sccsAnalysesSpecifications.rds"), add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "resultsSummary.rds"), add = errorMessages)
  checkmate::assertCharacter(exportFolder, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, lower = 0, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  start <- Sys.time()
  message("Exporting results to CSV")
  exportSccsAnalyses(
    outputFolder = outputFolder,
    exportFolder = exportFolder
  )

  exportExposuresOutcomes(
    outputFolder = outputFolder,
    exportFolder = exportFolder
  )

  exportFromSccsDataStudyPopSccsModel(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount,
    maxCores = maxCores
  )

  exportSccsResults(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId,
    minCellCount = minCellCount
  )

  exportDiagnosticsSummary(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportEventDepObservation(
    exportFolder = exportFolder
  )

  # Add all to zip file -------------------------------------------------------------------------------
  message("Adding results to zip file")
  zipName <- file.path(exportFolder, sprintf("Results_%s.zip", databaseId))
  files <- list.files(exportFolder, pattern = ".*\\.csv$")
  oldWd <- setwd(exportFolder)
  on.exit(setwd(oldWd))
  DatabaseConnector::createZipFile(zipFile = zipName, files = files)

  delta <- Sys.time() - start
  message("Exporting to CSV took ", signif(delta, 3), " ", attr(delta, "units"))
  message("Results are ready for sharing at:", zipName)
}

writeToCsv <- function(data, fileName, append = FALSE) {
  if (nrow(data) == 0) {
    tableName <- gsub(".csv", "", basename(fileName))
    data <- createEmptyResult(tableName)
  }
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  readr::write_csv(x = data, file = fileName, append = append)
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(pull(data, fieldName)) & pull(data, fieldName) < minValues & pull(data, fieldName) != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor) / nrow(data), 1)
    message(
      "    censoring ",
      sum(toCensor),
      " values (",
      percent,
      "%) from ",
      fieldName,
      " because value below minimum"
    )
  }
  if (length(minValues) == 1) {
    data[toCensor, fieldName] <- -minValues
  } else {
    data[toCensor, fieldName] <- -minValues[toCensor]
  }
  return(data)
}

createEmptyResult <- function(tableName) {
  columns <- readr::read_csv(
    file = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"),
    show_col_types = FALSE
  ) |>
    SqlRender::snakeCaseToCamelCaseNames() |>
    filter(.data$tableName == !!tableName) |>
    pull(.data$columnName) |>
    SqlRender::snakeCaseToCamelCase()
  result <- vector(length = length(columns))
  names(result) <- columns
  result <- as_tibble(t(result), name_repair = "check_unique")
  result <- result[FALSE, ]
  return(result)
}

exportSccsAnalyses <- function(outputFolder, exportFolder) {
  sccsAnalysesSpecificationsFile <- file.path(outputFolder, "sccsAnalysesSpecifications.rds")
  sccsAnalysesSpecifications <- readRDS(sccsAnalysesSpecificationsFile)

  message("- sccs_analysis table")
  sccsAnalysisToRow <- function(sccsAnalysis) {
    row <- tibble(
      analysisId = sccsAnalysis$analysisId,
      description = sccsAnalysis$description,
      definition = as.character(sccsAnalysis$toJson())
    )
    return(row)
  }
  sccsAnalysis <- lapply(sccsAnalysesSpecifications$sccsAnalysisList, sccsAnalysisToRow)
  sccsAnalysis <- bind_rows(sccsAnalysis) |>
    distinct()
  fileName <- file.path(exportFolder, "sccs_analysis.csv")
  writeToCsv(sccsAnalysis, fileName)

  message("- sccs_covariate_analysis table")
  # sccsAnalysis <- sccsAnalysisList[[1]]
  # i = 1
  sccsAnalysisToRows <- function(sccsAnalysis) {
    if (is.list(sccsAnalysis$createIntervalDataArgs$eraCovariateSettings) && !is(sccsAnalysis$createIntervalDataArgs$eraCovariateSettings, "EraCovariateSettings")) {
      eraCovariateSettingsList <- sccsAnalysis$createIntervalDataArgs$eraCovariateSettings
    } else {
      eraCovariateSettingsList <- list(sccsAnalysis$createIntervalDataArgs$eraCovariateSettings)
    }
    rows <- list()
    for (i in seq_along(eraCovariateSettingsList)) {
      eraCovariateSettings <- eraCovariateSettingsList[[i]]
      row <- tibble(
        analysisId = sccsAnalysis$analysisId,
        covariateAnalysisId = i,
        covariateAnalysisName = eraCovariateSettings$label,
        variableOfInterest = ifelse(eraCovariateSettings$exposureOfInterest, 1, 0),
        preExposure = ifelse(eraCovariateSettings$preExposure, 1, 0),
        endOfObservationPeriod = 0
      )
      rows[[length(rows) + 1]] <- row
    }
    return(bind_rows(rows))
  }
  sccsCovariateAnalysis <- lapply(sccsAnalysesSpecifications$sccsAnalysisList, sccsAnalysisToRows)
  sccsCovariateAnalysis <- sccsCovariateAnalysis |>
    bind_rows() |>
    bind_rows(
      tibble(
        analysisId = 99,
        covariateAnalysisId = 99,
        covariateAnalysisName = "End of observation period",
        variableOfInterest = 0,
        preExposure = 0,
        endOfObservationPeriod = 1
      )
    )
  fileName <- file.path(exportFolder, "sccs_covariate_analysis.csv")
  writeToCsv(sccsCovariateAnalysis, fileName)
}

exportExposuresOutcomes <- function(outputFolder, exportFolder) {
  message("- sccs_exposure and sccs_exposures_outcome_set tables")

  sccsAnalysesSpecificationsFile <- file.path(outputFolder, "sccsAnalysesSpecifications.rds")
  sccsAnalysesSpecifications <- readRDS(sccsAnalysesSpecificationsFile)
  esoList <- sccsAnalysesSpecifications$exposuresOutcomeList
  ref <- getFileReference(outputFolder) |>
    distinct(.data$exposuresOutcomeSetId, .data$exposuresOutcomeSetSeqId)

  # exposure = eso$exposures[[1]]
  convertExposureToTable <- function(exposure) {
    table <- tibble(
      eraId = exposure$exposureId,
      trueEffectSize = if (is.null(exposure$trueEffectSize)) as.numeric(NA) else exposure$trueEffectSize
    )
    return(table)
  }
  sccsExposure <- list()
  sccsExposuresOutcomeSet <- list()

  # i = 1
  for (i in seq_along(esoList)) {
    eso <- esoList[[i]]
    exposures <- lapply(eso$exposures, convertExposureToTable) |>
      bind_rows() |>
      mutate(
        exposuresOutcomeSetSeqId = i
      )
    sccsExposure[[length(sccsExposure) + 1]] <- exposures

    exposuresOutcomeSet <- tibble(
      exposuresOutcomeSetSeqId = i,
      nestingCohortId = if (is.null(eso$nestingCohortId)) NA else as.numeric(eso$nestingCohortId),
      outcomeId = eso$outcomeId
    )
    sccsExposuresOutcomeSet[[length(sccsExposuresOutcomeSet) + 1]] <- exposuresOutcomeSet
  }

  sccsExposure <- sccsExposure |>
    bind_rows() |>
    inner_join(ref, by = join_by("exposuresOutcomeSetSeqId")) |>
    select(-"exposuresOutcomeSetSeqId")
  fileName <- file.path(exportFolder, "sccs_exposure.csv")
  writeToCsv(sccsExposure, fileName)

  sccsExposuresOutcomeSet <- sccsExposuresOutcomeSet |>
    bind_rows() |>
    inner_join(ref, by = join_by("exposuresOutcomeSetSeqId")) |>
    select(-"exposuresOutcomeSetSeqId")
  fileName <- file.path(exportFolder, "sccs_exposures_outcome_set.csv")
  writeToCsv(sccsExposuresOutcomeSet, fileName)
}

exportFromSccsDataStudyPopSccsModel <- function(outputFolder, exportFolder, databaseId, minCellCount, maxCores) {
  message("- sccs_age_spanning, sccs_attrition, sccs_calender_time_spanning, sccs_censor_model, sccs_covariate, sccs_covariate_result, sccs_era, sccs_event_dep_observation, sccs_likelihood_profile, sccs_spline, sccs_time_to_event, and sccs_time_trend tables")
  groups <- getFileReference(outputFolder) |>
    group_by(.data$sccsDataFile) |>
    group_split()
  cluster <- ParallelLogger::makeCluster(maxCores)
  on.exit(ParallelLogger::stopCluster(cluster))
  tables <- ParallelLogger::clusterApply(
    cluster = cluster,
    groups,
    exportGroup,
    outputFolder = outputFolder,
    databaseId = databaseId
  )
  getTable <- function(tableName) {
    table <- do.call(c, lapply(tables, function(table) table[tableName])) |>
      bind_rows()
    return(table)
  }
  message("  Censoring sccs_age_spanning table")
  sccsAgeSpanning <- getTable("sccsAgeSpanning") |>
    enforceMinCellValue("coverBeforeAfterSubjects", minCellCount)
  fileName <- file.path(exportFolder, "sccs_age_spanning.csv")
  writeToCsv(sccsAgeSpanning, fileName)

  sccsEra <- getTable("sccsEra")
  fileName <- file.path(exportFolder, "sccs_era.csv")
  writeToCsv(sccsEra, fileName)

  message("  Censoring sccs_attrition table")
  sccsAttrition <- getTable("sccsAttrition") |>
    enforceMinCellValue("outcomeSubjects", minCellCount) |>
    enforceMinCellValue("outcomeEvents", minCellCount) |>
    enforceMinCellValue("outcomeObservationPeriods", minCellCount) |>
    enforceMinCellValue("observedDays", minCellCount)
  fileName <- file.path(exportFolder, "sccs_attrition.csv")
  writeToCsv(sccsAttrition, fileName)

  message("  Censoring sccs_calendar_time_spanning table")
  sccsCalendarTimeSpanning <- getTable("sccsCalendarTimeSpanning") |>
    enforceMinCellValue("coverBeforeAfterSubjects", minCellCount)
  fileName <- file.path(exportFolder, "sccs_calendar_time_spanning.csv")
  writeToCsv(sccsCalendarTimeSpanning, fileName)

  sccsTimePeriod <- getTable("sccsTimePeriod")
  fileName <- file.path(exportFolder, "sccs_time_period.csv")
  writeToCsv(sccsTimePeriod, fileName)

  sccsCovariate <- getTable("sccsCovariate") |>
    distinct()
  fileName <- file.path(exportFolder, "sccs_covariate.csv")
  writeToCsv(sccsCovariate, fileName)

  sccsCovariateResult <- getTable("sccsCovariateResult")
  fileName <- file.path(exportFolder, "sccs_covariate_result.csv")
  writeToCsv(sccsCovariateResult, fileName)

  sccsDiagnosticsSummary <- getTable("sccsDiagnosticsSummary")
  fileName <- file.path(exportFolder, "sccs_diagnostics_summary.csv")
  writeToCsv(sccsDiagnosticsSummary, fileName)

  sccsLikelihoodProfile <- getTable("sccsLikelihoodProfile")
  fileName <- file.path(exportFolder, "sccs_likelihood_profile.csv")
  writeToCsv(sccsLikelihoodProfile, fileName)

  sccsSpline <- getTable("sccsSpline")
  fileName <- file.path(exportFolder, "sccs_spline.csv")
  writeToCsv(sccsSpline, fileName)

  sccsCensorModel <- getTable("sccsCensorModel")
  fileName <- file.path(exportFolder, "sccs_censor_model.csv")
  writeToCsv(sccsCensorModel, fileName)

  message("  Censoring sccs_time_to_event table")
  sccsTimeToEvent <- getTable("sccsTimeToEvent") |>
    enforceMinCellValue("outcomes", minCellCount) |>
    enforceMinCellValue("observedSubjects", minCellCount)
  fileName <- file.path(exportFolder, "sccs_time_to_event.csv")
  writeToCsv(sccsTimeToEvent, fileName)

  message(" Censoring sccs_time_trend table")
  sccsTimeTrend <- getTable("sccsTimeTrend") |>
    enforceMinCellValue("observedSubjects", minCellCount)
  fileName <- file.path(exportFolder, "sccs_time_trend.csv")
  writeToCsv(sccsTimeTrend, fileName)
}

exportGroup <- function(group, outputFolder, databaseId) {
  group <- group |>
    arrange(.data$studyPopFile, .data$sccsModelFile)
  sccsAgeSpanning <- list()
  sccsAttrition <- list()
  sccsCalendarTimeSpanning <- list()
  sccsCensorModel <- list()
  sccsCovariate <- list()
  sccsCovariateResult <- list()
  sccsEra <- list()
  sccsLikelihoodProfile <- list()
  sccsSpline <- list()
  sccsTimeToEvent <- list()
  sccsTimeTrend <- list()
  sccsTimePeriod <- list()
  sccsDiagnosticsSummary <- list()

  sccsDataFile <- group$sccsDataFile[1]
  ParallelLogger::logTrace("Processing ", sccsDataFile)
  sccsData <- loadSccsData(file.path(outputFolder, sccsDataFile))
  studyPopFile <- ""
  sccsAnalysesSpecificationsFile <- file.path(outputFolder, "sccsAnalysesSpecifications.rds")
  sccsAnalysesSpecifications <- readRDS(sccsAnalysesSpecificationsFile)
  esoList <- sccsAnalysesSpecifications$exposuresOutcomeList

  # sccsEra table
  eraRef <- sccsData$eraRef |>
    select("eraType", "eraId", "eraName") |>
    collect()
  rows <- group |>
    select("exposuresOutcomeSetId", "analysisId") |>
    cross_join(eraRef) |>
    mutate(databaseId = !!databaseId)
  sccsEra <- rows

  for (i in seq_len(nrow(group))) {
    refRow <- group[i, ]
    if (refRow$studyPopFile != studyPopFile) {
      studyPopFile <- refRow$studyPopFile
      studyPop <- readRDS(file.path(outputFolder, studyPopFile))

      # sccs_age_spanning table
      ageSpans <- computeSpans(studyPop, variable = "age") |>
        mutate(databaseId = !!databaseId)
      refRows <- group |>
        filter(.data$studyPopFile == !!studyPopFile)
      sccsAgeSpanning[[length(sccsAgeSpanning) + 1]] <- refRows |>
        select("analysisId", "exposuresOutcomeSetId") |>
        cross_join(ageSpans)

      # sccs_calendar_time_spanning table
      timeSpans <- computeSpans(studyPop, variable = "time") |>
        mutate(databaseId = !!databaseId)
      refRows <- group |>
        filter(.data$studyPopFile == !!studyPopFile)
      sccsCalendarTimeSpanning[[length(sccsCalendarTimeSpanning) + 1]] <- refRows |>
        select("analysisId", "exposuresOutcomeSetId") |>
        cross_join(timeSpans)

      # time_period table
      if (!is.null(studyPop$metaData$restrictedTimeToEra)) {
        timePeriod <- studyPop$metaData$restrictedTimeToEra |>
          select(minDate = "minObservedDate",
                 maxDate = "maxObservedDate")
      } else {
        if (nrow(studyPop$cases) == 0) {
          timePeriod <- tibble(minDate = as.Date(NA), maxDate = as.Date(NA))
        } else {
          timePeriod <- studyPop$cases |>
            mutate(startDate = .data$observationPeriodStartDate + .data$startDay,
                   endDate = .data$observationPeriodStartDate + .data$endDay) |>
            summarise(minDate = min(.data$startDate),
                      maxDate = max(.data$endDate))
        }
      }
      refRows <- group |>
        filter(.data$studyPopFile == !!studyPopFile)
      sccsTimePeriod[[length(sccsCalendarTimeSpanning) + 1]] <- refRows |>
        select("analysisId", "exposuresOutcomeSetId") |>
        cross_join(timePeriod)
    }
    sccsModel <- readRDS(file.path(outputFolder, as.character(refRow$sccsModelFile)))
    if (is.null(sccsModel$estimates)) {
      estimates <- tibble(covariateId = 1, rr = 1, ci95Lb = 1, ci95Ub = 1) |>
        filter(.data$covariateId == 2)
    } else {
      estimates <- sccsModel$estimates |>
        mutate(rr = exp(.data$logRr), ci95Lb = exp(.data$logLb95), ci95Ub = exp(.data$logUb95))
    }

    # sccsTimeToEvent table
    for (exposure in esoList[[refRow$exposuresOutcomeSetSeqId]]$exposures) {
      data <- computeTimeToEvent(
        studyPopulation = studyPop,
        sccsData = sccsData,
        exposureEraId = exposure$exposureId
      ) |>
        mutate(
          databaseId = !!databaseId,
          eraId = exposure$exposureId,
          outcomes = .data$eventsExposed + .data$eventsUnexposed
        ) |>
        select("databaseId",
               "eraId",
               week = "number",
               "outcomes",
               observedSubjects = "observed"
        )

      sccsTimeToEvent[[length(sccsTimeToEvent) + 1]] <- refRow |>
        select("analysisId", "exposuresOutcomeSetId") |>
        bind_cols(data)
    }

    # sccsAttrition table
    baseAttrition <- refRow |>
      select("analysisId", "exposuresOutcomeSetId") |>
      bind_cols(
        sccsModel$metaData$attrition |>
          rename(outcomeObservationPeriods = "outcomeObsPeriods") |>
          mutate(sequenceNumber = row_number()) |>
          select(-"outcomeId")
      ) |>
      mutate(databaseId = !!databaseId)
    # covariateSettings = sccsModel$metaData$covariateSettingsList[[1]]
    for (covariateSettings in sccsModel$metaData$covariateSettingsList) {
      if (covariateSettings$exposureOfInterest) {
        if (is.null(sccsModel$metaData$covariateStatistics)) {
          covariateStatistics <- tibble(
            covariateId = covariateSettings$outputIds[1],
            personCount = 0,
            eraCount = 0,
            dayCount = 0,
            outcomeCount = 0,
            observationPeriodCount = 0,
            observedDayCount = 0,
            observedOutcomeCount = 0
          )
        } else {
          covariateStatistics <- sccsModel$metaData$covariateStatistics
        }
        attrition <- bind_rows(
          baseAttrition,
          covariateStatistics |>
            filter(.data$covariateId == covariateSettings$outputIds[1]) |>
            mutate(
              analysisId = refRow$analysisId,
              exposuresOutcomeSetId = refRow$exposuresOutcomeSetId,
              description = "Having the covariate of interest",
              sequenceNumber = max(baseAttrition$sequenceNumber) + 1,
              databaseId = !!databaseId
            ) |>
            select(
              "analysisId",
              "exposuresOutcomeSetId",
              "description",
              outcomeSubjects = "personCount",
              outcomeEvents = "observedOutcomeCount",
              outcomeObservationPeriods = "observationPeriodCount",
              observedDays = "observedDayCount",
              "sequenceNumber",
              "databaseId"
            )
        ) |>
          mutate(covariateId = covariateSettings$outputIds[1])
        sccsAttrition[[length(sccsAttrition) + 1]] <- attrition
      }
    }

    # sccsCovariate table
    sccsCovariate[[length(sccsCovariate) + 1]] <- refRow |>
      select("analysisId", "exposuresOutcomeSetId") |>
      bind_cols(
        sccsModel$metaData$covariateRef |>
          select("covariateAnalysisId", "covariateId", "covariateName", eraId = "originalEraId")
      ) |>
      mutate(databaseId = !!databaseId)

    # sccsCovariateResult table
    sccsCovariateResult[[length(sccsCovariateResult) + 1]] <- refRow |>
      select("analysisId", "exposuresOutcomeSetId") |>
      bind_cols(
        sccsModel$metaData$covariateRef |>
          select("covariateId") |>
          left_join(
            estimates |>
              select("covariateId", "rr", "ci95Lb", "ci95Ub"),
            by = join_by("covariateId")
          )
      ) |>
      mutate(databaseId = !!databaseId)

    # sccsLikelihoodProfile table
    for (j in seq_along(sccsModel$logLikelihoodProfiles)) {
      if (!is.null(sccsModel$logLikelihoodProfiles[[j]])) {
        sccsLikelihoodProfile[[length(sccsLikelihoodProfile) + 1]] <- refRow |>
          select("analysisId", "exposuresOutcomeSetId") |>
          bind_cols(
            sccsModel$logLikelihoodProfiles[[j]] |>
              rename(logRr = "point", logLikelihood = "value") |>
              mutate(covariateId = as.numeric(names(sccsModel$logLikelihoodProfiles[j])),
                     gradient = as.numeric(NA))
          ) |>
          mutate(databaseId = !!databaseId)
      }
    }

    # sccsSpline table
    if (!is.null(sccsModel$metaData$age)) {
      ageKnots <- sccsModel$metaData$age$ageKnots

      sccsSpline[[length(sccsSpline) + 1]] <- refRow |>
        select("analysisId", "exposuresOutcomeSetId") |>
        bind_cols(
          tibble(
            covariateId = 99 + seq_len(length(ageKnots)),
            knotMonth = ageKnots
          ) |>
            left_join(
              estimates |>
                filter(.data$covariateId >= 99 & .data$covariateId < 200) |>
                select("covariateId", "rr"),
              by = join_by("covariateId")
            ) |>
            select("knotMonth", "rr")
        ) |>
        mutate(
          databaseId = !!databaseId,
          splineType = "age"
        )
    }
    if (!is.null(sccsModel$metaData$seasonality)) {
      seasonKnots <- sccsModel$metaData$seasonality$seasonKnots
      seasonKnots <- seasonKnots[c(-1, -length(seasonKnots))]

      sccsSpline[[length(sccsSpline) + 1]] <- refRow |>
        select("analysisId", "exposuresOutcomeSetId") |>
        bind_cols(
          tibble(
            covariateId = 199 + seq_len(length(seasonKnots)),
            knotMonth = seasonKnots
          ) |>
            left_join(
              estimates |>
                filter(.data$covariateId >= 200 & .data$covariateId < 300) |>
                select("covariateId", "rr"),
              by = join_by("covariateId")
            ) |>
            select("knotMonth", "rr") |>
            bind_rows(tibble(knotMonth = 1, rr = 1))
        ) |>
        mutate(
          databaseId = !!databaseId,
          splineType = "season"
        )
    }
    if (!is.null(sccsModel$metaData$calendarTime)) {
      calendarTimeKnots <- do.call(c, sccsModel$metaData$calendarTime$calendarTimeKnotsInPeriods)
      sccsSpline[[length(sccsSpline) + 1]] <- refRow |>
        select("analysisId", "exposuresOutcomeSetId") |>
        bind_cols(
          tibble(
            covariateId = 299 + seq_len(length(calendarTimeKnots)),
            knotMonth = calendarTimeKnots
          ) |>
            left_join(
              estimates |>
                filter(.data$covariateId >= 299 & .data$covariateId < 400) |>
                select("covariateId", "rr"),
              by = join_by("covariateId")
            ) |>
            select("knotMonth", "rr")
        ) |>
        mutate(
          databaseId = !!databaseId,
          splineType = "calendar time"
        )
    }

    # sccsCensorModel table
    if (!is.null(sccsModel$metaData$censorModel)) {
      censorModel <- sccsModel$metaData$censorModel
      sccsCensorModel[[length(sccsCensorModel) + 1]] <- refRow |>
        select("analysisId", "exposuresOutcomeSetId") |>
        bind_cols(
          tibble(
            parameterId = seq_len(length(censorModel$p)),
            parameterValue = censorModel$p,
            modelType = case_when(
              censorModel$model == 1 ~ "Weibull-Age",
              censorModel$model == 2 ~ "Weibull-Interval",
              censorModel$model == 3 ~ "Gamma-Age",
              censorModel$model == 4 ~ "Gamma-Interval"
            ),
            databaseId = !!databaseId
          )
        )
    }

    timeTrendData <- computeOutcomeRatePerMonth(
      studyPopulation = studyPop,
      sccsModel = sccsModel
    ) |>
      mutate(
        calendarYear = floor(.data$month / 12),
        calendarMonth = floor(.data$month %% 12) + 1,
        observedSubjects = round(.data$observationPeriodCount)
      ) |>
      select(
        "calendarYear",
        "calendarMonth",
        "observedSubjects",
        ratio = "ratio",
        "adjustedRatio"
      ) |>
      mutate(databaseId = !!databaseId)

    # Add deprecated columns:
    timeTrendData <- timeTrendData |>
      mutate(outcomeRate = as.numeric(NA),
             adjustedRate = as.numeric(NA),
             stable = as.numeric(NA),
             p = as.numeric(NA))

    sccsTimeTrend[[length(sccsTimeTrend) + 1]] <- refRow |>
      select("analysisId", "exposuresOutcomeSetId") |>
      bind_cols(timeTrendData)
  }
  return(list(
    sccsAgeSpanning = sccsAgeSpanning,
    sccsAttrition = sccsAttrition,
    sccsCalendarTimeSpanning = sccsCalendarTimeSpanning,
    sccsCensorModel = sccsCensorModel,
    sccsCovariate = sccsCovariate,
    sccsCovariateResult = sccsCovariateResult,
    sccsEra = sccsEra,
    sccsLikelihoodProfile = sccsLikelihoodProfile,
    sccsSpline = sccsSpline,
    sccsTimeToEvent = sccsTimeToEvent,
    sccsTimeTrend = sccsTimeTrend,
    sccsTimePeriod = sccsTimePeriod
  ))
}

computeSpans <- function(studyPopulation, variable = "age") {
  if (variable == "age") {
    ages <- studyPopulation$cases |>
      transmute(
        start = ceiling(.data$ageAtObsStart + .data$startDay / (365.25 / 12)) + 1,
        end = floor((.data$ageAtObsStart + .data$endDay) / (365.25 / 12)) - 1,
        count = 1
      )
  } else {
    ages <- studyPopulation$cases |>
      transmute(
        start = convertDateToMonth(.data$observationPeriodStartDate + .data$startDay) + 1,
        end = convertDateToMonth(.data$observationPeriodStartDate + .data$endDay) - 1,
        count = 1
      )
  }

  addedCounts <- ages |>
    rename(month = "start") |>
    group_by(.data$month) |>
    summarise(addedCount = sum(.data$count), .groups = "drop")
  removedCounts <- ages |>
    rename(month = "end") |>
    group_by(.data$month) |>
    summarise(removedCount = sum(.data$count), .groups = "drop")
  counts <- addedCounts |>
    full_join(removedCounts, by = join_by("month")) |>
    mutate(
      addedCount = if_else(is.na(.data$addedCount), 0, .data$addedCount),
      removedCount = if_else(is.na(.data$removedCount), 0, .data$removedCount)
    ) |>
    mutate(netAdded = .data$addedCount - .data$removedCount) |>
    arrange(.data$month) |>
    mutate(count = cumsum(.data$netAdded)) |>
    filter(count > 0) |>
    select("month", "count")
  if (nrow(counts) != 0) {
    counts <- counts |>
      full_join(
        tibble(month = seq(min(counts$month), max(counts$month))),
        by = join_by("month")
      ) |>
      arrange(.data$month)
  }
  lastCount <- 0
  for (i in seq_len(nrow(counts))) {
    if (is.na(counts$count[i])) {
      counts$count[i] <- lastCount
    } else {
      lastCount <- counts$count[i]
    }
  }
  if (variable == "age") {
    counts <- counts |>
      rename(ageMonth = "month", coverBeforeAfterSubjects = "count")
  } else {
    counts <- counts |>
      transmute(
        calendarYear = floor(.data$month / 12),
        calendarMonth = .data$month %% 12 + 1,
        coverBeforeAfterSubjects = .data$count
      )
  }

  return(counts)
}


exportSccsResults <- function(outputFolder,
                              exportFolder,
                              databaseId,
                              minCellCount) {
  message("- sccs_result table")
  results <- getResultsSummary(outputFolder) |>
    select(
      "analysisId",
      "exposuresOutcomeSetId",
      "covariateId",
      "rr",
      "ci95Lb",
      "ci95Ub",
      "p",
      "oneSidedP",
      "outcomeSubjects",
      "outcomeEvents",
      "outcomeObservationPeriods",
      "covariateSubjects",
      "covariateDays",
      "covariateEras",
      "covariateOutcomes",
      "observedDays",
      "logRr",
      "seLogRr",
      "llr",
      "calibratedRr",
      "calibratedCi95Lb",
      "calibratedCi95Ub",
      "calibratedP",
      "calibratedOneSidedP",
      "calibratedLogRr",
      "calibratedSeLogRr"
    ) |>
    mutate(databaseId = !!databaseId) |>
    enforceMinCellValue("outcomeSubjects", minCellCount) |>
    enforceMinCellValue("outcomeEvents", minCellCount) |>
    enforceMinCellValue("outcomeObservationPeriods", minCellCount) |>
    enforceMinCellValue("covariateSubjects", minCellCount) |>
    enforceMinCellValue("covariateDays", minCellCount) |>
    enforceMinCellValue("covariateEras", minCellCount) |>
    enforceMinCellValue("covariateOutcomes", minCellCount) |>
    enforceMinCellValue("observedDays", minCellCount)
  fileName <- file.path(exportFolder, "sccs_result.csv")
  writeToCsv(results, fileName)
}

exportDiagnosticsSummary <- function(outputFolder = outputFolder,
                                     exportFolder = exportFolder,
                                     databaseId = databaseId){
  message("- sccs_diagnostics_summary table")
  results <- getDiagnosticsSummary(outputFolder) |>
    select(
      "analysisId",
      "exposuresOutcomeSetId",
      "covariateId",
      "timeStabilityP",
      "timeStabilityDiagnostic",
      "eventExposureLb",
      "eventExposureUb",
      "eventExposureDiagnostic",
      "eventObservationLb",
      "eventObservationUb",
      "eventObservationDiagnostic",
      "rareOutcomePrevalence",
      "rareOutcomeDiagnostic",
      "mdrr",
      "mdrrDiagnostic",
      "ease",
      "easeDiagnostic",
      "unblind",
      "unblindForEvidenceSynthesis"
    ) |>
    mutate(databaseId = !!databaseId,
           unblind = as.integer(.data$unblind),
           unblindForEvidenceSynthesis = as.integer(.data$unblindForEvidenceSynthesis))

  # Add deprecated columns:
  results <- results |>
    mutate(timeTrendP = as.numeric(NA),
           preExposureP = as.numeric(NA),
           timeTrendDiagnostic = "NOT EVALUATED",
           preExposureDiagnostic = "NOT EVALUATED")

  fileName <- file.path(exportFolder, "sccs_diagnostics_summary.csv")
  writeToCsv(results, fileName)
}

exportEventDepObservation <- function(exportFolder = exportFolder){
  message("- sccs_event_dep_observation table")
  # Entire table is deprecated
  fileName <- file.path(exportFolder, "sccs_event_dep_observation.csv")
  writeToCsv(tibble(), fileName)
}
