# Copyright 2022 Observational Health Data Sciences and Informatics
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

# options(andromedaTempFolder = "s:/andromedaTemp")
# outputFolder <- "s:/temp/sccsVignette2"
# maxCores <- 8
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
#' @param maxCores      How many parallel cores should be used?
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
  checkmate::assertFileExists(file.path(outputFolder, "sccsAnalysisList.rds"), add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "exposuresOutcomeList.rds"), add = errorMessages)
  checkmate::assertFileExists(file.path(outputFolder, "resultsSummary.rds"), add = errorMessages)
  checkmate::assertCharacter(exportFolder, len = 1, add = errorMessages)
  checkmate::assertInt(minCellCount, lower = 0, add = errorMessages)
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
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

  exportFromSccsData(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportExposuresOutcomes(
    outputFolder = outputFolder,
    exportFolder = exportFolder
  )

  exportFromSccsModel(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
  )

  exportDiagnosticsSummary(
    outputFolder = outputFolder,
    exportFolder = exportFolder,
    databaseId = databaseId
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
  colnames(data) <- SqlRender::camelCaseToSnakeCase(colnames(data))
  readr::write_csv(x = data, file = fileName, append = append)
}

enforceMinCellValue <- function(data, fieldName, minValues, silent = FALSE) {
  toCensor <- !is.na(pull(data, fieldName)) & pull(data, fieldName) < minValues & pull(data, fieldName) != 0
  if (!silent) {
    percent <- round(100 * sum(toCensor) / nrow(data), 1)
    message(
      "   censoring ",
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
    file = system.file("csv", "resultsDataModelSpecification.csv", package = "CohortMethod"),
    show_col_types = FALSE) %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    filter(.data$tableName == !!tableName) %>%
    pull(.data$columnName) %>%
    SqlRender::snakeCaseToCamelCase()
  result <- vector(length = length(columns))
  names(result) <- columns
  result <- as_tibble(t(result), name_repair = "check_unique")
  result <- result[FALSE, ]
  return(result)
}

exportSccsAnalyses <- function(outputFolder, exportFolder) {
  sccsAnalysisListFile <- file.path(outputFolder, "sccsAnalysisList.rds")
  sccsAnalysisList <- readRDS(sccsAnalysisListFile)

  message("- sccs_analysis table")
  tempFileName <- tempfile()
  sccsAnalysisToRow <- function(sccsAnalysis) {
    ParallelLogger::saveSettingsToJson(sccsAnalysis, tempFileName)
    row <- tibble(
      analysisId = sccsAnalysis$analysisId,
      description = sccsAnalysis$description,
      definition = readChar(tempFileName, file.info(tempFileName)$size)
    )
    return(row)
  }
  sccsAnalysis <- lapply(sccsAnalysisList, sccsAnalysisToRow)
  sccsAnalysis <- bind_rows(sccsAnalysis) %>%
    distinct()
  unlink(tempFileName)
  fileName <- file.path(exportFolder, "sccs_analysis.csv")
  writeToCsv(sccsAnalysis, fileName)

  message("- sccs_covariate_analysis table")
  # sccsAnalysis <- sccsAnalysisList[[1]]
  # i = 1
  sccsAnalysisToRows <- function(sccsAnalysis) {
    if (is.list(sccsAnalysis$createIntervalDataArgs$eraCovariateSettings) && class(sccsAnalysis$createIntervalDataArgs$eraCovariateSettings) != "EraCovariateSettings") {
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
        coveriateAnalysisName = eraCovariateSettings$label,
        variableOfInterest = eraCovariateSettings$exposureOfInterest
      )
      rows[[length(rows) + 1]] <- row
    }
    return(bind_rows(rows))
  }
  sccsCoveriateAnalysis <- lapply(sccsAnalysisList, sccsAnalysisToRows)
  sccsCoveriateAnalysis <- sccsCoveriateAnalysis %>%
    bind_rows()
  fileName <- file.path(exportFolder, "sccs_covariate_analysis.csv")
  writeToCsv(sccsCoveriateAnalysis, fileName)
}

exportFromSccsData <- function(outputFolder, exportFolder, databaseId) {
  # Combining processing of tables so we only have to load sccsData objects once
  message("- sccs_era table")
  reference <- getFileReference(outputFolder)
  sccsDataFiles <- reference %>%
    distinct(.data$sccsDataFile) %>%
    filter(.data$sccsDataFile != "") %>%
    pull()

  sccsEra <- list()

  # sccsDataFile = sccsDataFiles[1]
  for (sccsDataFile in sccsDataFiles) {
    sccsData <- SelfControlledCaseSeries::loadSccsData(file.path(outputFolder, sccsDataFile))
    eraRef <- sccsData$eraRef %>%
      collect()
    rows <- reference %>%
      filter(.data$sccsDataFile == !!sccsDataFile) %>%
      select("exposuresOutcomeSetId", "analysisId") %>%
      inner_join(eraRef, by = character())
    sccsEra[[length(sccsEra) + 1]] <- rows
  }
  sccsEra <- sccsEra %>%
    bind_rows()

  fileName <- file.path(exportFolder, "sccs_era.csv")
  writeToCsv(sccsEra, fileName)
}

exportExposuresOutcomes <- function(outputFolder, exportFolder) {
  message("- sccs_exposure and sccs_exposures_outcome_set tables")

  esoList <- readRDS(file.path(outputFolder, "exposuresOutcomeList.rds"))

  # exposure = eso$exposures[[1]]
  convertExposureToTable <- function(exposure) {
    tibble(
      eraId = exposure$exposureId,
      trueEffectSize = if (is.null(exposure$trueEffectSize)) as.numeric(NA) else exposure$trueEffectSize
    ) %>%
      return()
  }
  sccsExposure <- list()
  sccsExposuresOutcomeSet <- list()

  # i = 1
  for (i in seq_along(esoList)) {
    eso <- esoList[[i]]
    exposures <- lapply(eso$exposures, convertExposureToTable) %>%
      bind_rows() %>%
      mutate(
        exposuresOutcomeSetId = i
      )
    sccsExposure[[length(sccsExposure) + 1]] <- exposures

    exposuresOutcomeSet <- tibble(
      exposuresOutcomeSetId = i,
      outcomeId = eso$outcomeId
    )
    sccsExposuresOutcomeSet[[length(sccsExposuresOutcomeSet) + 1]] <- exposuresOutcomeSet
  }

  sccsExposure <- sccsExposure %>%
    bind_rows()
  fileName <- file.path(exportFolder, "sccs_exposure.csv")
  writeToCsv(sccsExposure, fileName)

  sccsExposuresOutcomeSet <- sccsExposuresOutcomeSet %>%
    bind_rows()
  fileName <- file.path(exportFolder, "sccs_exposures_outcome_set.csv")
  writeToCsv(sccsExposuresOutcomeSet, fileName)
}

exportFromSccsModel <- function(outputFolder, exportFolder, databaseId) {
  # Combining processing of tables so we only have to load sccsModel objects once
  message("- sccs_attrition, sccs_covariate, sccs_covariate_result, and sccs_likelihood_profile tables")

  reference <- getFileReference(outputFolder)

  #TODO: add splines, some of the diagnostics (maybe combine with fromSccsData?)

  sccsAttrition <- list()
  sccsCovariate <- list()
  sccsCovariateResult <- list()
  sccsLikelihoodProfile <- list()

  # i = 1
  for (i in seq_len(nrow(reference))) {
    refRow <- reference[i, ]
    sccsModel <- readRDS(file.path(outputFolder, as.character(refRow$sccsModelFile)))

    sccsAttrition[[length(sccsAttrition) + 1]] <- refRow %>%
      select("analysisId", "exposuresOutcomeSetId") %>%
      bind_cols(
        sccsModel$metaData$attrition %>%
          rename(outcomeObservationPeriod = "outcomeObsPeriods") %>%
          mutate(sequenceNumber = row_number())
      ) %>%
      mutate(databaseId = !!databaseId)

    sccsCovariate[[length(sccsCovariate) + 1]] <- refRow %>%
      select("analysisId", "exposuresOutcomeSetId") %>%
      bind_cols(
        sccsModel$metaData$covariateRef %>%
          select("covariateAnalysisId", "covariateId", "covariateName", eraId = "originalEraId", eraName = "originalEraName")
      ) %>%
      mutate(databaseId = !!databaseId)

    if (is.null(sccsModel$estimates)) {
      estimates <- tibble(covariateId = 1, rr = 1, ci95Lb = 1, ci95Ub = 1) %>%
        filter(.data$covariateId == 2)
    } else {
      estimates <- sccsModel$estimates %>%
        mutate(rr = exp(.data$logRr), ci95Lb = exp(.data$logLb95), ci95Ub = exp(.data$logUb95))
    }
    sccsCovariateResult[[length(sccsCovariateResult) + 1]] <- refRow %>%
      select("analysisId", "exposuresOutcomeSetId") %>%
      bind_cols(
        sccsModel$metaData$covariateRef %>%
          select("covariateId") %>%
          left_join(estimates %>%
                      select("covariateId", "rr" , "ci95Lb", "ci95Ub"),
                    by = "covariateId")
      ) %>%
      mutate(databaseId = !!databaseId)

    # j = 1
    for (j in seq_along(sccsModel$logLikelihoodProfiles)) {
      sccsLikelihoodProfile[[length(sccsLikelihoodProfile) + 1]] <- refRow %>%
        select("analysisId", "exposuresOutcomeSetId") %>%
        bind_cols(
          sccsModel$logLikelihoodProfiles[[j]] %>%
            rename(logRr = "point", logLikelihood = "value") %>%
            mutate(covariateId = as.numeric(names(sccsModel$logLikelihoodProfiles[j])))
        ) %>%
        mutate(databaseId = !!databaseId)
    }
  }

  sccsAttrition <- sccsAttrition %>%
    bind_rows()
  fileName <- file.path(exportFolder, "sccs_attrition.csv")
  writeToCsv(sccsAttrition, fileName)

  sccsCovariate <- sccsCovariate %>%
    bind_rows() %>%
    distinct()
  fileName <- file.path(exportFolder, "sccs_covariate.csv")
  writeToCsv(sccsCovariate, fileName)

  sccsCovariateResult <- sccsCovariateResult %>%
    bind_rows()
  fileName <- file.path(exportFolder, "sccs_covariate_result.csv")
  writeToCsv(sccsCovariateResult, fileName)

  sccsLikelihoodProfile <- sccsLikelihoodProfile %>%
    bind_rows()
  fileName <- file.path(exportFolder, "sccs_likelihood_profile.csv")
  writeToCsv(sccsLikelihoodProfile, fileName)
}



exportSccsResults <- function(outputFolder,
                                      exportFolder,
                                      databaseId,
                                      minCellCount) {
  message("- sccs_result table")
  results <- getResultsSummary(outputFolder) %>%
    select(
      "analysisId",
      "exposuresOutcomeSetId",
      "covariateId",
      "rr",
      "ci95Lb",
      "ci95Ub",
      "p",
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
      "calibratedLogRr",
      "calibratedSeLogRr"
    ) %>%
    mutate(databaseId = !!databaseId) %>%
    enforceMinCellValue("outcomeSubjects", minCellCount) %>%
    enforceMinCellValue("outcomeEvents", minCellCount) %>%
    enforceMinCellValue("outcomeObservationPeriods", minCellCount) %>%
    enforceMinCellValue("covariateSubjects", minCellCount) %>%
    enforceMinCellValue("covariateDays", minCellCount) %>%
    enforceMinCellValue("covariateEras", minCellCount) %>%
    enforceMinCellValue("covariateOutcomes", minCellCount) %>%
    enforceMinCellValue("observedDays", minCellCount)
  fileName <- file.path(exportFolder, "sccs_result.csv")
  writeToCsv(results, fileName)
}

exportDiagnosticsSummary <- function(outputFolder = outputFolder,
                                     exportFolder = exportFolder,
                                     databaseId = databaseId) {
  #TODO: change from CM to SCCS
  message("- diagnostics_summary table")
  reference <- getFileReference(outputFolder)

  getMaxSdm <- function(balanceFile) {
    balance <- readRDS(file.path(outputFolder, balanceFile))
    if (nrow(balance) == 0) {
      return(as.numeric(NA))
    } else {
      return(max(abs(balance$afterMatchingStdDiff), na.rm = TRUE))
    }
  }

  getEquipoise <- function(sharedPsFile) {
    ps <- readRDS(file.path(outputFolder, sharedPsFile))
    return(computeEquipoise(ps))
  }

  balanceFiles <- reference %>%
    filter(.data$balanceFile != "") %>%
    distinct(.data$balanceFile) %>%
    pull()
  maxSdm <- as.numeric(sapply(balanceFiles, getMaxSdm))

  sharedBalanceFiles <- reference %>%
    filter(.data$sharedBalanceFile != "") %>%
    distinct(.data$sharedBalanceFile) %>%
    pull()
  sharedMaxSdm <- as.numeric(sapply(sharedBalanceFiles, getMaxSdm))

  sharedPsFiles <- reference %>%
    filter(.data$sharedPsFile != "") %>%
    distinct(.data$sharedPsFile) %>%
    pull()
  equipoise <- as.numeric(sapply(sharedPsFiles, getEquipoise))

  results1 <- reference %>%
    filter(.data$outcomeOfInterest) %>%
    left_join(tibble(
      balanceFile = balanceFiles,
      maxSdm = maxSdm
    ),
    by = "balanceFile"
    ) %>%
    left_join(tibble(
      sharedBalanceFile = sharedBalanceFiles,
      sharedMaxSdm = sharedMaxSdm
    ),
    by = "sharedBalanceFile"
    ) %>%
    left_join(tibble(
      sharedPsFile = sharedPsFiles,
      equipoise = equipoise
    ),
    by = "sharedPsFile"
    ) %>%
    select(
      .data$analysisId,
      .data$targetId,
      .data$comparatorId,
      .data$outcomeId,
      .data$maxSdm,
      .data$sharedMaxSdm,
      .data$equipoise
    )

  results2 <- getResultsSummary(outputFolder) %>%
    select(
      .data$analysisId,
      .data$targetId,
      .data$comparatorId,
      .data$outcomeId,
      .data$mdrr,
      .data$attritionFraction,
      .data$ease
    )

  results <- results1 %>%
    inner_join(results2, by = c("analysisId", "targetId", "comparatorId", "outcomeId")) %>%
    mutate(balanceDiagnostic = case_when(
      is.na(.data$maxSdm) ~ "NOT EVALUATED",
      .data$maxSdm < 0.1 ~ "PASS",
      TRUE ~ "FAIL"
    )) %>%
    mutate(sharedBalanceDiagnostic = case_when(
      is.na(.data$sharedMaxSdm) ~ "NOT EVALUATED",
      .data$sharedMaxSdm < 0.1 ~ "PASS",
      TRUE ~ "FAIL"
    )) %>%
    mutate(equipoiseDiagnostic = case_when(
      is.na(.data$equipoise) ~ "NOT EVALUATED",
      .data$equipoise >= 0.5 ~ "PASS",
      .data$equipoise >= 0.1 ~ "WARNING",
      TRUE ~ "FAIL"
    )) %>%
    mutate(mdrrDiagnostic = case_when(
      is.na(.data$mdrr) ~ "NOT EVALUATED",
      .data$mdrr < 2 ~ "PASS",
      .data$mdrr < 10 ~ "WARNING",
      TRUE ~ "FAIL"
    )) %>%
    mutate(attritionDiagnostic = case_when(
      is.na(.data$attritionFraction) ~ "NOT EVALUATED",
      .data$attritionFraction < 0.5 ~ "PASS",
      .data$attritionFraction < 0.9 ~ "WARNING",
      TRUE ~ "FAIL"
    )) %>%
    mutate(easeDiagnostic = case_when(
      is.na(.data$ease) ~ "NOT EVALUATED",
      abs(.data$ease) < 0.1 ~ "PASS",
      abs(.data$ease) < 0.25 ~ "WARNING",
      TRUE ~ "FAIL"
    )) %>%
    mutate(unblind = ifelse(.data$mdrrDiagnostic != "FAIL" &
                              .data$attritionDiagnostic != "FAIL" &
                              .data$easeDiagnostic != "FAIL" &
                              .data$equipoiseDiagnostic != "FAIL" &
                              .data$balanceDiagnostic != "FAIL", 1, 0),
           databaseId = !!databaseId)

  if (nrow(results) == 0) {
    results <- createEmptyResult("sccs_diagnostics_summary")
  }
  fileName <- file.path(exportFolder, "sccs_diagnostics_summary.csv")
  writeToCsv(results, fileName)
}
