# @file RunAnalyses.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the drug-comparator-outcomes of interest. This function will run all
#' specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(cmAnalysisList) * length(drugComparatorOutcomesList)` (if all analyses specify an
#' outcome model should be fitted). When you provide several analyses it will determine whether any of
#' the analyses have anything in common, and will take advantage of this fact. For example, if we
#' specify several analyses that only differ in the way the outcome model is fitted, then this
#' function will extract the data and fit the propensity model only once, and re-use this in all the
#' analysis.
#'
#' @param connectionDetails                An R object of type `ConnectionDetails` created using
#'                                         the function [DatabaseConnector::createConnectionDetails()].
#' @param cdmDatabaseSchema                The name of the database schema that contains the OMOP CDM
#'                                         instance.  Requires read permissions to this database. On
#'                                         SQL Server, this should specify both the database and the
#'                                         schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                 A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         `outcomeTable = "CONDITION_ERA"`, `outcomeDatabaseSchema` is not
#'                                         used.  Requires read permissions to this database.
#' @param outcomeTable                     The table name that contains the outcome cohorts.  If
#'                                         outcomeTable is not CONDITION_OCCURRENCE or CONDITION_ERA,
#'                                         then expectation is outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param exposureDatabaseSchema           The name of the database schema that is the location where
#'                                         the exposure data used to define the exposure cohorts is
#'                                         available. If `exposureTable = "DRUG_ERA"`,
#'                                         `exposureDatabaseSchema` is not used but assumed to be
#'                                         `cdmDatabaseSchema`.  Requires read permissions to this database.
#' @param exposureTable                    The table name that contains the exposure cohorts.  If
#'                                         `exposureTable <> "DRUG_ERA"`, then expectation is `exposureTable`
#'                                         has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                         COHORT_START_DATE, COHORT_END_DATE.
#' @param customCovariateDatabaseSchema    The name of the database schema that is the location where
#'                                         the custom covariate data is available.
#' @param customCovariateTable             Name of the table holding the custom covariates. This table
#'                                         should have the same structure as the cohort table.
#' @param nestingCohortDatabaseSchema      The name of the database schema that is the location
#'                                         where the nesting cohort is defined.
#' @param nestingCohortTable               Name of the table holding the nesting cohort. This table
#'                                          should have the same structure as the cohort table.
#' @param cdmVersion                       Define the OMOP CDM version used: currently support "4" and
#'                                         "5".
#' @param sccsAnalysisList                 A list of objects of `sccsAnalysis` as created
#'                                         using the [createSccsAnalysis()] function.
#' @param exposureOutcomeList              A list of objects of type `exposureOutcome` as created
#'                                         using the [createExposureOutcome()] function.
#' @param outputFolder                     Name of the folder where all the outputs will written to.
#' @param combineDataFetchAcrossOutcomes   Should fetching data from the database be done one outcome
#'                                         at a time, or for all outcomes in one fetch? Combining
#'                                         fetches will be more efficient if there is large overlap in
#'                                         the subjects that have the different outcomes.
#' @param getDbSccsDataThreads             The number of parallel threads to use for building the
#'                                         `SccsData` objects.
#' @param createStudyPopulationThreads     The number of parallel threads to use for building the
#'                                         `studyPopulation` objects.
#' @param createSccsIntervalDataThreads    The number of parallel threads to use for building the
#'                                         `SccsIntervalData` objects.
#' @param fitSccsModelThreads              The number of parallel threads to use for fitting the
#'                                         models.
#' @param cvThreads                        The number of parallel threads to use for the cross-
#'                                         validation when estimating the hyperparameter for the
#'                                         outcome model. Note that the total number of CV threads at
#'                                         one time could be `fitSccsModelThreads * cvThreads`.
#'
#' @return
#' A tibble describing for each exposure-outcome-analysisId combination where the intermediary and
#' outcome model files can be found, relative to the `outputFolder`.
#'
#' @export
runSccsAnalyses <- function(connectionDetails,
                            cdmDatabaseSchema,
                            oracleTempSchema = cdmDatabaseSchema,
                            exposureDatabaseSchema = cdmDatabaseSchema,
                            exposureTable = "drug_era",
                            outcomeDatabaseSchema = cdmDatabaseSchema,
                            outcomeTable = "condition_era",
                            customCovariateDatabaseSchema = cdmDatabaseSchema,
                            customCovariateTable = "cohort",
                            nestingCohortDatabaseSchema = cdmDatabaseSchema,
                            nestingCohortTable = "cohort",
                            cdmVersion = 5,
                            outputFolder = "./SccsOutput",
                            sccsAnalysisList,
                            exposureOutcomeList,
                            combineDataFetchAcrossOutcomes = TRUE,
                            getDbSccsDataThreads = 1,
                            createStudyPopulationThreads = 1,
                            createSccsIntervalDataThreads = 1,
                            fitSccsModelThreads = 1,
                            cvThreads = 1) {
  for (exposureOutcome in exposureOutcomeList)
    stopifnot(class(exposureOutcome) == "exposureOutcome")
  for (sccsAnalysis in sccsAnalysisList)
    stopifnot(class(sccsAnalysis) == "sccsAnalysis")
  uniqueExposureOutcomeList <- unique(ParallelLogger::selectFromList(exposureOutcomeList,
                                                                     c("exposureId", "outcomeId")))
  if (length(uniqueExposureOutcomeList) != length(exposureOutcomeList))
    stop("Duplicate exposure-outcomes pairs are not allowed")
  uniqueAnalysisIds <- unlist(unique(ParallelLogger::selectFromList(sccsAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(sccsAnalysisList))
    stop("Duplicate analysis IDs are not allowed")

  if (!file.exists(outputFolder))
    dir.create(outputFolder)

  referenceTable <- tibble()
  for (sccsAnalysis in sccsAnalysisList) {
    analysisId <- sccsAnalysis$analysisId
    for (exposureOutcome in exposureOutcomeList) {
      exposureId <- .selectByType(sccsAnalysis$exposureType, exposureOutcome$exposureId, "exposure")
      outcomeId <- .selectByType(sccsAnalysis$outcomeType, exposureOutcome$outcomeId, "outcome")
      row <- tibble(exposureId = exposureId, outcomeId = outcomeId, analysisId = analysisId)
      referenceTable <- rbind(referenceTable, row)
    }
  }

  ### Determine if loading calls can be combined for efficiency ###

  # Step 1: determine concepts to be fetched per analysis - exposure - outcome combination -------
  conceptsPerLoad <- list()
  rowId <- 1
  for (sccsAnalysis in sccsAnalysisList) {
    for (exposureOutcome in exposureOutcomeList) {
      exposureIds <- c()
      if (is.null(sccsAnalysis$getDbSccsDataArgs$exposureIds)) {
        exposureIds <- "all"
      } else {
        for (exposureId in sccsAnalysis$getDbSccsDataArgs$exposureIds) {
          if (suppressWarnings(is.na(as.numeric(exposureId)))) {
            if (is.null(exposureOutcome[[exposureId]]))
              stop(paste("Variable", exposureId, " not found in exposure-outcome pair"))
            exposureIds <- c(exposureIds, exposureOutcome[[exposureId]])
          } else {
            exposureIds <- c(exposureIds, as.numeric(exposureId))
          }
        }
      }
      outcomeId <- .selectByType(sccsAnalysis$outcomeType, exposureOutcome$outcomeId, "outcome")
      customCovariateIds <- c()
      if (sccsAnalysis$getDbSccsDataArgs$useCustomCovariates) {
        if (is.null(sccsAnalysis$getDbSccsDataArgs$customCovariateIds)) {
          customCovariateIds <- "all"
        } else {
          for (customCovariateId in sccsAnalysis$getDbSccsDataArgs$customCovariateIds) {
            if (is.character(customCovariateId)) {
              if (is.null(exposureOutcome[[customCovariateId]]))
                stop(paste("Variable", customCovariateId, " not found in exposure-outcome pair"))
              customCovariateIds <- c(customCovariateIds, exposureOutcome[[customCovariateId]])
            } else {
              customCovariateIds <- c(customCovariateIds, customCovariateId)
            }
          }
        }
      }
      nestingCohortId <- -1
      if (sccsAnalysis$getDbSccsDataArgs$useNestingCohort) {
        nestingCohortId <- sccsAnalysis$getDbSccsDataArgs$nestingCohortId
      }
      row <- list(outcomeId = outcomeId,
                  exposureIds = exposureIds,
                  customCovariateIds = customCovariateIds,
                  nestingCohortId = nestingCohortId,
                  deleteCovariatesSmallCount = sccsAnalysis$getDbSccsDataArgs$deleteCovariatesSmallCount,
                  studyStartDate = sccsAnalysis$getDbSccsDataArgs$studyStartDate,
                  studyEndDate = sccsAnalysis$getDbSccsDataArgs$studyEndDate,
                  maxCasesPerOutcome = sccsAnalysis$getDbSccsDataArgs$maxCasesPerOutcome,
                  rowId = rowId)
      conceptsPerLoad[[length(conceptsPerLoad) + 1]] <- row
      rowId <- rowId + 1
    }
  }

  # Step 2: group loads where possible ------------------------------------------
  if (combineDataFetchAcrossOutcomes) {
    uniqueLoads <- unique(ParallelLogger::selectFromList(conceptsPerLoad,
                                                         c("nestingCohortId",
                                                           "deleteCovariatesSmallCount",
                                                           "studyStartDate",
                                                           "studyEndDate",
                                                           "maxCasesPerOutcome")))
  } else {
    uniqueLoads <- unique(ParallelLogger::selectFromList(conceptsPerLoad,
                                                         c("nestingCohortId",
                                                           "deleteCovariatesSmallCount",
                                                           "studyStartDate",
                                                           "studyEndDate",
                                                           "maxCasesPerOutcome",
                                                           "outcomeId")))
  }
  # Step 3: Compute unions of concept sets, and generate loading arguments and file names ------
  referenceTable$sccsDataFile <- ""
  referenceTable$loadId <- NA
  sccsDataObjectsToCreate <- list()
  for (loadId in 1:length(uniqueLoads)) {
    uniqueLoad <- uniqueLoads[[loadId]]
    groupables <- ParallelLogger::matchInList(conceptsPerLoad, uniqueLoad)
    outcomeIds <- c()
    exposureIds <- c()
    customCovariateIds <- c()
    rowIds <- c()
    for (groupable in groupables) {
      outcomeIds <- c(outcomeIds, groupable$outcomeId)
      if (!(length(exposureIds) == 1 && exposureIds[1] == "all")) {
        if (groupable$exposureIds[1] == "all") {
          exposureIds <- "all"
        } else {
          exposureIds <- c(exposureIds, groupable$exposureIds)
        }
      }
      if (!(length(customCovariateIds) == 1 && customCovariateIds[1] == "all")) {
        if ((length(groupable$customCovariateIds) == 1 && groupable$customCovariateIds[1] == "all")) {
          customCovariateIds <- "all"
        } else {
          customCovariateIds <- c(customCovariateIds, groupable$customCovariateIds)
        }
      }
      rowIds <- c(rowIds, groupable$rowId)
    }
    sccsDataFileName <- .createSccsDataFileName(loadId)
    referenceTable$loadId[rowIds] <- loadId
    referenceTable$sccsDataFile[rowIds] <- sccsDataFileName
    if (!file.exists(file.path(outputFolder, sccsDataFileName))) {
      if (length(exposureIds) == 1 && exposureIds[1] == "all")
        exposureIds <- c()
      useCustomCovariates <- (length(customCovariateIds) > 0)
      if (length(customCovariateIds) == 1 && customCovariateIds[1] == "all")
        customCovariateIds <- c()
      outcomeIds <- unique(outcomeIds)
      exposureIds <- unique(exposureIds)
      customCovariateIds <- unique(customCovariateIds)
      args <- list(connectionDetails = connectionDetails,
                   cdmDatabaseSchema = cdmDatabaseSchema,
                   oracleTempSchema = oracleTempSchema,
                   exposureDatabaseSchema = exposureDatabaseSchema,
                   exposureTable = exposureTable,
                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                   outcomeTable = outcomeTable,
                   customCovariateDatabaseSchema = customCovariateDatabaseSchema,
                   customCovariateTable = customCovariateTable,
                   nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
                   nestingCohortTable = nestingCohortTable,
                   cdmVersion = cdmVersion,
                   exposureIds = exposureIds,
                   outcomeIds = outcomeIds,
                   useCustomCovariates = useCustomCovariates,
                   customCovariateIds = customCovariateIds,
                   useNestingCohort = groupables[[1]]$nestingCohortId != -1,
                   nestingCohortId = groupables[[1]]$nestingCohortId,
                   deleteCovariatesSmallCount = groupables[[1]]$deleteCovariatesSmallCount,
                   studyStartDate = groupables[[1]]$studyStartDate,
                   studyEndDate = groupables[[1]]$studyEndDate,
                   maxCasesPerOutcome = groupables[[1]]$maxCasesPerOutcome)
      sccsDataObjectsToCreate[[length(sccsDataObjectsToCreate) + 1]] <- list(args = args,
                                                                             sccsDataFileName = file.path(outputFolder, sccsDataFileName))
    }
  }


  # Creation of study population objects ---------------------------------------------
  analysisIds <- unlist(ParallelLogger::selectFromList(sccsAnalysisList, "analysisId"))
  uniqueStudyPopArgs <- unique(ParallelLogger::selectFromList(sccsAnalysisList, "createStudyPopulationArgs"))
  uniqueStudyPopArgs <- lapply(uniqueStudyPopArgs, function(x) return(x[[1]]))
  studyPopId <- sapply(sccsAnalysisList,
                       function(sccsAnalysis, uniqueStudyPopArgs) return(which.list(uniqueStudyPopArgs,
                                                                                    sccsAnalysis$createStudyPopulationArgs)),
                       uniqueStudyPopArgs)
  analysisIdToStudyPopId <- tibble(analysisId = analysisIds, studyPopId = studyPopId)
  referenceTable <- inner_join(referenceTable, analysisIdToStudyPopId, by = "analysisId")
  referenceTable$studyPopFile <- .createStudyPopulationFileName(loadId = referenceTable$loadId,
                                                                studyPopId = referenceTable$studyPopId,
                                                                outcomeId = referenceTable$outcomeId)

  uniqueStudyPopFiles <- unique(referenceTable$studyPopFile)
  uniqueStudyPopFiles <- uniqueStudyPopFiles[!file.exists(file.path(outputFolder, uniqueStudyPopFiles))]
  studyPopFilesToCreate <- list()
  for (studyPopFile in uniqueStudyPopFiles) {
    refRow <- referenceTable[referenceTable$studyPopFile == studyPopFile, ][1, ]
    analysisRow <- ParallelLogger::matchInList(sccsAnalysisList,
                                               list(analysisId = refRow$analysisId))[[1]]
    args <- analysisRow$createStudyPopulationArgs
    args$outcomeId <- refRow$outcomeId
    studyPopFilesToCreate[[length(studyPopFilesToCreate) + 1]] <- list(args = args,
                                                                       sccsDataFile = file.path(outputFolder, refRow$sccsDataFile),
                                                                       studyPopFile = file.path(outputFolder, refRow$studyPopFile))
  }

  # Creation of interval data objects ---------------------------------------------------
  rowId <- 1
  sccsIntervalDataObjectsToCreate <- list()
  referenceTable$sccsIntervalDataFile <- ""
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- paste("Analysis_", sccsAnalysis$analysisId, sep = "")
    if (!file.exists(file.path(outputFolder, analysisFolder)))
      dir.create(file.path(outputFolder, analysisFolder))
    for (exposureOutcome in exposureOutcomeList) {
      sccsIntervalDataFileName <- .createSccsIntervalDataFileName(analysisFolder,
                                                                  referenceTable$exposureId[rowId],
                                                                  referenceTable$outcomeId[rowId])
      referenceTable$sccsIntervalDataFile[rowId] <- sccsIntervalDataFileName
      if (!file.exists(file.path(outputFolder, sccsIntervalDataFileName))) {

        design <- sccsAnalysis$design
        sccs <- toupper(design) == "SCCS"
        if (sccs) {
          args <- sccsAnalysis$createSccsIntervalDataArgs
        } else {
          args <- sccsAnalysis$createScriIntervalDataArgs
        }
        covariateSettings <- args$eraCovariateSettings
        if (is(covariateSettings, "EraCovariateSettings"))
          covariateSettings <- list(covariateSettings)
        if (!sccs) {
          covariateSettings[[length(covariateSettings) + 1]] <- args$controlIntervalSettings
        }
        instantiatedSettings <- list()
        for (settings in covariateSettings) {
          includeEraIds <- c()
          if (length(settings$includeEraIds) != 0) {
            for (includeEraId in settings$includeEraIds) {
              if (is.character(includeEraId)) {
                if (is.null(exposureOutcome[[includeEraId]]))
                  stop(paste("Variable", includeEraId, " not found in exposure-outcome pair"))
                includeEraIds <- c(includeEraIds, exposureOutcome[[includeEraId]])
              } else {
                includeEraIds <- c(includeEraIds, includeEraId)
              }
            }
          }
          excludeEraIds <- c()
          if (length(settings$excludeEraIds) != 0) {
            for (excludeEraId in settings$excludeEraIds) {
              if (is.character(excludeEraId)) {
                if (is.null(exposureOutcome[[excludeEraId]]))
                  stop(paste("Variable", excludeEraId, " not found in exposure-outcome pair"))
                excludeEraIds <- c(excludeEraIds, exposureOutcome[[excludeEraId]])
              } else {
                excludeEraIds <- c(excludeEraIds, excludeEraId)
              }
            }
          }
          settings$includeEraIds <- includeEraIds
          settings$excludeEraIds <- excludeEraIds
          instantiatedSettings[[length(instantiatedSettings) + 1]] <- settings
        }
        if (sccs) {
          args$eraCovariateSettings <- instantiatedSettings
        } else {
          args$controlIntervalSettings <- instantiatedSettings[[length(instantiatedSettings)]]
          args$eraCovariateSettings <- instantiatedSettings[1:(length(instantiatedSettings) - 1)]
        }
        sccsDataFileName <- referenceTable$sccsDataFile[rowId]
        studyPopFile <- referenceTable$studyPopFile[rowId]
        sccsIntervalDataObjectsToCreate[[length(sccsIntervalDataObjectsToCreate) + 1]] <- list(args = args,
                                                                                               sccs = sccs,
                                                                                               sccsDataFileName = file.path(outputFolder, sccsDataFileName),
                                                                                               studyPopFile = file.path(outputFolder, studyPopFile),
                                                                                               sccsIntervalDataFileName = file.path(outputFolder, sccsIntervalDataFileName))
      }
      rowId <- rowId + 1
    }
  }

  ### Creation of model objects ###
  rowId <- 1
  sccsModelObjectsToCreate <- list()
  referenceTable$sccsModelFile <- ""
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- paste("Analysis_", sccsAnalysis$analysisId, sep = "")
    for (exposureOutcome in exposureOutcomeList) {
      sccsModelFileName <- .createSccsModelFileName(analysisFolder,
                                                    referenceTable$exposureId[rowId],
                                                    referenceTable$outcomeId[rowId])
      referenceTable$sccsModelFile[rowId] <- sccsModelFileName
      if (!file.exists(file.path(outputFolder, sccsModelFileName))) {
        args <- sccsAnalysis$fitSccsModelArgs
        args$control$threads <- cvThreads
        sccsIntervalDataFileName <- referenceTable$sccsIntervalDataFile[rowId]

        sccsModelObjectsToCreate[[length(sccsModelObjectsToCreate) + 1]] <- list(args = args,
                                                                                 sccsIntervalDataFileName = file.path(outputFolder, sccsIntervalDataFileName),
                                                                                 sccsModelFileName = file.path(outputFolder, sccsModelFileName))
      }
      rowId <- rowId + 1
    }
  }
  referenceTable$loadId <- NULL
  referenceTable$studyPopId <- NULL
  saveRDS(referenceTable, file.path(outputFolder, "outcomeModelReference.rds"))

  ### Actual construction of objects ###

  ParallelLogger::logInfo("*** Creating sccsData objects ***")
  if (length(sccsDataObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(getDbSccsDataThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsDataObjectsToCreate, createSccsDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating studyPopulation objects ***")
  if (length(studyPopFilesToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(createStudyPopulationThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, studyPopFilesToCreate, createStudyPopObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Creating sccsIntervalData objects ***")
  if (length(sccsIntervalDataObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(createSccsIntervalDataThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsIntervalDataObjectsToCreate, createSccsIntervalDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  ParallelLogger::logInfo("*** Fitting models ***")
  if (length(sccsModelObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(fitSccsModelThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsModelObjectsToCreate, createSccsModelObject)
    ParallelLogger::stopCluster(cluster)
  }

  invisible(referenceTable)
}

which.list <- function(list, object) {
  return(do.call("c", lapply(1:length(list), function(i, list, object) {
    if (identical(list[[i]], object)) return(i) else return(c())
  }, list, object)))
}

getSccsData <- function(sccsDataFile) {
  if (mget("cachedSccsDataFile", envir = globalenv(), ifnotfound = "") == sccsDataFile) {
    sccsData <- get("cachedSccsData", envir = globalenv())
    if (!Andromeda::isValidAndromeda(sccsData)) {
      sccsData <- loadSccsData(sccsDataFile)
      assign("cachedSccsData", sccsData, envir = globalenv())
    }
  } else {
    sccsData <- loadSccsData(sccsDataFile)
    assign("cachedSccsData", sccsData, envir = globalenv())
    assign("cachedSccsDataFile", sccsDataFile, envir = globalenv())
  }
  return(sccsData)
}

createSccsDataObject <- function(params) {
  sccsData <- do.call("getDbSccsData", params$args)
  saveSccsData(sccsData, params$sccsDataFileName)
  return(NULL)
}

createStudyPopObject <- function(params) {
  sccsData <- getSccsData(params$sccsDataFile)
  params$args$sccsData <- sccsData
  studyPopulation <- do.call("createStudyPopulation", params$args)
  saveRDS(studyPopulation, params$studyPopFile)
  return(NULL)
}

createSccsIntervalDataObject <- function(params) {
  sccsData <- getSccsData(params$sccsDataFileName)
  params$args$sccsData <- sccsData
  studyPopulation <- readRDS(params$studyPopFile)
  params$args$studyPopulation <- studyPopulation
  if (params$sccs) {
    sccsIntervalData <- do.call("createSccsIntervalData", params$args)
  } else {
    sccsIntervalData <- do.call("createScriIntervalData", params$args)
  }
  saveSccsIntervalData(sccsIntervalData, params$sccsIntervalDataFileName)
  return(NULL)
}

createSccsModelObject <- function(params) {
  sccsIntervalData <- loadSccsIntervalData(params$sccsIntervalDataFileName)
  params$args$sccsIntervalData <- sccsIntervalData
  # sccsModel <- do.call("fitSccsModel", params$args)
  sccsModel <- fitSccsModel(sccsIntervalData = sccsIntervalData,
                            prior = params$args$prior,
                            control = params$args$control)
  saveRDS(sccsModel, params$sccsModelFileName)
  return(NULL)
}

.createSccsDataFileName <- function(loadId) {
  name <- sprintf("SccsData_l%s.zip", loadId)
  return(name)
}

.f <- function(x) {
  return(format(x, scientific = FALSE, trim = TRUE))
}

.createStudyPopulationFileName <- function(loadId,
                                           studyPopId,
                                           outcomeId) {
  name <- sprintf("StudyPop_l%s_s%s_o%s.rds", loadId, studyPopId, .f(outcomeId))
  return(name)
}

.createSccsIntervalDataFileName <- function(analysisFolder, exposureId, outcomeId) {
  name <- sprintf("SccsIntervalData_e%s_o%s.zip", .f(exposureId), .f(outcomeId))
  return(file.path(analysisFolder, name))
}

.createSccsModelFileName <- function(analysisFolder, exposureId, outcomeId) {
  name <- sprintf("SccsModel_e%s_o%s.rds", .f(exposureId), .f(outcomeId))
  return(file.path(analysisFolder, name))
}

.selectByType <- function(type, value, label) {
  if (is.null(type)) {
    if (is.list(value)) {
      stop(paste("Multiple ",
                 label,
                 "s specified, but none selected in analyses (comparatorType).",
                 sep = ""))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

#' Create a summary report of the analyses
#'
#' @param referenceTable   A tibble as created by the [runSccsAnalyses] function.
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing summary statistics for each exposure-outcome-analysis combination.
#'
#' @export
summarizeSccsAnalyses <- function(referenceTable, outputFolder) {
  columns <- c("analysisId", "exposureId", "outcomeId")
  result <- referenceTable[, columns]
  result$outcomeSubjects <- 0
  result$outcomeEvents <- 0
  result$outcomeObsPeriods <- 0

  for (i in 1:nrow(referenceTable)) {
    sccsModel <- readRDS(file.path(outputFolder, as.character(referenceTable$sccsModelFile[i])))
    attrition <- as.data.frame(sccsModel$metaData$attrition)
    attrition <- attrition[nrow(attrition), ]
    result$outcomeSubjects[i] <- attrition$outcomeSubjects
    result$outcomeEvents[i] <- attrition$outcomeEvents
    result$outcomeObsPeriods[i] <- attrition$outcomeObsPeriods
    estimates <- sccsModel$estimates[sccsModel$estimates$originalEraId == referenceTable$exposureId[i], ]
    if (!is.null(estimates) && nrow(estimates) != 0) {
      for (j in 1:nrow(estimates)) {
        estimatesToInsert <- c(rr = exp(estimates$logRr[j]),
                               ci95lb = exp(estimates$logLb95[j]),
                               ci95ub = exp(estimates$logUb95[j]),
                               logRr = estimates$logRr[j],
                               seLogRr = estimates$seLogRr[j],
                               llr = estimates$llr)
        if (grepl(".*, day -?[0-9]+--?[0-9]*$", estimates$covariateName[j])) {
          name <- as.character(estimates$covariateName[j])
          pos1 <- attr(regexpr("^[^:]*:", name),"match.length") - 1
          pos2 <- regexpr(", day -?[0-9]+--?[0-9]*$", name) + 2
          label <- paste(substr(name, 1, pos1),
                         substr(name, pos2, nchar(name)))
        } else {
          label <- sub(":.*$", "", estimates$covariateName[j])
        }
        names(estimatesToInsert) <- paste0(names(estimatesToInsert),
                                           "(",
                                           label,
                                           ")")
        for (colName in names(estimatesToInsert)) {
          if (!(colName %in% colnames(result))) {
            result$newVar <- as.numeric(NA)
            colnames(result)[colnames(result) == "newVar"] <- colName
          }
          result[i, colName] <- estimatesToInsert[colName]
        }
      }
    }
  }
  return(result)
}
