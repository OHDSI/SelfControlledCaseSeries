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

#' Create SelfControlledCaseSeries multi-threading settings
#'
#' @param getDbSccsDataThreads             The number of parallel threads to use for building the
#'                                         `SccsData` objects.
#' @param createStudyPopulationThreads     The number of parallel threads to use for building the
#'                                         `studyPopulation` objects.
#' @param createIntervalDataThreads        The number of parallel threads to use for building the
#'                                         `SccsIntervalData` objects.
#' @param fitSccsModelThreads              The number of parallel threads to use for fitting the
#'                                         models.
#' @param cvThreads                        The number of parallel threads to use for the cross-
#'                                         validation when estimating the hyperparameter for the
#'                                         outcome model. Note that the total number of CV threads at
#'                                         one time could be `fitSccsModelThreads * cvThreads`.
#' @param calibrationThreads               The number of parallel threads to use for empirical calibration.
#'
#' @return
#' An object of type `SccsMultiThreadingSettings`.
#'
#' @seealso [createDefaultSccsMultiThreadingSettings()]
#'
#' @export
createSccsMultiThreadingSettings <- function(getDbSccsDataThreads = 1,
                                             createStudyPopulationThreads = 1,
                                             createIntervalDataThreads = 1,
                                             fitSccsModelThreads = 1,
                                             cvThreads = 1,
                                             calibrationThreads = 1) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(getDbSccsDataThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(createStudyPopulationThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(createIntervalDataThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(fitSccsModelThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(cvThreads, lower = 1, add = errorMessages)
  checkmate::assertInt(calibrationThreads, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  settings <- list()
  for (name in names(formals(createSccsMultiThreadingSettings))) {
    settings[[name]] <- get(name)
  }
  class(settings) <- "SccsMultiThreadingSettings"
  return(settings)
}

#' Create default SelfControlledCaseSeries multi-threading settings
#'
#' @description
#' Create SelfControlledCaseSeries multi-threading settings based on the maximum
#' number of cores to be used.
#'
#' @param maxCores  Maximum number of CPU cores to use.
#'
#' @return
#' An object of type `SccsMultiThreadingSettings`.
#'
#' @seealso [createSccsMultiThreadingSettings()]
#'
#' @examples
#' settings <- createDefaultSccsMultiThreadingSettings(10)
#'
#' @export
createDefaultSccsMultiThreadingSettings <- function(maxCores) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertInt(maxCores, lower = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  settings <- createSccsMultiThreadingSettings(
    getDbSccsDataThreads = min(3, maxCores),
    createStudyPopulationThreads = min(3, maxCores),
    createIntervalDataThreads = min(3, maxCores),
    fitSccsModelThreads = min(5, max(1, floor(maxCores / 5))),
    cvThreads = min(5, maxCores),
    calibrationThreads = min(4, maxCores)
  )
  return(settings)
}

#' Run a list of analyses
#'
#' @details
#' Run a list of analyses for the exposures-outcomes of interest. This function will run all
#' specified analyses against all hypotheses of interest, meaning that the total number of outcome
#' models is `length(sccsAnalysisList) * length(exposuresOutcomeList)` When you provide several analyses
#' it will determine whether any of the analyses have anything in common, and will take advantage of
#' this fact.
#'
#' ## Analyses to Exclude
#'
#' Normally, `runSccsAnalyses` will run all combinations of exposures-outcome-analyses settings.
#' However, sometimes we may not need all those combinations. Using the `analysesToExclude` argument,
#' we can remove certain items from the full matrix. This argument should be a data frame with at least
#' one of the following columns:
#'
#' - exposureId
#' - outcomeId
#' - analysisId
#'
#' This data frame will be joined to the outcome model reference table before executing, and matching rows
#' will be removed. For example, if one specifies only one exposure ID and analysis ID, then any analyses with
#' that exposure and that analysis ID will be skipped.
#'
#' @param connectionDetails                An R object of type `ConnectionDetails` created using
#'                                         the function [DatabaseConnector::createConnectionDetails()].
#' @param cdmDatabaseSchema                The name of the database schema that contains the OMOP CDM
#'                                         instance.  Requires read permissions to this database. On
#'                                         SQL Server, this should specify both the database and the
#'                                         schema, so for example 'cdm_instance.dbo'.
#' @param tempEmulationSchema              Some database platforms like Oracle and Impala do not truly support
#'                                         temp tables. To emulate temp tables, provide a schema with write
#'                                         privileges where temp tables can be created.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. Requires
#'                                         read permissions to this database.
#' @param outcomeTable                     The table name that contains the outcome cohorts.
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
#'                                         should have the same structure as the cohort table.
#' @param cdmVersion                       Define the OMOP CDM version used: currently supports "5".
#' @param sccsAnalysisList                 A list of objects of `SccsAnalysis` as created
#'                                         using the [createSccsAnalysis()] function.
#' @param exposuresOutcomeList             A list of objects of type `ExposuresOutcome` as created
#'                                         using the [createExposuresOutcome()] function.
#' @param outputFolder                     Name of the folder where all the outputs will written to.
#' @param combineDataFetchAcrossOutcomes   Should fetching data from the database be done one outcome
#'                                         at a time, or for all outcomes in one fetch? Combining
#'                                         fetches will be more efficient if there is large overlap in
#'                                         the subjects that have the different outcomes.
#' @param analysesToExclude                Analyses to exclude. See the Analyses to Exclude section for
#'                                         details.
#' @param sccsMultiThreadingSettings       An object of type `SccsMultiThreadingSettings` as created using
#'                                         the [createSccsMultiThreadingSettings()] or
#'                                         [createDefaultSccsMultiThreadingSettings()] functions.
#'
#' @return
#' A tibble describing for each exposure-outcome-analysisId combination where the intermediary and
#' outcome model files can be found, relative to the `outputFolder`.
#'
#' @export
runSccsAnalyses <- function(connectionDetails,
                            cdmDatabaseSchema,
                            tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                            exposureDatabaseSchema = cdmDatabaseSchema,
                            exposureTable = "drug_era",
                            outcomeDatabaseSchema = cdmDatabaseSchema,
                            outcomeTable = "cohort",
                            customCovariateDatabaseSchema = cdmDatabaseSchema,
                            customCovariateTable = "cohort",
                            nestingCohortDatabaseSchema = cdmDatabaseSchema,
                            nestingCohortTable = "cohort",
                            cdmVersion = "5",
                            outputFolder = "./SccsOutput",
                            sccsAnalysisList,
                            exposuresOutcomeList,
                            analysesToExclude = NULL,
                            combineDataFetchAcrossOutcomes = FALSE,
                            sccsMultiThreadingSettings = createSccsMultiThreadingSettings()) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertClass(connectionDetails, "connectionDetails", add = errorMessages)
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(exposureDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(customCovariateDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(customCovariateTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(nestingCohortDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(nestingCohortTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(cdmVersion, len = 1, add = errorMessages)
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::assertList(sccsAnalysisList, min.len = 1, add = errorMessages)
  for (i in 1:length(sccsAnalysisList)) {
    checkmate::assertClass(sccsAnalysisList[[i]], "SccsAnalysis", add = errorMessages)
  }
  checkmate::assertList(exposuresOutcomeList, min.len = 1, add = errorMessages)
  for (i in 1:length(exposuresOutcomeList)) {
    checkmate::assertClass(exposuresOutcomeList[[i]], "ExposuresOutcome", add = errorMessages)
  }
  checkmate::assertDataFrame(analysesToExclude, null.ok = TRUE, add = errorMessages)
  checkmate::assertLogical(combineDataFetchAcrossOutcomes, len = 1, add = errorMessages)
  checkmate::assertClass(sccsMultiThreadingSettings, "SccsMultiThreadingSettings", add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  uniqueExposuresOutcomeList <- unique(lapply(lapply(lapply(exposuresOutcomeList, unlist), as.character), paste, collapse = " "))
  if (length(uniqueExposuresOutcomeList) != length(exposuresOutcomeList)) {
    stop("Duplicate exposure-outcomes pairs are not allowed")
  }
  uniqueAnalysisIds <- unlist(unique(ParallelLogger::selectFromList(sccsAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(sccsAnalysisList)) {
    stop("Duplicate analysis IDs are not allowed")
  }

  if (!file.exists(outputFolder)) {
    dir.create(outputFolder)
  }

  referenceTable <- createReferenceTable(
    sccsAnalysisList,
    exposuresOutcomeList,
    outputFolder,
    combineDataFetchAcrossOutcomes,
    analysesToExclude
  )

  loadConceptsPerLoad <- attr(referenceTable, "loadConceptsPerLoad")

  # Create arguments for sccsData objects ----------------------------
  sccsDataObjectsToCreate <- list()
  for (sccsDataFileName in unique(referenceTable$sccsDataFile)) {
    if (!file.exists(file.path(outputFolder, sccsDataFileName))) {
      referenceRow <- referenceTable %>%
        filter(.data$sccsDataFile == sccsDataFileName) %>%
        head(1)

      loadConcepts <- loadConceptsPerLoad[[referenceRow$loadId]]
      if (length(loadConcepts$exposureIds) == 1 && loadConcepts$exposureIds[1] == "all") {
        loadConcepts$exposureIds <- c()
      }
      useCustomCovariates <- (length(loadConcepts$customCovariateIds) > 0)
      if (length(loadConcepts$customCovariateIds) == 1 && loadConcepts$customCovariateIds[1] == "all") {
        loadConcepts$customCovariateIds <- c()
      }
      args <- list(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = cdmDatabaseSchema,
        tempEmulationSchema = tempEmulationSchema,
        exposureDatabaseSchema = exposureDatabaseSchema,
        exposureTable = exposureTable,
        outcomeDatabaseSchema = outcomeDatabaseSchema,
        outcomeTable = outcomeTable,
        customCovariateDatabaseSchema = customCovariateDatabaseSchema,
        customCovariateTable = customCovariateTable,
        nestingCohortDatabaseSchema = nestingCohortDatabaseSchema,
        nestingCohortTable = nestingCohortTable,
        cdmVersion = cdmVersion,
        exposureIds = loadConcepts$exposureIds,
        outcomeIds = loadConcepts$outcomeIds,
        useCustomCovariates = useCustomCovariates,
        customCovariateIds = loadConcepts$customCovariateIds,
        useNestingCohort = loadConcepts$nestingCohortId != -1,
        nestingCohortId = loadConcepts$nestingCohortId,
        deleteCovariatesSmallCount = loadConcepts$deleteCovariatesSmallCount,
        studyStartDate = loadConcepts$studyStartDate,
        studyEndDate = loadConcepts$studyEndDate,
        maxCasesPerOutcome = loadConcepts$maxCasesPerOutcome
      )
      sccsDataObjectsToCreate[[length(sccsDataObjectsToCreate) + 1]] <- list(
        args = args,
        sccsDataFileName = file.path(outputFolder, sccsDataFileName)
      )
    }
  }

  # Create arguments for study population objects ---------------------------------------------
  uniqueStudyPopFiles <- unique(referenceTable$studyPopFile)
  uniqueStudyPopFiles <- uniqueStudyPopFiles[!file.exists(file.path(outputFolder, uniqueStudyPopFiles))]
  studyPopFilesToCreate <- list()
  for (studyPopFile in uniqueStudyPopFiles) {
    refRow <- referenceTable[referenceTable$studyPopFile == studyPopFile, ][1, ]
    analysisRow <- ParallelLogger::matchInList(
      sccsAnalysisList,
      list(analysisId = refRow$analysisId)
    )[[1]]
    args <- analysisRow$createStudyPopulationArgs
    args$outcomeId <- refRow$outcomeId
    studyPopFilesToCreate[[length(studyPopFilesToCreate) + 1]] <- list(
      args = args,
      sccsDataFile = file.path(outputFolder, refRow$sccsDataFile),
      studyPopFile = file.path(outputFolder, refRow$studyPopFile)
    )
  }

  # Create arguments for interval data objects ---------------------------------------------------
  sccsIntervalDataFiles <- referenceTable$sccsIntervalDataFile
  sccsIntervalDataFiles <- sccsIntervalDataFiles[!file.exists(file.path(outputFolder, sccsIntervalDataFiles))]
  sccsIntervalDataObjectsToCreate <- list()
  for (sccsIntervalDataFile in sccsIntervalDataFiles) {
    refRow <- referenceTable[referenceTable$sccsIntervalDataFile == sccsIntervalDataFile, ][1, ]
    analysisRow <- ParallelLogger::matchInList(
      sccsAnalysisList,
      list(analysisId = refRow$analysisId)
    )[[1]]
    sccs <- (!"controlIntervalSettings" %in% names(analysisRow$createIntervalDataArgs))
    args <- analysisRow$createIntervalDataArgs
    covariateSettings <- args$eraCovariateSettings
    if (is(covariateSettings, "EraCovariateSettings")) {
      covariateSettings <- list(covariateSettings)
    }
    if (!sccs) {
      covariateSettings[[length(covariateSettings) + 1]] <- args$controlIntervalSettings
    }
    instantiatedSettings <- list()
    for (settings in covariateSettings) {
      includeEraIds <- c()
      if (length(settings$includeEraIds) != 0) {
        for (includeEraId in settings$includeEraIds) {
          if (is.character(includeEraId)) {
            if (is.null(refRow[[includeEraId]])) {
              stop(paste("Variable", includeEraId, " not found in exposures-outcome set"))
            }
            includeEraIds <- c(includeEraIds, refRow[[includeEraId]])
          } else {
            includeEraIds <- c(includeEraIds, includeEraId)
          }
        }
      }
      excludeEraIds <- c()
      if (length(settings$excludeEraIds) != 0) {
        for (excludeEraId in settings$excludeEraIds) {
          if (is.character(excludeEraId)) {
            if (is.null(refRow[[excludeEraId]])) {
              stop(paste("Variable", excludeEraId, " not found in exposure-outcome set"))
            }
            excludeEraIds <- c(excludeEraIds, refRow[[excludeEraId]])
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
    sccsDataFileName <- refRow$sccsDataFile
    studyPopFile <- refRow$studyPopFile
    sccsIntervalDataObjectsToCreate[[length(sccsIntervalDataObjectsToCreate) + 1]] <- list(
      args = args,
      sccs = sccs,
      sccsDataFileName = file.path(outputFolder, sccsDataFileName),
      studyPopFile = file.path(outputFolder, studyPopFile),
      sccsIntervalDataFileName = file.path(outputFolder, sccsIntervalDataFile)
    )
  }

  # Create arguments for model objects ---------------------------------------------
  sccsModelFiles <- referenceTable$sccsModelFile
  sccsModelFiles <- sccsModelFiles[!file.exists(file.path(outputFolder, sccsModelFiles))]
  sccsModelObjectsToCreate <- list()
  for (sccsModelFile in sccsModelFiles) {
    refRow <- referenceTable[referenceTable$sccsModelFile == sccsModelFile, ][1, ]
    analysisRow <- ParallelLogger::matchInList(
      sccsAnalysisList,
      list(analysisId = refRow$analysisId)
    )[[1]]
    args <- analysisRow$fitSccsModelArgs
    args$control$threads <- sccsMultiThreadingSettings$cvThreads
    sccsModelObjectsToCreate[[length(sccsModelObjectsToCreate) + 1]] <- list(
      args = args,
      sccsIntervalDataFileName = file.path(outputFolder, refRow$sccsIntervalDataFile),
      sccsModelFileName = file.path(outputFolder, sccsModelFile)
    )
  }

  referenceTable$loadId <- NULL
  referenceTable$studyPopId <- NULL
  attr(referenceTable, "loadConcepts") <- NULL
  saveRDS(referenceTable, file.path(outputFolder, "outcomeModelReference.rds"))
  saveRDS(sccsAnalysisList, file.path(outputFolder, "sccsAnalysisList.rds"))
  saveRDS(exposuresOutcomeList, file.path(outputFolder, "exposuresOutcomeList.rds"))

  # Construction of objects -------------------------------------------------------------------------
  if (length(sccsDataObjectsToCreate) != 0) {
    message("*** Creating sccsData objects ***")
    cluster <- ParallelLogger::makeCluster(min(length(sccsDataObjectsToCreate), sccsMultiThreadingSettings$getDbSccsDataThreads))
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsDataObjectsToCreate, createSccsDataObject)
    ParallelLogger::stopCluster(cluster)
  }


  if (length(studyPopFilesToCreate) != 0) {
    message("*** Creating studyPopulation objects ***")
    cluster <- ParallelLogger::makeCluster(min(length(studyPopFilesToCreate), sccsMultiThreadingSettings$createStudyPopulationThreads))
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, studyPopFilesToCreate, createStudyPopObject)
    ParallelLogger::stopCluster(cluster)
  }

  if (length(sccsIntervalDataObjectsToCreate) != 0) {
    message("*** Creating sccsIntervalData objects ***")
    cluster <- ParallelLogger::makeCluster(min(length(sccsIntervalDataObjectsToCreate), sccsMultiThreadingSettings$createIntervalDataThreads))
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsIntervalDataObjectsToCreate, createSccsIntervalDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  if (length(sccsModelObjectsToCreate) != 0) {
    message("*** Fitting models ***")
    cluster <- ParallelLogger::makeCluster(min(length(sccsModelObjectsToCreate), sccsMultiThreadingSettings$fitSccsModelThreads))
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsModelObjectsToCreate, createSccsModelObject)
    ParallelLogger::stopCluster(cluster)
  }

  mainFileName <- file.path(outputFolder, "resultsSummary.rds")
  if (!file.exists(mainFileName)) {
    message("*** Summarizing results ***")
    summarizeResults(
      referenceTable = referenceTable,
      exposuresOutcomeList = exposuresOutcomeList,
      outputFolder = outputFolder,
      mainFileName = mainFileName,
      calibrationThreads = sccsMultiThreadingSettings$calibrationThreads
    )
  }

  invisible(referenceTable)
}

createReferenceTable <- function(sccsAnalysisList,
                                 exposuresOutcomeList,
                                 outputFolder,
                                 combineDataFetchAcrossOutcomes,
                                 analysesToExclude) {
  convertAnalysisToTable <- function(analysis) {
    tibble(
      analysisId = analysis$analysisId,
      analysisFolder = sprintf("Analysis_%d", analysis$analysisId)
    )
  }
  analyses <- bind_rows(lapply(sccsAnalysisList, convertAnalysisToTable))
  foldersToCreate <- file.path(outputFolder, analyses$analysisFolder)
  foldersToCreate <- foldersToCreate[!dir.exists(foldersToCreate)]
  sapply(foldersToCreate, dir.create)

  extractExposureIdRefs <- function(exposuresOutcome) {
    return(unlist(ParallelLogger::selectFromList(exposuresOutcome$exposures, "exposureIdRef")))
  }
  uniqueExposureIdRefs <- unique(unlist(sapply(exposuresOutcomeList, extractExposureIdRefs)))

  convertExposuresOutcomeToTable <- function(i) {
    exposuresOutcome <- exposuresOutcomeList[[i]]
    # names <- c("exposuresOutcomeSetId", "outcomeId", uniqueExposureIdRefs, sprintf("%sTrueEffectSize", uniqueExposureIdRefs))
    # values <- c(i, exposuresOutcome$outcomeId, rep(-1, length(uniqueExposureIdRefs)), rep(NA, length(uniqueExposureIdRefs)))
    # for (exposure in exposuresOutcome$exposures) {
    #   idx <- which(uniqueExposureIdRefs == exposure$exposureIdRef)
    #   values[idx + 1] <- exposure$exposureId
    #   if (!is.null(exposure$trueEffectSize)) {
    #     values[idx + 1 + length(uniqueExposureIdRefs)] <- exposure$trueEffectSize
    #   }
    # }
    names <- c("exposuresOutcomeSetId", "outcomeId", uniqueExposureIdRefs)
    values <- c(i, exposuresOutcome$outcomeId, rep(-1, length(uniqueExposureIdRefs)))
    for (exposure in exposuresOutcome$exposures) {
      idx <- which(uniqueExposureIdRefs == exposure$exposureIdRef)
      values[idx + 2] <- exposure$exposureId
    }
    names(values) <- names
    as_tibble(t(values)) %>%
      return()
  }
  eos <- bind_rows(lapply(seq_along(exposuresOutcomeList), convertExposuresOutcomeToTable))

  referenceTable <- eos %>%
    inner_join(analyses, by = character())

  # Determine if loading calls can be combined for efficiency ----------------------------

  # Instantiate loading settings per row in the reference table
  instantiatedArgsPerRow <- vector(mode = "list", length = nrow(referenceTable))
  for (sccsAnalysis in sccsAnalysisList) {
    idx <- which(referenceTable$analysisId == sccsAnalysis$analysisId)
    for (i in idx) {
      exposureIds <- c()
      if (is.null(sccsAnalysis$getDbSccsDataArgs$exposureIds)) {
        exposureIds <- "all"
      } else {
        for (exposureId in sccsAnalysis$getDbSccsDataArgs$exposureIds) {
          if (is.character(exposureId)) {
            if (!exposureId %in% uniqueExposureIdRefs) {
              stop(paste("Variable", exposureId, " not found in exposures-outcome sets"))
            }
            exposureIds <- c(exposureIds, referenceTable[i, ]$exposureId)
          } else {
            exposureIds <- c(exposureIds, as.numeric(exposureId))
          }
        }
      }
      customCovariateIds <- c()
      if (sccsAnalysis$getDbSccsDataArgs$useCustomCovariates) {
        if (is.null(sccsAnalysis$getDbSccsDataArgs$customCovariateIds)) {
          customCovariateIds <- "all"
        } else {
          for (customCovariateId in sccsAnalysis$getDbSccsDataArgs$customCovariateIds) {
            if (is.character(customCovariateId)) {
              if (!customCovariateId %in% uniqueExposureIdRefs) {
                stop(paste("Variable", customCovariateId, " not found in exposures-outcome sets"))
              }
              customCovariateIds <- c(customCovariateIds, referenceTable[i, customCovariateId])
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
      instantiatedArgs <- sccsAnalysis$getDbSccsDataArgs
      instantiatedArgs$outcomeId <- referenceTable$outcomeId[i]
      instantiatedArgs$exposureIds <- exposureIds
      instantiatedArgs$customCovariateIds <- customCovariateIds
      instantiatedArgs$nestingCohortId <- nestingCohortId
      instantiatedArgs$rowId <- i
      instantiatedArgsPerRow[[i]] <- instantiatedArgs
    }
  }

  # Group loads where possible
  if (combineDataFetchAcrossOutcomes) {
    uniqueLoads <- unique(ParallelLogger::selectFromList(
      instantiatedArgsPerRow,
      c(
        "nestingCohortId",
        "deleteCovariatesSmallCount",
        "studyStartDate",
        "studyEndDate",
        "maxCasesPerOutcome"
      )
    ))
  } else {
    uniqueLoads <- unique(ParallelLogger::selectFromList(
      instantiatedArgsPerRow,
      c(
        "nestingCohortId",
        "deleteCovariatesSmallCount",
        "studyStartDate",
        "studyEndDate",
        "maxCasesPerOutcome",
        "outcomeId"
      )
    ))
  }

  # Compute unions of concept sets
  referenceTable$sccsDataFile <- ""
  referenceTable$loadId <- NA
  loadConceptsPerLoad <- list()
  for (loadId in 1:length(uniqueLoads)) {
    uniqueLoad <- uniqueLoads[[loadId]]
    groupables <- ParallelLogger::matchInList(instantiatedArgsPerRow, uniqueLoad)
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
    loadConceptsPerLoad[[loadId]] <- list(
      exposureIds = unique(exposureIds),
      outcomeIds = unique(outcomeIds),
      customCovariateIds = unique(customCovariateIds),
      nestingCohortId = groupables[[1]]$nestingCohortId,
      deleteCovariatesSmallCount = groupables[[1]]$deleteCovariatesSmallCount,
      studyStartDate = groupables[[1]]$studyStartDate,
      studyEndDate = groupables[[1]]$studyEndDate,
      maxCasesPerOutcome = groupables[[1]]$maxCasesPerOutcome
    )
    sccsDataFileName <- .createSccsDataFileName(loadId)
    referenceTable$loadId[rowIds] <- loadId
    referenceTable$sccsDataFile[rowIds] <- sccsDataFileName
  }
  attr(referenceTable, "loadConceptsPerLoad") <- loadConceptsPerLoad

  # Add study population filenames --------------------------
  analysisIds <- unlist(ParallelLogger::selectFromList(sccsAnalysisList, "analysisId"))
  uniqueStudyPopArgs <- unique(ParallelLogger::selectFromList(sccsAnalysisList, "createStudyPopulationArgs"))
  uniqueStudyPopArgs <- lapply(uniqueStudyPopArgs, function(x) {
    return(x[[1]])
  })
  studyPopId <- sapply(
    sccsAnalysisList,
    function(sccsAnalysis, uniqueStudyPopArgs) {
      return(which.list(
        uniqueStudyPopArgs,
        sccsAnalysis$createStudyPopulationArgs
      ))
    },
    uniqueStudyPopArgs
  )
  analysisIdToStudyPopId <- tibble(analysisId = analysisIds, studyPopId = studyPopId)
  referenceTable <- inner_join(referenceTable, analysisIdToStudyPopId, by = "analysisId")
  referenceTable$studyPopFile <- .createStudyPopulationFileName(
    loadId = referenceTable$loadId,
    studyPopId = referenceTable$studyPopId,
    outcomeId = referenceTable$outcomeId
  )

  # Add interval data and model filenames -----------------------------------------------------
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- paste("Analysis_", sccsAnalysis$analysisId, sep = "")
    if (!file.exists(file.path(outputFolder, analysisFolder))) {
      dir.create(file.path(outputFolder, analysisFolder))
    }
  }

  generateFileName <- function(i) {
    return(.createSccsIntervalDataFileName(
      paste("Analysis_", referenceTable$analysisId[i], sep = ""),
      referenceTable$exposureId[i],
      referenceTable$outcomeId[i]
    ))
  }
  referenceTable$sccsIntervalDataFile <- generateFileName(1:nrow(referenceTable))

  generateFileName <- function(i) {
    return(.createSccsModelFileName(
      paste("Analysis_", referenceTable$analysisId[i], sep = ""),
      referenceTable$exposureId[i],
      referenceTable$outcomeId[i]
    ))
  }
  referenceTable$sccsModelFile <- generateFileName(1:nrow(referenceTable))

  # Remove rows that the user specified to exclude ---------------------------------
  if (!is.null(analysesToExclude)) {
    matchingColumns <- colnames(analysesToExclude)[colnames(analysesToExclude) %in% c("exposureId", "outcomeId", "analysisId")]
    if (length(matchingColumns) == 0) {
      stop("The 'analysesToExclude' argument should contain columns 'exposureId', 'outcomeId', or 'analysisId'.")
    }
    analysesToExclude <- analysesToExclude[, matchingColumns]
    countBefore <- nrow(referenceTable)
    referenceTable <- referenceTable %>%
      anti_join(analysesToExclude, by = matchingColumns)
    countAfter <- nrow(referenceTable)
    message(sprintf(
      "Removed %d of the %d exposure-outcome-analysis combinations as specified by the user.",
      countBefore - countAfter,
      countBefore
    ))
  }

  return(referenceTable)
}

which.list <- function(list, object) {
  return(do.call("c", lapply(1:length(list), function(i, list, object) {
    if (identical(list[[i]], object)) {
      return(i)
    } else {
      return(c())
    }
  }, list, object)))
}

getSccsData <- function(sccsDataFile) {
  if (mget("cachedSccsDataFile", envir = cache, ifnotfound = "") == sccsDataFile) {
    sccsData <- get("cachedSccsData", envir = cache)
    if (!Andromeda::isValidAndromeda(sccsData)) {
      sccsData <- loadSccsData(sccsDataFile)
      assign("cachedSccsData", sccsData, envir = cache)
    }
  } else {
    sccsData <- loadSccsData(sccsDataFile)
    assign("cachedSccsData", sccsData, envir = cache)
    assign("cachedSccsDataFile", sccsDataFile, envir = cache)
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
  sccsModel <- fitSccsModel(
    sccsIntervalData = sccsIntervalData,
    prior = params$args$prior,
    control = params$args$control
  )
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
                 sep = ""
      ))
    }
    return(value)
  } else {
    if (!is.list(value) || is.null(value[type])) {
      stop(paste(label, "type not found:", type))
    }
    return(value[type])
  }
}

summarizeResults <- function(referenceTable, exposuresOutcomeList, outputFolder, mainFileName, calibrationThreads = 1) {
  rows <- list()
  # i = 1
  pb <- txtProgressBar(style = 3)
  for (i in seq_len(nrow(referenceTable))) {
    refRow <- referenceTable[i, ]
    sccsModel <- readRDS(file.path(outputFolder, as.character(refRow$sccsModelFile)))
    attrition <- as.data.frame(sccsModel$metaData$attrition)
    attrition <- attrition[nrow(attrition), ]
    # covariateSettings = sccsModel$metaData$covariateSettingsList[[1]]
    for (covariateSettings in sccsModel$metaData$covariateSettingsList) {
      if (covariateSettings$exposureOfInterest) {
        # j = 1
        for (j in seq_along(covariateSettings$outputIds)) {
          if (is.null(sccsModel$metaData$covariateStatistics)) {
            covariateStatistics <- tibble()
          } else {
            covariateStatistics <- sccsModel$metaData$covariateStatistics %>%
              filter(.data$covariateId == covariateSettings$outputIds[j])
          }
          if (is.null(sccsModel$estimates)) {
            estimate <- tibble()
          } else {
            estimate <- sccsModel$estimates %>%
              filter(.data$covariateId == covariateSettings$outputIds[j])
          }
          if (nrow(estimate) == 0) {
            p <- NA
          } else {
            z <- estimate$logRr / estimate$seLogRr
            p <- 2 * pmin(pnorm(z), 1 - pnorm(z))
          }
          if (covariateSettings$eraIds[j] == -1) {
            exposure <- list(trueEffectSize = NA)
          } else {
            exposuresOutcome <- exposuresOutcomeList[[refRow$exposuresOutcomeSetId]]
            exposure <- ParallelLogger::matchInList(exposuresOutcome$exposures, list(exposureId = covariateSettings$eraIds[j]))
            if (length(exposure) != 1) {
              stop(sprintf("Error finding exposure for covariate ID %d in analysis ID %d",  covariateSettings$outputIds[j], refRow$analysisId))
            } else {
              exposure <- exposure[[1]]
            }
          }
          row <- tibble(
            exposuresOutcomeSetId = refRow$exposuresOutcomeSetId,
            outcomeId = refRow$outcomeId,
            analysisId = refRow$analysisId,
            covariateAnalysisId = covariateSettings$covariateAnalysisId,
            covariateId = covariateSettings$outputIds[j],
            covariateName = covariateSettings$label,
            eraId = covariateSettings$eraIds[j],
            trueEffectSize = exposure$trueEffectSize,
            outcomeSubjects = attrition$outcomeSubjects,
            outcomeEvents = attrition$outcomeEvents,
            outcomeObservationPeriods = attrition$outcomeObsPeriods,
            covariateSubjects = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$personCount),
            covariateDays = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$dayCount),
            covariateEras = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$eraCount),
            covariateOutcomes = ifelse(nrow(covariateStatistics) == 0, 0, covariateStatistics$outcomeCount),
            observedDays = sccsModel$metaData$daysObserved,
            rr = ifelse(nrow(estimate) == 0, NA, exp(estimate$logRr)),
            ci95Lb = ifelse(nrow(estimate) == 0, NA, exp(estimate$logLb95)),
            ci95Ub = ifelse(nrow(estimate) == 0, NA, exp(estimate$logUb95)),
            p = p,
            logRr = ifelse(nrow(estimate) == 0, NA, estimate$logRr),
            seLogRr = ifelse(nrow(estimate) == 0, NA, estimate$seLogRr),
            llr = ifelse(nrow(estimate) == 0, NA, estimate$llr)
          )
          rows[[length(rows) + 1]] <- row
        }
      }
    }
    setTxtProgressBar(pb, i / nrow(referenceTable))
  }
  close(pb)
  mainResults <- bind_rows(rows)
  mainResults <- calibrateEstimates(
    results = mainResults,
    calibrationThreads = calibrationThreads
  )
  saveRDS(mainResults, mainFileName)
}

calibrateEstimates <- function(results, calibrationThreads) {
  if (nrow(results) == 0) {
    return(results)
  }
  message("Calibrating estimates")
  groups <- split(results, paste(results$covariateId, results$analysisId))

  cluster <- ParallelLogger::makeCluster(min(length(groups), calibrationThreads))
  results <- ParallelLogger::clusterApply(cluster, groups, calibrateGroup)
  ParallelLogger::stopCluster(cluster)
  results <- bind_rows(results)
  return(results)
}

# group = groups[[1]]
calibrateGroup <- function(group) {
  ncs <- group[group$trueEffectSize == 1 & !is.na(group$seLogRr), ]
  pcs <- group[!is.na(group$trueEffectSize) & group$trueEffectSize != 1 & !is.na(group$seLogRr), ]
  if (nrow(ncs) >= 5) {
    null <- EmpiricalCalibration::fitMcmcNull(logRr = ncs$logRr, seLogRr = ncs$seLogRr)
    ease <- EmpiricalCalibration::computeExpectedAbsoluteSystematicError(null)
    calibratedP <- EmpiricalCalibration::calibrateP(null = null, logRr = group$logRr, seLogRr = group$seLogRr)
    if (nrow(pcs) >= 5) {
      model <- EmpiricalCalibration::fitSystematicErrorModel(
        logRr = c(ncs$logRr, pcs$logRr),
        seLogRr = c(ncs$seLogRr, pcs$seLogRr),
        trueLogRr = log(c(ncs$trueEffectSize, pcs$trueEffectSize)),
        estimateCovarianceMatrix = FALSE
      )
    } else {
      model <- EmpiricalCalibration::convertNullToErrorModel(null)
    }
    calibratedCi <- EmpiricalCalibration::calibrateConfidenceInterval(model = model, logRr = group$logRr, seLogRr = group$seLogRr)
    group$calibratedRr <- exp(calibratedCi$logRr)
    group$calibratedCi95Lb <- exp(calibratedCi$logLb95Rr)
    group$calibratedCi95Ub <- exp(calibratedCi$logUb95Rr)
    group$calibratedP <- calibratedP$p
    group$calibratedLogRr <- calibratedCi$logRr
    group$calibratedSeLogRr <- calibratedCi$seLogRr
    group$ease <- ease$ease
  } else {
    group$calibratedRr <- NA
    group$calibratedCi95Lb <- NA
    group$calibratedCi95Ub <- NA
    group$calibratedP <- NA
    group$calibratedLogRr <- NA
    group$calibratedSeLogRr <- NA
    group$ease <- NA
  }
  return(group)
}


#' Get file reference
#'
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing the names of the files for various artifacts created for each outcome-exposures set.
#'
#' @export
getFileReference <- function(outputFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  outputFolder <- normalizePath(outputFolder)
  omr <- readRDS(file.path(outputFolder, "outcomeModelReference.rds"))
  return(omr)
}

#' Get a summary report of the analyses results
#'
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A tibble containing summary statistics for each outcome-covariate-analysis combination.
#'
#' @export
getResultsSummary <- function(outputFolder) {
  errorMessages <- checkmate::makeAssertCollection()
  checkmate::assertCharacter(outputFolder, len = 1, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)
  outputFolder <- normalizePath(outputFolder)
  results <- readRDS(file.path(outputFolder, "resultsSummary.rds"))
  return(results)
}
