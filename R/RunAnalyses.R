# @file RunAnalyses.R
#
# Copyright 2018 Observational Health Data Sciences and Informatics
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
#' @param connectionDetails                An R object of type \code{ConnectionDetails} created using
#'                                         the function \code{createConnectionDetails} in the
#'                                         \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                The name of the database schema that contains the OMOP CDM
#'                                         instance.  Requires read permissions to this database. On
#'                                         SQL Server, this should specifiy both the database and the
#'                                         schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                 A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema            The name of the database schema that is the location where
#'                                         the data used to define the outcome cohorts is available. If
#'                                         outcomeTable = CONDITION_ERA, outcomeDatabaseSchema is not
#'                                         used.  Requires read permissions to this database.
#' @param outcomeTable                     The tablename that contains the outcome cohorts.  If
#'                                         outcomeTable is not CONDITION_OCCURRENCE or CONDITION_ERA,
#'                                         then expectation is outcomeTable has format of COHORT table:
#'                                         COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                         COHORT_END_DATE.
#' @param exposureDatabaseSchema           The name of the database schema that is the location where
#'                                         the exposure data used to define the exposure cohorts is
#'                                         available. If exposureTable = DRUG_ERA,
#'                                         exposureDatabaseSchema is not used but assumed to be
#'                                         cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                    The tablename that contains the exposure cohorts.  If
#'                                         exposureTable <> DRUG_ERA, then expectation is exposureTable
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
#' @param sccsAnalysisList                 A list of objects of type \code{sccsAnalysis} as created
#'                                         using the \code{\link{createSccsAnalysis}} function.
#' @param exposureOutcomeList              A list of objects of type \code{exposureOutcome} as created
#'                                         using the \code{\link{createExposureOutcome}} function.
#' @param outputFolder                     Name of the folder where all the outputs will written to.
#' @param combineDataFetchAcrossOutcomes   Should fetching data from the database be done one outcome
#'                                         at a time, or for all outcomes in one fetch? Combining
#'                                         fetches will be more efficient if there is large overlap in
#'                                         the subjects that have the different outcomes.
#' @param compressSccsEraDataFiles         Should compression be used when saving?
#' @param getDbSccsDataThreads             The number of parallel threads to use for building the
#'                                         sccsData objects.
#' @param createSccsEraDataThreads         The number of parallel threads to use for building the
#'                                         sccsEraData objects.
#' @param fitSccsModelThreads              The number of parallel threads to use for fitting the
#'                                         models.
#' @param cvThreads                        The number of parallel threads to use for the cross-
#'                                         validation when estimating the hyperparameter for the
#'                                         outcome model. Note that the total number of CV threads at
#'                                         one time could be `fitSccsModelThreads * cvThreads`.
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{exposureId} \tab The ID of the target drug.\cr
#' \verb{outcomeId} \tab The ID of the outcome.\cr \verb{sccsDataFolder} \tab The folder where the
#' sccsData object is stored.\cr \verb{sccsEraDataFolder} \tab The folder where the sccsEraData object
#' is stored.\cr \verb{sccsModelFile} \tab The file where the fitted SCCS model is stored.\cr }
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
                            compressSccsEraDataFiles = FALSE,
                            getDbSccsDataThreads = 1,
                            createSccsEraDataThreads = 1,
                            fitSccsModelThreads = 1,
                            cvThreads = 1) {
  for (exposureOutcome in exposureOutcomeList) stopifnot(class(exposureOutcome) == "exposureOutcome")
  for (sccsAnalysis in sccsAnalysisList) stopifnot(class(sccsAnalysis) == "sccsAnalysis")
  uniqueExposureOutcomeList <- unique(ParallelLogger::selectFromList(exposureOutcomeList,
                                                                  c("exposureId", "outcomeId")))
  if (length(uniqueExposureOutcomeList) != length(exposureOutcomeList))
    stop("Duplicate exposure-outcomes pairs are not allowed")
  uniqueAnalysisIds <- unlist(unique(ParallelLogger::selectFromList(sccsAnalysisList, "analysisId")))
  if (length(uniqueAnalysisIds) != length(sccsAnalysisList))
    stop("Duplicate analysis IDs are not allowed")

  if (!file.exists(outputFolder))
    dir.create(outputFolder)


  outcomeReference <- data.frame()
  for (sccsAnalysis in sccsAnalysisList) {
    analysisId <- sccsAnalysis$analysisId
    for (exposureOutcome in exposureOutcomeList) {
      exposureId <- .selectByType(sccsAnalysis$exposureType, exposureOutcome$exposureId, "exposure")
      outcomeId <- .selectByType(sccsAnalysis$outcomeType, exposureOutcome$outcomeId, "outcome")
      row <- data.frame(exposureId = exposureId, outcomeId = outcomeId, analysisId = analysisId)
      outcomeReference <- rbind(outcomeReference, row)
    }
  }

  ### Determine if loading calls can be combined for efficiency ###

  # Step 1: determine concepts to be fetched per analysis - exposure - outcome combination
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

  # Step 2: group loads where possible
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
  # Step 3: Compute unions of concept sets, and generate loading arguments and file names
  outcomeReference$sccsDataFolder <- ""
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
    outcomeReference$sccsDataFolder[rowIds] <- sccsDataFileName
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

  ### Creation of era data objects ###
  rowId <- 1
  sccsEraDataObjectsToCreate <- list()
  outcomeReference$sccsEraDataFolder <- ""
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- paste("Analysis_", sccsAnalysis$analysisId, sep = "")
    if (!file.exists(file.path(outputFolder, analysisFolder)))
      dir.create(file.path(outputFolder, analysisFolder))
    for (exposureOutcome in exposureOutcomeList) {
      sccsEraDataFileName <- .createSccsEraDataFileName(analysisFolder,
                                                        outcomeReference$exposureId[rowId],
                                                        outcomeReference$outcomeId[rowId])
      outcomeReference$sccsEraDataFolder[rowId] <- sccsEraDataFileName
      if (!file.exists(file.path(outputFolder, sccsEraDataFileName))) {

        args <- sccsAnalysis$createSccsEraDataArgs
        covariateSettings <- args$covariateSettings
        if (is(covariateSettings, "covariateSettings"))
          covariateSettings <- list(covariateSettings)
        instantiatedSettings <- list()
        for (settings in covariateSettings) {
          includeCovariateIds <- c()
          if (length(settings$includeCovariateIds) != 0) {
            for (includeCovariateId in settings$includeCovariateIds) {
              if (is.character(includeCovariateId)) {
                if (is.null(exposureOutcome[[includeCovariateId]]))
                  stop(paste("Variable", includeCovariateId, " not found in exposure-outcome pair"))
                includeCovariateIds <- c(includeCovariateIds, exposureOutcome[[includeCovariateId]])
              } else {
                includeCovariateIds <- c(includeCovariateIds, includeCovariateId)
              }
            }
          }
          excludeCovariateIds <- c()
          if (length(settings$excludeCovariateIds) != 0) {
            for (excludeCovariateId in settings$excludeCovariateIds) {
              if (is.character(excludeCovariateId)) {
                if (is.null(exposureOutcome[[excludeCovariateId]]))
                  stop(paste("Variable", excludeCovariateId, " not found in exposure-outcome pair"))
                excludeCovariateIds <- c(excludeCovariateIds, exposureOutcome[[excludeCovariateId]])
              } else {
                excludeCovariateIds <- c(excludeCovariateIds, excludeCovariateId)
              }
            }
          }
          settings$includeCovariateIds <- includeCovariateIds
          settings$excludeCovariateIds <- excludeCovariateIds
          instantiatedSettings[[length(instantiatedSettings) + 1]] <- settings
        }
        args$covariateSettings <- instantiatedSettings
        args$outcomeId <- outcomeReference$outcomeId[rowId]
        sccsDataFileName <- outcomeReference$sccsDataFolder[rowId]
        sccsEraDataObjectsToCreate[[length(sccsEraDataObjectsToCreate) + 1]] <- list(args = args,
                                                                                     compressSccsEraDataFiles = compressSccsEraDataFiles,
                                                                                     sccsDataFileName = file.path(outputFolder, sccsDataFileName),
                                                                                     sccsEraDataFileName = file.path(outputFolder, sccsEraDataFileName))
      }
      rowId <- rowId + 1
    }
  }

  ### Creation of model objects ###
  rowId <- 1
  sccsModelObjectsToCreate <- list()
  outcomeReference$sccsModelFile <- ""
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- paste("Analysis_", sccsAnalysis$analysisId, sep = "")
    for (exposureOutcome in exposureOutcomeList) {
      sccsModelFileName <- .createSccsModelFileName(analysisFolder,
                                                    outcomeReference$exposureId[rowId],
                                                    outcomeReference$outcomeId[rowId])
      outcomeReference$sccsModelFile[rowId] <- sccsModelFileName
      if (!file.exists(file.path(outputFolder, sccsModelFileName))) {
        args <- sccsAnalysis$fitSccsModelArgs
        args$control$threads <- cvThreads
        sccsEraDataFileName <- outcomeReference$sccsEraDataFolder[rowId]

        sccsModelObjectsToCreate[[length(sccsModelObjectsToCreate) + 1]] <- list(args = args,
                                                                                 sccsEraDataFileName = file.path(outputFolder, sccsEraDataFileName),
                                                                                 sccsModelFileName = file.path(outputFolder, sccsModelFileName))
      }
      rowId <- rowId + 1
    }
  }
  saveRDS(outcomeReference, file.path(outputFolder, "outcomeModelReference.rds"))

  ### Actual construction of objects ###

  writeLines("*** Creating sccsData objects ***")
  if (length(sccsDataObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(getDbSccsDataThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsDataObjectsToCreate, createSccsDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  writeLines("*** Creating sccsEraData objects ***")
  if (length(sccsEraDataObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(createSccsEraDataThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsEraDataObjectsToCreate, createSccsEraDataObject)
    ParallelLogger::stopCluster(cluster)
  }

  writeLines("*** Fitting models ***")
  if (length(sccsModelObjectsToCreate) != 0) {
    cluster <- ParallelLogger::makeCluster(fitSccsModelThreads)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- ParallelLogger::clusterApply(cluster, sccsModelObjectsToCreate, createSccsModelObject)
    ParallelLogger::stopCluster(cluster)
  }

  invisible(outcomeReference)
}

getSccsData <- function(sccsDataFolder) {
  if (mget("sccsDataFolder", envir = globalenv(), ifnotfound = "") == sccsDataFolder) {
    sccsData <- get("sccsData", envir = globalenv())
  } else {
    sccsData <- loadSccsData(sccsDataFolder, readOnly = TRUE)
    assign("sccsData", sccsData, envir = globalenv())
    assign("sccsDataFolder", sccsDataFolder, envir = globalenv())
  }
  return(sccsData)
}

createSccsDataObject <- function(params) {
  sccsData <- do.call("getDbSccsData", params$args)
  saveSccsData(sccsData, params$sccsDataFileName)
  return(NULL)
}

createSccsEraDataObject <- function(params) {
  sccsData <- getSccsData(params$sccsDataFileName)
  params$args$sccsData <- sccsData
  sccsEraData <- do.call("createSccsEraData", params$args)
  saveSccsEraData(sccsEraData = sccsEraData,
                  folder = params$sccsEraDataFileName,
                  compress = params$compressSccsEraDataFiles)
  return(NULL)
}

createSccsModelObject <- function(params) {
  sccsEraData <- loadSccsEraData(params$sccsEraDataFileName, readOnly = TRUE)
  params$args$sccsEraData <- sccsEraData
  # sccsModel <- do.call("fitSccsModel", params$args)
  sccsModel <- fitSccsModel(sccsEraData = sccsEraData,
                            prior = params$args$prior,
                            control = params$args$control)
  saveRDS(sccsModel, params$sccsModelFileName)
  return(NULL)
}

.createSccsDataFileName <- function(loadId) {
  name <- paste("SccsData_l", loadId, sep = "")
  return(name)
}

.createSccsEraDataFileName <- function(analysisFolder, exposureId, outcomeId) {
  name <- paste("SccsEraData_e", exposureId, "_o", outcomeId, sep = "")
  return(file.path(analysisFolder, name))
}

.createSccsModelFileName <- function(analysisFolder, exposureId, outcomeId) {
  name <- paste("SccsModel_e", exposureId, "_o", outcomeId, ".rds", sep = "")
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
#' @param outcomeReference   A data.frame as created by the \code{\link{runSccsAnalyses}} function.
#' @param outputFolder       Name of the folder where all the outputs have been written to.
#'
#' @return
#' A data frame with the following columns: \tabular{ll}{ \verb{analysisId} \tab The unique identifier
#' for a set of analysis choices.\cr \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr \verb{indicationConceptIds} \tab The
#' ID(s) of indications in which to nest to study. \cr \verb{outcomeId} \tab The ID of the outcome.\cr
#' \verb{rr} \tab The estimated effect size.\cr \verb{ci95lb} \tab The lower bound of the 95 percent
#' confidence interval.\cr \verb{ci95ub} \tab The upper bound of the 95 percent confidence
#' interval.\cr \verb{treated} \tab The number of subjects in the treated group (after any trimming
#' and matching).\cr \verb{comparator} \tab The number of subjects in the comparator group (after any
#' trimming and matching).\cr \verb{eventsTreated} \tab The number of outcomes in the treated group
#' (after any trimming and matching).\cr \verb{eventsComparator} \tab The number of outcomes in the
#' comparator group (after any trimming and \cr \tab matching).\cr \verb{logRr} \tab The log of the
#' estimated relative risk.\cr \verb{seLogRr} \tab The standard error of the log of the estimated
#' relative risk.\cr }
#'
#' @export
summarizeSccsAnalyses <- function(outcomeReference, outputFolder) {
  columns <- c("analysisId", "exposureId", "outcomeId")
  result <- outcomeReference[, columns]
  result$caseCount <- 0
  result$eventCount <- 0

  for (i in 1:nrow(outcomeReference)) {
    # sccsEraData <- loadSccsEraData(as.character(outcomeReference$sccsEraDataFolder[i]))
    # s <- summary(sccsEraData)
    # result$caseCount[i] <- s$outcomeCounts$caseCount
    # result$eventCount[i] <- s$outcomeCounts$eventCount
    sccsModel <- readRDS(file.path(outputFolder, as.character(outcomeReference$sccsModelFile[i])))
    result$caseCount[i] <- ifelse(is.null(sccsModel$metaData$counts$caseCount), 0, sccsModel$metaData$counts$caseCount)
    result$eventCount[i] <- ifelse(is.null(sccsModel$metaData$counts$eventCount), 0, sccsModel$metaData$counts$eventCount)
    estimates <- sccsModel$estimates[sccsModel$estimates$originalCovariateId == outcomeReference$exposureId[i], ]
    if (!is.null(estimates) && nrow(estimates) != 0) {
      for (j in 1:nrow(estimates)) {
        estimatesToInsert <- c(rr = exp(estimates$logRr[j]),
                               ci95lb = exp(estimates$logLb95[j]),
                               ci95ub = exp(estimates$logUb95[j]),
                               logRr = estimates$logRr[j],
                               seLogRr = estimates$seLogRr[j])
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
            result$newVar <- NA
            colnames(result)[colnames(result) == "newVar"] <- colName
          }
        }
        result[i, names(estimatesToInsert)] <- estimatesToInsert
      }
    }
  }
  return(result)
}
