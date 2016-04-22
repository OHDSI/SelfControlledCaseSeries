# @file RunAnalyses.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
#' @param connectionDetails                     An R object of type \code{connectionDetails} created
#'                                              using the function \code{createConnectionDetails} in
#'                                              the \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema                     The name of the database schema that contains the OMOP
#'                                              CDM instance. Requires read permissions to this
#'                                              database. On SQL Server, this should specifiy both the
#'                                              database and the schema, so for example
#'                                              'cdm_instance.dbo'.
#' @param cdmVersion                            Define the OMOP CDM version used: currently support "4" and "5".
#' @param oracleTempSchema                      For Oracle only: the name of the database schema where
#'                                              you want all temporary tables to be managed. Requires
#'                                              create/insert permissions to this database.
#' @param exposureDatabaseSchema                The name of the database schema that is the location
#'                                              where the exposure data used to define the exposure
#'                                              cohorts is available. If exposureTable = DRUG_ERA,
#'                                              exposureDatabaseSchema is not used by assumed to be
#'                                              cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                         The tablename that contains the exposure cohorts.  If
#'                                              exposureTable <> DRUG_ERA, then expectation is
#'                                              exposureTable has format of COHORT table:
#'                                              COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                              COHORT_END_DATE.
#' @param outcomeDatabaseSchema                 The name of the database schema that is the location
#'                                              where the data used to define the outcome cohorts is
#'                                              available. If exposureTable = CONDITION_ERA,
#'                                              exposureDatabaseSchema is not used by assumed to be
#'                                              cdmSchema.  Requires read permissions to this database.
#' @param outcomeTable                          The tablename that contains the outcome cohorts.  If
#'                                              outcomeTable <> CONDITION_OCCURRENCE, then expectation
#'                                              is outcomeTable has format of COHORT table:
#'                                              COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                              COHORT_END_DATE.
#' @param outputFolder                          Name of the folder where all the outputs will written
#'                                              to.
#' @param cmAnalysisList                        A list of objects of type \code{cmAnalysis} as created
#'                                              using the \code{\link{createCmAnalysis}} function.
#' @param drugComparatorOutcomesList            A list of objects of type \code{drugComparatorOutcomes}
#'                                              as created using the
#'                                              \code{\link{createDrugComparatorOutcomes}} function.
#' @param refitPsForEveryOutcome                Should the propensity model be fitted for every outcome
#'                                              (i.e. after people who already had the outcome are
#'                                              removed)? If false, a single propensity model will be
#'                                              fitted, and people who had the outcome previously will
#'                                              be removed afterwards.
#' @param getDbCohortMethodDataThreads          The number of parallel threads to use for building the
#'                                              cohortMethod data objects.
#' @param createPsThreads                       The number of parallel threads to use for fitting the
#'                                              propensity models.
#' @param psCvThreads                           The number of parallel threads to use for the cross-
#'                                              validation when estimating the hyperparameter for the
#'                                              propensity model. Note that the total number of CV
#'                                              threads at one time could be `createPsThreads *
#'                                              psCvThreads`.
#' @param createStudyPopThreads                 The number of parallel threads to use for creating the
#'                                              study population.
#' @param computeCovarBalThreads                The number of parallel threads to use for computing the
#'                                              covariate balance.
#' @param trimMatchStratifyThreads              The number of parallel threads to use for trimming,
#'                                              matching and stratifying.
#' @param fitOutcomeModelThreads                The number of parallel threads to use for fitting the
#'                                              outcome models.
#' @param outcomeCvThreads                      The number of parallel threads to use for the cross-
#'                                              validation when estimating the hyperparameter for the
#'                                              outcome model. Note that the total number of CV threads
#'                                              at one time could be `fitOutcomeModelThreads *
#'                                              outcomeCvThreads`.
#'
#' @return
#' A data frame with the following columns:
#' \tabular{ll}{
#' \verb{analysisId} \tab The unique identifier for a set of analysis choices.\cr
#' \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr
#' \verb{excludedCovariateConceptIds} \tab The ID(s) of concepts that cannot be used to construct covariates. \cr
#' \verb{includedCovariateConceptIds} \tab The ID(s) of concepts that should be used to construct covariates. \cr
#' \verb{outcomeId} \tab The ID of the outcome \cr
#' \verb{cohortMethodDataFolder} \tab The ID of the outcome.\cr
#' \verb{sharedPsFile}                \tab The name of the file containing the propensity scores of the shared \cr
#'                                    \tab propensity model. This model is used to create the outcome-specific \cr
#'                                    \tab propensity scores by removing people with prior outcomes.\cr
#' \verb{studyPopFile                 \tab The name of the file containing the study population (prior\cr
#'                                    \tab and trimming, matching, or stratification on the PS.\cr
#' \verb{psFile}                      \tab The name of file containing the propensity scores for a specific \cr
#'                                    \tab outcomes (ie after people with prior outcomes have been removed).\cr
#' \verb{strataFile}                  \tab The name of the file containing the identifiers of the population \cr
#'                                    \tab after any trimming, matching or stratifying, including their strata.\cr
#' \verb{covariateBalanceFile}        \tab The name of the file containing the covariate balance (ie. the \cr
#'                                    \tab output of the \code{computeCovariateBalance} function.\cr
#' \verb{outcomeModelFile} \tab The name of the file containing the outcome model.\cr
#' }
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
                            cdmVersion = 5,
                            outputFolder = "./SccsOutput",
                            sccsAnalysisList,
                            exposureOutcomeList,
                            combineDataFetchAcrossOutcomes = TRUE,
                            getDbSccsDataThreads = 1,
                            createSccsEraDataThreads = 1,
                            fitSccsModelThreads = 1 ) {
  for (exposureOutcome in exposureOutcomeList)
    stopifnot(class(exposureOutcome) == "exposureOutcome")
  for (sccsAnalysis in sccsAnalysisList)
    stopifnot(class(sccsAnalysis) == "sccsAnalysis")
  uniqueExposureOutcomeList <- unique(OhdsiRTools::selectFromList(exposureOutcomeList,
                                                                  c("exposureId",
                                                                    "outcomeId")))
  if (length(uniqueExposureOutcomeList) != length(exposureOutcomeList))
    stop("Duplicate exposure-outcomes pairs are not allowed")
  uniqueAnalysisIds <- unlist(unique(OhdsiRTools::selectFromList(sccsAnalysisList, "analysisId")))
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
      row <- data.frame(exposureId = exposureId,
                        outcomeId = outcomeId,
                        analysisId = analysisId)
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
          if (is.character(exposureId)) {
            if (is.null(exposureOutcome[[exposureId]]))
              stop(paste("Variable", exposureId," not found in exposure-outcome pair"))
            if (exposureId == "exposureId") {
              exposureId <- .selectByType(exposureId, exposureOutcome$exposureId, "exposure")
              exposureIds <- c(exposureIds, exposureId)
            } else {
              exposureIds <- c(exposureIds, exposureOutcome[[exposureId]])
            }
          } else {
            exposureIds <- c(exposureIds, exposureId)
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
                stop(paste("Variable", customCovariateId," not found in exposure-outcome pair"))
              exposureIds <- c(exposureIds, exposureOutcome[[exposureId]])
            } else {
              exposureIds <- c(exposureIds, exposureId)
            }
          }
        }
      }
      row <- list(outcomeId = outcomeId,
                  exposureIds = exposureIds,
                  customCovariateIds = customCovariateIds,
                  deleteCovariatesSmallCount = sccsAnalysis$getDbSccsDataArgs$deleteCovariatesSmallCount,
                  studyStartDate = sccsAnalysis$getDbSccsDataArgs$studyStartDate,
                  studyEndDate = sccsAnalysis$getDbSccsDataArgs$studyEndDate,
                  rowId = rowId)
      conceptsPerLoad[[length(conceptsPerLoad) + 1]] <- row
      rowId <- rowId + 1
    }
  }
  # Step 2: group loads where possible
  if (combineDataFetchAcrossOutcomes) {
    uniqueLoads <-unique(OhdsiRTools::selectFromList(conceptsPerLoad,
                                                     c("deleteCovariatesSmallCount",
                                                       "studyStartDate",
                                                       "studyEndDate")))
  } else {
    uniqueLoads <-unique(OhdsiRTools::selectFromList(conceptsPerLoad,
                                                     c("deleteCovariatesSmallCount",
                                                       "studyStartDate",
                                                       "studyEndDate",
                                                       "outcomeId")))
  }
  # Step 3: Compute unions of concept sets, and generate loading arguments and file names
  outcomeReference$sccsDataFolder <- ""
  sccsDataObjectsToCreate <- list()
  for (loadId in 1:length(uniqueLoads)) {
    uniqueLoad <- uniqueLoads[[loadId]]
    groupables <- OhdsiRTools::matchInList(conceptsPerLoad, uniqueLoad)
    outcomeIds <- c()
    exposureIds <- c()
    customCovariateIds <- c()
    rowIds <- c()
    for (groupable in groupables) {
      outcomeIds <- c(outcomeIds, groupable$outcomeId)
      if (!(length(exposureIds) == 1 && exposureIds[1] == "all")) {
        if (groupable$exposureIds == "all") {
          exposureIds = "all"
        } else {
          exposureIds <- c(exposureIds, groupable$exposureIds)
        }
      }
      if (!(length(customCovariateIds) == 1 && customCovariateIds[1] == "all")) {
        if ((length(groupable$customCovariateIds) == 1 && groupable$customCovariateIds == "all")) {
          customCovariateIds = "all"
        } else {
          customCovariateIds <- c(customCovariateIds, groupable$customCovariateIds)
        }
      }
      rowIds <- c(rowIds, groupable$rowId)
    }
    sccsDataFileName <- .createSccsDataFileName(outputFolder, loadId)
    outcomeReference$sccsDataFolder[rowIds] <- sccsDataFileName
    if (!file.exists(sccsDataFileName)) {
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
                   exposureDatabaseSchema = exposureDatabaseSchema,
                   exposureTable = exposureTable,
                   outcomeDatabaseSchema = outcomeDatabaseSchema,
                   outcomeTable = outcomeTable,
                   customCovariateDatabaseSchema = customCovariateDatabaseSchema,
                   customCovariateTable = customCovariateTable,
                   cdmVersion = cdmVersion,
                   exposureIds = exposureIds,
                   outcomeIds = outcomeIds,
                   useCustomCovariates = useCustomCovariates,
                   customCovariateIds = customCovariateIds,
                   deleteCovariatesSmallCount = groupables[[1]]$deleteCovariatesSmallCount,
                   studyStartDate = groupables[[1]]$studyStartDate,
                   studyEndDate = groupables[[1]]$studyEndDate)
      sccsDataObjectsToCreate[[length(sccsDataObjectsToCreate) + 1]] <- list(args = args,
                                                                             sccsDataFileName = sccsDataFileName)
    }
  }

  ### Creation of era data objects ###
  rowId <- 1
  sccsEraDataObjectsToCreate <- list()
  outcomeReference$sccsEraDataFolder <- ""
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- file.path(outputFolder,
                                paste("Analysis_", sccsAnalysis$analysisId, sep = ""))
    if (!file.exists(analysisFolder))
      dir.create(analysisFolder)
    for (exposureOutcome in exposureOutcomeList) {
      sccsEraDataFileName <- .createSccsEraDataFileName(analysisFolder,
                                                        outcomeReference$exposureId[rowId],
                                                        outcomeReference$outcomeId[rowId])
      outcomeReference$sccsEraDataFolder[rowId] <- sccsEraDataFileName
      if (!file.exists(sccsEraDataFileName)) {

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
                  stop(paste("Variable", includeCovariateId," not found in exposure-outcome pair"))
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
                  stop(paste("Variable", excludeCovariateId," not found in exposure-outcome pair"))
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

        sccsDataFileName <- outcomeReference$sccsDataFolder[rowId]
        sccsEraDataObjectsToCreate[[length(sccsEraDataObjectsToCreate) + 1]] <- list(args = args,
                                                                                     sccsDataFileName = sccsDataFileName,
                                                                                     sccsEraDataFileName = sccsEraDataFileName)
      }
      rowId <- rowId + 1
    }
  }

  ### Creation of era data objects ###
  rowId <- 1
  sccsModelObjectsToCreate <- list()
  outcomeReference$sccsModelFile <- ""
  for (sccsAnalysis in sccsAnalysisList) {
    analysisFolder <- file.path(outputFolder,
                                paste("Analysis_", sccsAnalysis$analysisId, sep = ""))
    for (exposureOutcome in exposureOutcomeList) {
      sccsModelFileName <- .createSccsModelFileName(analysisFolder,
                                                    outcomeReference$exposureId[rowId],
                                                    outcomeReference$outcomeId[rowId])
      outcomeReference$sccsModelFile[rowId] <- sccsModelFileName
      if (!file.exists(sccsModelFileName)) {
        args <- sccsAnalysis$fitSccsModelArgs
        sccsEraDataFileName <- outcomeReference$sccsEraDataFolder[rowId]

        sccsModelObjectsToCreate[[length(sccsModelObjectsToCreate) + 1]] <- list(args = args,
                                                                                 sccsEraDataFileName = sccsEraDataFileName,
                                                                                 sccsModelFileName = sccsModelFileName)
      }
      rowId <- rowId + 1
    }
  }
  saveRDS(outcomeReference, file.path(outputFolder, "outcomeModelReference.rds"))

  ### Actual construction of objects ###

  createSccsDataObject <- function(params) {
    sccsData <- do.call("getDbSccsData", params$args)
    saveSccsData(sccsData, params$sccsDataFileName)
  }
  if (length(sccsDataObjectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(getDbSccsDataThreads)
    OhdsiRTools::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- OhdsiRTools::clusterApply(cluster, sccsDataObjectsToCreate, createSccsDataObject)
    OhdsiRTools::stopCluster(cluster)
  }

  createSccsEraDataObject <- function(params) {
    sccsData <- loadSccsData(params$sccsDataFileName, readOnly = TRUE)
    params$args$sccsData <- sccsData
    sccsEraData <- do.call("createSccsEraData", params$args)
    saveSccsEraData(sccsEraData, params$sccsEraDataFileName)
  }
  if (length(sccsEraDataObjectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(createSccsEraDataThreads)
    OhdsiRTools::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- OhdsiRTools::clusterApply(cluster, sccsEraDataObjectsToCreate, createSccsEraDataObject)
    OhdsiRTools::stopCluster(cluster)
  }

  createSccsModelObject <- function(params) {
    sccsEraData <- loadSccsEraData(params$sccsEraDataFileName, readOnly = TRUE)
    params$args$sccsEraData <- sccsEraData
    sccsModel <- do.call("fitSccsModel", params$args)
    saveRDS(sccsModel, params$sccsModelFileName)
  }
  if (length(sccsModelObjectsToCreate) != 0) {
    cluster <- OhdsiRTools::makeCluster(fitSccsModelThreads)
    OhdsiRTools::clusterRequire(cluster, "SelfControlledCaseSeries")
    dummy <- OhdsiRTools::clusterApply(cluster, sccsModelObjectsToCreate, createSccsModelObject)
    OhdsiRTools::stopCluster(cluster)
  }

  invisible(outcomeReference)
}

.createSccsDataFileName <- function(folder,
                                    loadId) {
  name <- paste("SccsData_l", loadId, sep = "")
  return(file.path(folder, name))
}

.createSccsEraDataFileName <- function(folder,
                                       exposureId,
                                       outcomeId) {
  name <- paste("SccsEraData_e", exposureId, "_o", outcomeId, sep = "")
  return(file.path(folder, name))
}

.createSccsModelFileName <- function(folder,
                                     exposureId,
                                     outcomeId) {
  name <- paste("SccsModel_e", exposureId, "_o", outcomeId, ".rds", sep = "")
  return(file.path(folder, name))
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
#' @param outcomeReference   A data.frame as created by the \code{\link{runCmAnalyses}} function.
#'
#' @return
#' A data frame with the following columns:
#' \tabular{ll}{
#' \verb{analysisId} \tab The unique identifier for a set of analysis choices.\cr
#' \verb{targetId} \tab The ID of the target drug.\cr
#' \verb{comparatorId} \tab The ID of the comparator group.\cr
#' \verb{indicationConceptIds} \tab The ID(s) of indications in which to nest to study. \cr
#' \verb{outcomeId} \tab The ID of the outcome.\cr
#' \verb{rr} \tab The estimated effect size.\cr
#' \verb{ci95lb} \tab The lower bound of the 95 percent confidence interval.\cr
#' \verb{ci95ub} \tab The upper bound of the 95 percent confidence interval.\cr
#' \verb{treated} \tab The number of subjects in the treated group (after any trimming and matching).\cr
#' \verb{comparator} \tab The number of subjects in the comparator group (after any trimming and matching).\cr
#' \verb{eventsTreated} \tab The number of outcomes in the treated group (after any trimming and matching).\cr
#' \verb{eventsComparator} \tab The number of outcomes in the comparator group (after any trimming and \cr
#' \tab matching).\cr
#' \verb{logRr} \tab The log of the estimated relative risk.\cr
#' \verb{seLogRr} \tab The standard error of the log of the estimated relative risk.\cr
#' }
#'
#' @export
summarizeSccsAnalyses <- function(outcomeReference) {
  columns <- c("analysisId", "targetId", "comparatorId", "outcomeId")
  result <- outcomeReference[, columns]
  result$rr <- 0
  result$ci95lb <- 0
  result$ci95ub <- 0
  result$p <- 1
  result$treated <- 0
  result$comparator <- 0
  result$treatedDays <- NA
  result$comparatorDays <- NA
  result$eventsTreated <- 0
  result$eventsComparator <- 0
  result$logRr <- 0
  result$seLogRr <- 0
  for (i in 1:nrow(outcomeReference)) {
    outcomeModel <- readRDS(outcomeReference$outcomeModelFile[i])
    if (outcomeReference$strataFile[i] == "") {
      studyPop <- readRDS(outcomeReference$studyPopFile[i])
    } else {
      studyPop <- readRDS(outcomeReference$strataFile[i])
    }
    result$rr[i] <- if (is.null(coef(outcomeModel)))
      NA else exp(coef(outcomeModel))
    result$ci95lb[i] <- if (is.null(coef(outcomeModel)))
      NA else exp(confint(outcomeModel)[1])
    result$ci95ub[i] <- if (is.null(coef(outcomeModel)))
      NA else exp(confint(outcomeModel)[2])
    if (is.null(coef(outcomeModel))) {
      result$p[i] <- NA
    } else {
      z <- coef(outcomeModel)/ outcomeModel$outcomeModelTreatmentEstimate$seLogRr
      result$p[i] <- 2 * pmin(pnorm(z), 1 - pnorm(z))
    }
    result$treated[i] <- sum(studyPop$treatment == 1)
    result$comparator[i] <- sum(studyPop$treatment == 0)
    if (outcomeModel$outcomeModelType == "cox") {
      result$treatedDays[i] <- sum(studyPop$survivalTime[studyPop$treatment == 1])
      result$comparatorDays[i] <- sum(studyPop$survivalTime[studyPop$treatment == 0])
    } else if (outcomeModel$outcomeModelType == "poisson") {
      result$treatedDays[i] <- sum(studyPop$timeAtRisk[studyPop$treatment == 1])
      result$comparatorDays[i] <- sum(studyPop$timeAtRisk[studyPop$treatment == 0])
    }
    if (outcomeModel$outcomeModelType == "cox" || outcomeModel$outcomeModelType == "logistic") {
      result$eventsTreated[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 1] != 0)
      result$eventsComparator[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 0] != 0)
    } else {
      result$eventsTreated[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 1])
      result$eventsComparator[i] <- sum(studyPop$outcomeCount[studyPop$treatment == 0])
    }
    result$logRr[i] <- if (is.null(coef(outcomeModel)))
      NA else coef(outcomeModel)
    result$seLogRr[i] <- if (is.null(coef(outcomeModel)))
      NA else outcomeModel$outcomeModelTreatmentEstimate$seLogRr
  }
  return(result)
}
