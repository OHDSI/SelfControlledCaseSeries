# @file DataLoadingSaving.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

#' Todo: add title
#'
#' @description
#' Todo: add description
#'
#' @details
#' Todo: add details
#'
#' @param connectionDetails  An R object of type \code{ConnectionDetails} created using the
#'                                         function \code{createConnectionDetails} in the
#'                                         \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema
#' @param oracleTempSchema
#' @param outcomeDatabaseSchema
#' @param outcomeTable
#' @param outcomeIds
#' @param outcomeConditionTypeConceptIds
#' @param exposureDatabaseSchema
#' @param exposureTable
#' @param exposureIds
#' @param excludeConceptIds
#' @param drugEraCovariates
#' @param conditionEraCovariates
#' @param procedureCovariates
#' @param visitCovariates
#' @param observationCovariates
#' @param measurementCovariates
#' @param deleteCovariatesSmallCount
#' @param cdmVersion
#'
#' @export
getDbSccsData <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_occurrence",
                          outcomeIds,
                          outcomeConditionTypeConceptIds = c(),
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          excludeConceptIds = c(),
                          drugEraCovariates = FALSE,
                          conditionEraCovariates = FALSE,
                          procedureCovariates = FALSE,
                          visitCovariates = FALSE,
                          observationCovariates = FALSE,
                          measurementCovariates = FALSE,
                          deleteCovariatesSmallCount = 100,
                          cdmVersion = "4") {
  if (exposureTable == "drug_era" && length(exposureIds) == 0 && drugEraCovariates){
    drugEraCovariates = FALSE
    warning("Including all drugs in era table as exposures of interest, so using drug era covariates would duplicate all exposures. Setting drugEraCovariates to false.")
  }

  if (cdmVersion == "4"){
    cohortDefinitionId <- "cohort_concept_id"
  } else {
    cohortDefinitionId <- "cohort_definition_id"
  }

  cdmDatabase <- strsplit(cdmDatabaseSchema, "\\.")[[1]][1]
  renderedSql <- SqlRender::loadRenderTranslateSql("Sccs.sql",
                                                   packageName = "SelfControlledCaseSeries",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database = cdmDatabase,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_concept_ids = outcomeIds,
                                                   outcome_condition_type_concept_ids = outcomeConditionTypeConceptIds,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = exposureTable,
                                                   exposure_concept_ids = exposureIds,
                                                   exclude_concept_ids = excludeConceptIds,
                                                   drug_era_covariates = drugEraCovariates,
                                                   condition_era_covariates = conditionEraCovariates,
                                                   procedure_covariates = procedureCovariates,
                                                   visit_covariates = visitCovariates,
                                                   observation_covariates = observationCovariates,
                                                   measurement_covariates = measurementCovariates,
                                                   delete_covariates_small_count = deleteCovariatesSmallCount,
                                                   cdm_version = cdmVersion,
                                                   cohort_definition_id = cohortDefinitionId)
  conn <- connect(connectionDetails)

  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn, renderedSql)

  writeLines("Fetching data from server")
  start <- Sys.time()
  renderedSql <- SqlRender::loadRenderTranslateSql("QueryCases.sql",
                                                   packageName = "SelfControlledCaseSeries",
                                                   dbms = connectionDetails$dbms)
  cases <- querySql.ffdf(conn, renderedSql)

  renderedSql <- "SELECT era_type, observation_period_id, concept_id, era_value AS value, start_day, end_day FROM #eras ORDER BY observation_period_id"
  renderedSql <- SqlRender::translateSql(renderedSql, "sql server", connectionDetails$dbms)$sql
  eras <- querySql.ffdf(conn, renderedSql)

  renderedSql <- "SELECT covariate_id, covariate_name FROM #covariate_ref"
  renderedSql <- SqlRender::translateSql(renderedSql, "sql server", connectionDetails$dbms)$sql
  covariateRef <- querySql.ffdf(conn, renderedSql)

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  if (connectionDetails$dbms == "oracle") {
    renderedSql <- SqlRender::loadRenderTranslateSql("RemoveTempTables.sql",
                                                     packageName = "SelfControlledCaseSeries",
                                                     dbms = connectionDetails$dbms)
    DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  }
  dbDisconnect(conn)

  colnames(cases) <- SqlRender::snakeCaseToCamelCase(colnames(cases))
  colnames(eras) <- SqlRender::snakeCaseToCamelCase(colnames(eras))
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))
  metaData <- list(exposureIds = exposureIds,
                   outcomeIds = outcomeIds,
                   call = match.call())
  result <- list(cases = cases, eras = eras, covariateRef = covariateRef, metaData = metaData)

  # Open all ffdfs to prevent annoying messages later:
  open(result$cases)
  open(result$eras)
  open(result$covariateRef)
  class(result) <- "sccsData"
  return(result)
}

#' Save the SCCS data to folder
#'
#' @description
#' \code{sccsData} saves an object of type sccsData to folder.
#'
#' @param sccsData   An object of type \code{sccsData} as generated using
#'                     \code{\link{getDbSccsData}}.
#' @param folder       The name of the folder where the data will be written. The folder should not yet
#'                     exist.
#'
#' @details
#' The data will be written to a set of files in the specified folder.
#'
#' @examples
#' # todo
#'
#' @export
saveSccsData <- function(sccsData, folder) {
  if (missing(sccsData))
    stop("Must specify sccsData")
  if (missing(folder))
    stop("Must specify folder")
  if (class(sccsData) != "sccsData")
    stop("Data not of class sccsData")

  cases <- sccsData$cases
  eras <- sccsData$eras
  covariateRef <- sccsData$covariateRef
  ffbase::save.ffdf(cases, eras, covariateRef, dir = folder)
  metaData <- sccsData$metaData
  save(metaData, file = file.path(folder, "metaData.Rdata"))
  # Open all ffdfs to prevent annoying messages later:
  open(sccsData$cases)
  open(sccsData$eras)
  open(sccsData$covariateRef)
  invisible(TRUE)
}

#' Load the SCCS data from a folder
#'
#' @description
#' \code{loadSccsData} loads an object of type sccsData from a folder in the file system.
#'
#' @param folder       The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class cohortData.
#'
#' @examples
#' # todo
#'
#' @export
loadSccsData <- function(folder, readOnly = TRUE) {
  if (!file.exists(folder))
    stop(paste("Cannot find folder", folder))
  if (!file.info(folder)$isdir)
    stop(paste("Not a folder:", folder))

  temp <- setwd(folder)
  absolutePath <- setwd(temp)

  e <- new.env()
  ffbase::load.ffdf(absolutePath, e)
  load(file.path(absolutePath, "metaData.Rdata"), e)
  result <- list(cases = get("cases", envir = e),
                 eras = get("eras", envir = e),
                 covariateRef = get("covariateRef", envir = e),
                 metaData = get("metaData", envir = e))
  # Open all ffdfs to prevent annoying messages later:
  open(result$cases, readonly = readOnly)
  open(result$eras, readonly = readOnly)
  open(result$covariateRef, readonly = readOnly)
  class(result) <- "sccsData"
  rm(e)
  return(result)
}

#' @export
print.sccsData <- function(x, ...) {
  writeLines("SCCS data object")
  writeLines("")
  writeLines(paste("Exposure concept ID(s):", paste(x$metaData$exposureIds, collapse = ",")))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
}

#' @export
summary.sccsData <- function(object, ...) {
  caseCount <- nrow(object$cases)

  outcomeCounts <- data.frame(outcomeConceptId = object$metaData$outcomeIds,
                              eventCount = 0,
                              caseCount = 0)
  t <- object$eras$eraType == "hoi"
  hois <- object$eras[ffbase::ffwhich(t, t == TRUE),]
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- ffbase::sum.ff(hois$conceptId == object$metaData$outcomeIds[i])
    if (outcomeCounts$eventCount[i] == 0) {
      outcomeCounts$caseCount[i] <- 0
    } else {
      t <- (hois$conceptId == object$metaData$outcomeIds[i])
      outcomeCounts$caseCount[i] <- length(ffbase::unique.ff(hois$observationPeriodId[ffbase::ffwhich(t, t == TRUE)]))
    }
  }
  covariateValueCount <- ffbase::sum.ff(object$eras$eraType != "hoi")

  result <- list(metaData = object$metaData,
                 caseCount = caseCount,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef),
                 covariateValueCount = covariateValueCount)
  class(result) <- "summary.sccsData"
  return(result)
}

#' @export
print.summary.sccsData <- function(x, ...) {
  writeLines("sccsData object summary")
  writeLines("")
  writeLines(paste("Exposure concept ID(s):", paste(x$metaData$exposureIds, collapse = ",")))
  writeLines(paste("Outcome concept ID(s):", paste(x$metaData$outcomeIds, collapse = ",")))
  writeLines("")
  writeLines(paste("Cases:", paste(x$caseCount)))
  writeLines("")
  writeLines("Outcome counts:")
  outcomeCounts <- x$outcomeCounts
  rownames(outcomeCounts) <- outcomeCounts$outcomeConceptId
  outcomeCounts$outcomeConceptId <- NULL
  colnames(outcomeCounts) <- c("Event count", "Case count")
  printCoefmat(outcomeCounts)
  writeLines("")
  writeLines("Covariates:")
  writeLines(paste("Number of covariates:", x$covariateCount))
  writeLines(paste("Number of non-zero covariate values:", x$covariateValueCount))
}
