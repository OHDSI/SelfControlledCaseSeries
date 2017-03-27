# @file DataLoadingSaving.R
#
# Copyright 2017 Observational Health Data Sciences and Informatics
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

#' Load data for SCCS from the database
#'
#' @description
#' Load all data needed to perform an SCCS analysis from the database.
#'
#' @details
#' This function downloads several types of information:
#' \itemize{
#'   \item {Information on the occurrences of the outcome(s) of interest. Note that information for
#'         multiple outcomes can be fetched in one go, and later the specific outcome can be specified
#'         for which we want to build a model.}
#'   \item {Information on the observation time and age for the people with the outcomes.}
#'   \item {Information on exposures of interest which we want to include in the model.}
#' }
#' Four different database schemas can be specified, for four different types of information: The
#' \code{cdmDatabaseSchema} is used to extract patient age and observation period. The
#' \code{outcomeDatabaseSchema} is used to extract information about the outcomes, the
#' \code{exposureDatabaseSchema} is used to retrieve information on exposures, and the
#' \code{customCovariateDatabaseSchema} is optionally used to find additional, user-defined
#' covariates. All four locations could point to the same database schema.
#'
#' @return
#' Returns an object of type \code{sccsData}, containing information on the cases, their outcomes,
#' exposures, and potentially other covariates. Information about multiple outcomes can be captured at
#' once for efficiency reasons. This object is a list with the following components: \describe{
#' \item{cases}{An ffdf object listing the persons that have the outcome(s), their age, and
#' observation time.} \item{eras}{An ffdf object listing the exposures, outcomes and other
#' covariates.} \item{covariateRef}{An ffdf object describing the covariates that have been
#' extracted.} \item{metaData}{A list of objects with information on how the sccsData object was
#' constructed.} } The generic \code{summary()} function has been implemented for this object.
#'
#' @param connectionDetails               An R object of type \code{ConnectionDetails} created using
#'                                        the function \code{createConnectionDetails} in the
#'                                        \code{DatabaseConnector} package.
#' @param cdmDatabaseSchema               The name of the database schema that contains the OMOP CDM
#'                                        instance.  Requires read permissions to this database. On SQL
#'                                        Server, this should specifiy both the database and the
#'                                        schema, so for example 'cdm_instance.dbo'.
#' @param oracleTempSchema                A schema where temp tables can be created in Oracle.
#' @param outcomeDatabaseSchema           The name of the database schema that is the location where
#'                                        the data used to define the outcome cohorts is available. If
#'                                        outcomeTable = CONDITION_ERA, outcomeDatabaseSchema is not
#'                                        used.  Requires read permissions to this database.
#' @param outcomeTable                    The tablename that contains the outcome cohorts.  If
#'                                        outcomeTable is not CONDITION_OCCURRENCE or CONDITION_ERA,
#'                                        then expectation is outcomeTable has format of COHORT table:
#'                                        COHORT_DEFINITION_ID, SUBJECT_ID, COHORT_START_DATE,
#'                                        COHORT_END_DATE.
#' @param outcomeIds                      A list of ids used to define outcomes.  If outcomeTable =
#'                                        CONDITION_OCCURRENCE, the list is a set of ancestor
#'                                        CONCEPT_IDs, and all occurrences of all descendant concepts
#'                                        will be selected.  If outcomeTable <> CONDITION_OCCURRENCE,
#'                                        the list contains records found in COHORT_DEFINITION_ID
#'                                        field.
#' @param exposureDatabaseSchema          The name of the database schema that is the location where
#'                                        the exposure data used to define the exposure cohorts is
#'                                        available. If exposureTable = DRUG_ERA,
#'                                        exposureDatabaseSchema is not used but assumed to be
#'                                        cdmSchema.  Requires read permissions to this database.
#' @param exposureTable                   The tablename that contains the exposure cohorts.  If
#'                                        exposureTable <> DRUG_ERA, then expectation is exposureTable
#'                                        has format of COHORT table: cohort_concept_id, SUBJECT_ID,
#'                                        COHORT_START_DATE, COHORT_END_DATE.
#' @param exposureIds                     A list of identifiers to define the exposures of interest. If
#'                                        exposureTable = DRUG_ERA, exposureIds should be CONCEPT_ID.
#'                                        If exposureTable <> DRUG_ERA, exposureIds is used to select
#'                                        the cohort_concept_id in the cohort-like table. If no
#'                                        exposureIds are provided, all drugs or cohorts in the
#'                                        exposureTable are included as exposures.
#' @param useCustomCovariates             Create covariates from a custom table?
#' @param customCovariateDatabaseSchema   The name of the database schema that is the location where
#'                                        the custom covariate data is available.
#' @param customCovariateTable            Name of the table holding the custom covariates. This table
#'                                        should have the same structure as the cohort table.
#' @param customCovariateIds              A list of cohort definition IDS identifying the records in
#'                                        the customCovariateTable to use for building custom
#'                                        covariates.
#' @param deleteCovariatesSmallCount      The minimum count for a covariate to appear in the data to be
#'                                        kept.
#' @param studyStartDate                  A calendar date specifying the minimum date where data is
#'                                        used. Date format is 'yyyymmdd'.
#' @param studyEndDate                    A calendar date specifying the maximum date where data is
#'                                        used. Date format is 'yyyymmdd'.
#' @param cdmVersion                      Define the OMOP CDM version used: currently support "4" and
#'                                        "5".
#'
#' @export
getDbSccsData <- function(connectionDetails,
                          cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_era",
                          outcomeIds,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          useCustomCovariates = FALSE,
                          customCovariateDatabaseSchema = cdmDatabaseSchema,
                          customCovariateTable = "cohort",
                          customCovariateIds = c(),
                          deleteCovariatesSmallCount = 100,
                          studyStartDate = "",
                          studyEndDate = "",
                          cdmVersion = "4") {
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1) {
    stop("Study start date must have format YYYYMMDD")
  }
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1) {
    stop("Study end date must have format YYYYMMDD")
  }

  conn <- connect(connectionDetails)
  if (cdmVersion == "4") {
    cohortDefinitionId <- "cohort_concept_id"
  } else {
    cohortDefinitionId <- "cohort_definition_id"
  }
  if (is.null(exposureIds) || length(exposureIds) == 0) {
    hasExposureIds <- FALSE
  } else {
    if (!is.numeric(exposureIds))
      stop("exposureIds must be a (vector of) numeric")
    hasExposureIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#exposure_ids",
                                   data = data.frame(concept_id = as.integer(exposureIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }

  if (is.null(customCovariateIds) || length(customCovariateIds) == 0) {
    hasCustomCovariateIds <- FALSE
  } else {
    if (!is.numeric(customCovariateIds))
      stop("customCovariateIds must be a (vector of) numeric")
    hasCustomCovariateIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#custom_covariate_ids",
                                   data = data.frame(concept_id = as.integer(customCovariateIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }
  renderedSql <- SqlRender::loadRenderTranslateSql("Sccs.sql",
                                                   packageName = "SelfControlledCaseSeries",
                                                   dbms = connectionDetails$dbms,
                                                   oracleTempSchema = oracleTempSchema,
                                                   cdm_database_schema = cdmDatabaseSchema,
                                                   outcome_database_schema = outcomeDatabaseSchema,
                                                   outcome_table = outcomeTable,
                                                   outcome_concept_ids = outcomeIds,
                                                   exposure_database_schema = exposureDatabaseSchema,
                                                   exposure_table = exposureTable,
                                                   use_custom_covariates = useCustomCovariates,
                                                   custom_covariate_database_schema = customCovariateDatabaseSchema,
                                                   custom_covariate_table = customCovariateTable,
                                                   has_exposure_ids = hasExposureIds,
                                                   has_custom_covariate_ids = hasCustomCovariateIds,
                                                   delete_covariates_small_count = deleteCovariatesSmallCount,
                                                   study_start_date = studyStartDate,
                                                   study_end_date = studyEndDate,
                                                   cdm_version = cdmVersion,
                                                   cohort_definition_id = cohortDefinitionId)

  writeLines("Executing multiple queries. This could take a while")
  executeSql(conn, renderedSql)

  writeLines("Fetching data from server")
  start <- Sys.time()
  renderedSql <- SqlRender::loadRenderTranslateSql("QueryCases.sql",
                                                   packageName = "SelfControlledCaseSeries",
                                                   dbms = connectionDetails$dbms,
												   oracleTempSchema = oracleTempSchema)
  cases <- querySql.ffdf(conn, renderedSql)

  renderedSql <- "SELECT era_type, observation_period_id, concept_id, era_value AS value, start_day, end_day FROM #eras ORDER BY observation_period_id"
  renderedSql <- SqlRender::translateSql(sql = renderedSql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)$sql
  eras <- querySql.ffdf(conn, renderedSql)

  renderedSql <- "SELECT covariate_id, covariate_name FROM #covariate_ref"
  renderedSql <- SqlRender::translateSql(sql = renderedSql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)$sql
  covariateRef <- querySql.ffdf(conn, renderedSql)

  delta <- Sys.time() - start
  writeLines(paste("Loading took", signif(delta, 3), attr(delta, "units")))

  if (connectionDetails$dbms == "oracle") {
    renderedSql <- SqlRender::loadRenderTranslateSql("RemoveTempTables.sql",
                                                     packageName = "SelfControlledCaseSeries",
                                                     dbms = connectionDetails$dbms,
													 oracleTempSchema = oracleTempSchema)
    DatabaseConnector::executeSql(conn, renderedSql, progressBar = FALSE, reportOverallTime = FALSE)
  }
  dbDisconnect(conn)

  colnames(cases) <- SqlRender::snakeCaseToCamelCase(colnames(cases))
  colnames(eras) <- SqlRender::snakeCaseToCamelCase(colnames(eras))
  colnames(covariateRef) <- SqlRender::snakeCaseToCamelCase(colnames(covariateRef))
  metaData <- list(exposureIds = exposureIds, outcomeIds = outcomeIds, call = match.call())
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
#' @param sccsData   An object of type \code{sccsData} as generated using \code{\link{getDbSccsData}}.
#' @param folder     The name of the folder where the data will be written. The folder should not yet
#'                   exist.
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
#' @param folder     The name of the folder containing the data.
#' @param readOnly   If true, the data is opened read only.
#'
#' @details
#' The data will be written to a set of files in the folder specified by the user.
#'
#' @return
#' An object of class cohortData.
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
  hois <- object$eras[ffbase::ffwhich(t, t == TRUE), ]
  for (i in 1:nrow(outcomeCounts)) {
    outcomeCounts$eventCount[i] <- ffbase::sum.ff(hois$conceptId == object$metaData$outcomeIds[i])
    if (outcomeCounts$eventCount[i] == 0) {
      outcomeCounts$caseCount[i] <- 0
    } else {
      t <- (hois$conceptId == object$metaData$outcomeIds[i])
      outcomeCounts$caseCount[i] <- length(ffbase::unique.ff(hois$observationPeriodId[ffbase::ffwhich(t,
                                                                                                      t == TRUE)]))
    }
  }
  covariateValueCount <- ffbase::sum.ff(object$eras$eraType != "hoi")

  result <- list(metaData = object$metaData,
                 caseCount = caseCount,
                 outcomeCounts = outcomeCounts,
                 covariateCount = nrow(object$covariateRef) - length(object$metaData$outcomeIds),
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
  writeLines(paste("Number of covariate eras:", x$covariateValueCount))
}
