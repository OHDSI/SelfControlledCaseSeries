# @file DataLoadingSaving.R
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

#' Load data for SCCS from the database
#'
#' @description
#' Load all data needed to perform an SCCS analysis from the database.
#'
#' @details
#' This function downloads several types of information:
#'
#' - Information on the occurrences of the outcome(s) of interest. Note that information for
#' multiple outcomes can be fetched in one go, and later the specific outcome can be specified
#' for which we want to build a model.
#' - Information on the observation time and age for the people with the outcomes.
#' - Information on exposures of interest which we want to include in the model.
#'
#' Five different database schemas can be specified, for five different types of information: The
#' - **cdmDatabaseSchema** is used to extract patient age and observation period. The
#' - **outcomeDatabaseSchema** is used to extract information about the outcomes, the
#' - **exposureDatabaseSchema** is used to retrieve information on exposures, and the
#' - **customCovariateDatabaseSchema** is optionally used to find additional, user-defined
#' covariates. All four locations could point to the same database schema.
#' - **nestingCohortDatabaseSchema** is optionally used to define a cohort in which the analysis is nested,
#' for example a cohort of diabetics patients.
#'
#' All five locations could point to the same database schema.
#'
#' @return
#' An [SccsData] object.
#'
#' @param connectionDetails               An R object of type `ConnectionDetails` created using
#'                                        the function [DatabaseConnector::createConnectionDetails()] function.
#' @param cdmDatabaseSchema               The name of the database schema that contains the OMOP CDM
#'                                        instance.  Requires read permissions to this database. On SQL
#'                                        Server, this should specify both the database and the
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
#' @param useNestingCohort                    Should the study be nested in a cohort (e.g. people with
#'                                            a specific indication)? If not, the study will be nested
#'                                            in the general population.
#' @param nestingCohortDatabaseSchema         The name of the database schema that is the location
#'                                            where the nesting cohort is defined.
#' @param nestingCohortTable                  Name of the table holding the nesting cohort. This table
#'                                            should have the same structure as the cohort table.
#' @param nestingCohortId                     A cohort definition ID identifying the records in the
#'                                            nestingCohortTable to use as nesting cohort.
#' @param deleteCovariatesSmallCount      The minimum count for a covariate to appear in the data to be
#'                                        kept.
#' @param studyStartDate                  A calendar date specifying the minimum date where data is
#'                                        used. Date format is 'yyyymmdd'.
#' @param studyEndDate                    A calendar date specifying the maximum date where data is
#'                                        used. Date format is 'yyyymmdd'.
#' @param cdmVersion                      Define the OMOP CDM version used: currently support "4" and
#'                                        "5".
#' @param maxCasesPerOutcome              If there are more than this number of cases for a single
#'                                        outcome cases will be sampled to this size. `maxCasesPerOutcome = 0`
#'                                        indicates no maximum size.
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
                          useNestingCohort = FALSE,
                          nestingCohortDatabaseSchema = cdmDatabaseSchema,
                          nestingCohortTable = "cohort",
                          nestingCohortId = NULL,
                          deleteCovariatesSmallCount = 100,
                          studyStartDate = "",
                          studyEndDate = "",
                          cdmVersion = "5",
                          maxCasesPerOutcome = 0) {
  if (studyStartDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyStartDate) == -1)
    stop("Study start date must have format YYYYMMDD")
  if (studyEndDate != "" && regexpr("^[12][0-9]{3}[01][0-9][0-3][0-9]$", studyEndDate) == -1)
    stop("Study end date must have format YYYYMMDD")
  if (cdmVersion == "4")
    stop("CDM version 4 is no longer supported")
  if (!is.null(exposureIds) && length(exposureIds) > 0 && !is.numeric(exposureIds))
    stop("exposureIds must be a (vector of) numeric")
  if (useCustomCovariates && !is.null(customCovariateIds) && length(customCovariateIds) > 0 && !is.numeric(customCovariateIds))
    stop("customCovariateIds must be a (vector of) numeric")

  start <- Sys.time()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  if (is.null(exposureIds) || length(exposureIds) == 0) {
    hasExposureIds <- FALSE
  } else {
    hasExposureIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#exposure_ids",
                                   data = data.frame(concept_id = as.integer(exposureIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }

  if (!useCustomCovariates || is.null(customCovariateIds) || length(customCovariateIds) == 0) {
    hasCustomCovariateIds <- FALSE
  } else {
    hasCustomCovariateIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#custom_cov_ids",
                                   data = data.frame(concept_id = as.integer(customCovariateIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   oracleTempSchema = oracleTempSchema)
  }

  ParallelLogger::logInfo("Selecting outcomes")
  sql <- SqlRender::loadRenderTranslateSql("SelectOutcomes.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           outcome_database_schema = outcomeDatabaseSchema,
                                           outcome_table = outcomeTable,
                                           outcome_concept_ids = outcomeIds,
                                           use_nesting_cohort = useNestingCohort,
                                           nesting_cohort_database_schema = nestingCohortDatabaseSchema,
                                           nesting_cohort_table = nestingCohortTable,
                                           nesting_cohort_id = nestingCohortId,
                                           study_start_date = studyStartDate,
                                           study_end_date = studyEndDate)
  DatabaseConnector::executeSql(conn, sql)

  ParallelLogger::logInfo("Creating cases")
  sql <- SqlRender::loadRenderTranslateSql("CreateCases.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           use_nesting_cohort = useNestingCohort,
                                           nesting_cohort_database_schema = nestingCohortDatabaseSchema,
                                           nesting_cohort_table = nestingCohortTable,
                                           nesting_cohort_id = nestingCohortId,
                                           study_start_date = studyStartDate,
                                           study_end_date = studyEndDate)
  DatabaseConnector::executeSql(conn, sql)

  DatabaseConnector::insertTable(conn,
                                 tableName = "#outcome_ids",
                                 data = data.frame(outcome_id = as.integer(outcomeIds)),
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = TRUE,
                                 oracleTempSchema = oracleTempSchema)

  ParallelLogger::logInfo("Counting outcomes")
  sql <- SqlRender::loadRenderTranslateSql("CountOutcomes.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           use_nesting_cohort = useNestingCohort,
                                           study_start_date = studyStartDate,
                                           study_end_date = studyEndDate)
  DatabaseConnector::executeSql(conn, sql)

  sql <- "SELECT * FROM #counts;"
  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)
  outcomeCounts <- as_tibble(DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE))


  sampledCases <- FALSE
  if (maxCasesPerOutcome != 0) {
    for (outcomeId in unique(outcomeCounts$outcomeId)) {
      count <- min(outcomeCounts$outcomeObsPeriods[outcomeCounts$outcomeId == outcomeId])
      if (count > maxCasesPerOutcome) {
        ParallelLogger::logInfo(paste0("Downsampling cases for outcome ID ", outcomeId, " from ", count, " to ", maxCasesPerOutcome))
        sampledCases <- TRUE
      }
    }
    if (sampledCases) {
      sql <- SqlRender::loadRenderTranslateSql("SampleCases.sql",
                                               packageName = "SelfControlledCaseSeries",
                                               dbms = connectionDetails$dbms,
                                               oracleTempSchema = oracleTempSchema,
                                               max_cases_per_outcome = maxCasesPerOutcome)
      DatabaseConnector::executeSql(conn, sql)
    }
  }

  ParallelLogger::logInfo("Creating eras")
  sql <- SqlRender::loadRenderTranslateSql("CreateEras.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           outcome_database_schema = outcomeDatabaseSchema,
                                           outcome_table = outcomeTable,
                                           outcome_concept_ids = outcomeIds,
                                           exposure_database_schema = exposureDatabaseSchema,
                                           exposure_table = exposureTable,
                                           use_nesting_cohort = useNestingCohort,
                                           use_custom_covariates = useCustomCovariates,
                                           custom_covariate_database_schema = customCovariateDatabaseSchema,
                                           custom_covariate_table = customCovariateTable,
                                           has_exposure_ids = hasExposureIds,
                                           has_custom_covariate_ids = hasCustomCovariateIds,
                                           delete_covariates_small_count = deleteCovariatesSmallCount,
                                           study_start_date = studyStartDate,
                                           study_end_date = studyEndDate,
                                           sampled_cases = sampledCases)
  DatabaseConnector::executeSql(conn, sql)

  ParallelLogger::logInfo("Fetching data from server")
  sccsData <- Andromeda::andromeda()
  sql <- SqlRender::loadRenderTranslateSql("QueryCases.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           sampled_cases = sampledCases)
  DatabaseConnector::querySqlToAndromeda(connection = conn,
                                         sql = sql,
                                         andromeda = sccsData,
                                         andromedaTableName = "cases",
                                         snakeCaseToCamelCase = TRUE)

  ParallelLogger::logDebug("Fetched ", sccsData$cases %>% count() %>% pull(), " cases from server")

  countNegativeAges <- sccsData$cases %>%
    filter(.data$ageInDays < 0) %>%
    count() %>%
    pull()

  if (countNegativeAges > 0) {
    warning("There are ", countNegativeAges, " cases with negative ages. Setting their starting age to 0. Please review your data.")
    sccsData$cases <- sccsData$cases %>%
      mutate(ageInDays = case_when(.data$ageInDays < 0 ~ 0,
                                   TRUE ~ .data$ageInDays))
  }

  sql <- SqlRender::loadRenderTranslateSql("QueryEras.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema)
  DatabaseConnector::querySqlToAndromeda(connection = conn,
                                         sql = sql,
                                         andromeda = sccsData,
                                         andromedaTableName = "eras",
                                         snakeCaseToCamelCase = TRUE)

  sql <- "SELECT era_type, era_id, era_name FROM #era_ref"
  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms, oracleTempSchema = oracleTempSchema)
  DatabaseConnector::querySqlToAndromeda(connection = conn,
                                         sql = sql,
                                         andromeda = sccsData,
                                         andromedaTableName = "eraRef",
                                         snakeCaseToCamelCase = TRUE)

  # Delete temp tables
  sql <- SqlRender::loadRenderTranslateSql("RemoveTempTables.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           oracleTempSchema = oracleTempSchema,
                                           study_start_date = studyStartDate,
                                           study_end_date = studyEndDate,
                                           sampled_cases = sampledCases,
                                           has_exposure_ids = hasExposureIds,
                                           use_nesting_cohort = useNestingCohort,
                                           has_custom_covariate_ids = hasCustomCovariateIds)
  DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

  if  (sampledCases) {
    sampledCounts <- sccsData$eras %>%
      filter(.data$eraType == "hoi") %>%
      inner_join(sccsData$cases, by = "caseId") %>%
      group_by(.data$eraId) %>%
      summarise(outcomeSubjects = n_distinct(.data$personId),
                outcomeEvents = count(),
                outcomeObsPeriods = n_distinct(.data$observationPeriodId),
                .groups = "drop_last") %>%
      rename(outcomeId = .data$eraId) %>%
      mutate(description = "Random sample") %>%
      collect()
    if (nrow(sampledCounts) > 0) {
      # If no rows then description becomes a logical, causing an error here:
      outcomeCounts <- bind_rows(outcomeCounts, sampledCounts)
    }
  }

  attr(sccsData, "metaData") <- list(exposureIds = exposureIds,
                                     outcomeIds = outcomeIds,
                                     attrition = outcomeCounts)
  class(sccsData) <- "SccsData"
  attr(class(sccsData), "package") <- "SelfControlledCaseSeries"

  delta <- Sys.time() - start
  ParallelLogger::logInfo("Getting SCCS data from server took ", signif(delta, 3), " ", attr(delta, "units"))

  return(sccsData)
}
