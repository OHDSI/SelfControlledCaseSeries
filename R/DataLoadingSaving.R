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
#' Cohort tables are assumed to have the following fields: `cohort_definition_id`, `subject_id`,
#' `cohort_start_date`, and `cohort_end_date.`
#'
#' @section Study period start and end dates:
#' A study start and end date define a period when patient data will be included in the
#' analysis. Multiple non-overlapping periods can be defined, which for example will allow
#' for excluding the time of the COVID pandemic, when most outcome rates were unstable.
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
#' @param tempEmulationSchema             Some database platforms like Oracle and Impala do not truly support
#'                                        temp tables. To emulate temp tables, provide a schema with write
#'                                        privileges where temp tables can be created.
#' @param outcomeDatabaseSchema           The name of the database schema that is the location where
#'                                        the data used to define the outcome cohorts is available. If
#'                                        `outcomeTable = "condition_era"`, `outcomeDatabaseSchema` is not
#'                                        used.  Requires read permissions to this database.
#' @param outcomeTable                    The table name that contains the outcome cohorts.  If
#'                                        `outcomeTable` is not `"condition_era"`,
#'                                        then expectation is `outcomeTable` has format of cohort table (see
#'                                        details).
#' @param outcomeIds                      A list of IDs used to define outcomes.  If `outcomeTable` is not
#'                                        `"condition_era"` the list contains records found in the
#'                                        `cohort_definition_id` field.
#' @param exposureDatabaseSchema          The name of the database schema that is the location where
#'                                        the exposure data used to define the exposure eras is
#'                                        available. If `exposureTable = "drug_era"`,
#'                                        `exposureDatabaseSchema` is not used but assumed to be equal to
#'                                        `cdmDatabaseSchema`.  Requires read permissions to this database.
#' @param exposureTable                   The tablename that contains the exposure cohorts.  If
#'                                        `exposureTable` is not "drug_era", then expectation is `exposureTable`
#'                                        has format of a cohort table (see details).
#' @param customCovariateDatabaseSchema   The name of the database schema that is the location where
#'                                        the custom covariate data is available.
#' @param customCovariateTable            Name of the table holding the custom covariates. This table
#'                                        should have the same structure as the cohort table (see details).
#' @param nestingCohortDatabaseSchema     The name of the database schema that is the location
#'                                        where the nesting cohort is defined.
#' @param nestingCohortTable              Name of the table holding the nesting cohort. This table
#'                                        should have the same structure as the cohort table (see details).
#' @param getDbSccsDataArgs               An object of type `GetDbSccsDataArgs` as created by the `createGetDbSccsDataArgs()`
#'                                        function.
#'
#' @export
getDbSccsData <- function(connectionDetails,
                          cdmDatabaseSchema,
                          tempEmulationSchema = getOption("sqlRenderTempEmulationSchema"),
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "condition_era",
                          outcomeIds,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          customCovariateDatabaseSchema = cdmDatabaseSchema,
                          customCovariateTable = "cohort",
                          nestingCohortDatabaseSchema = cdmDatabaseSchema,
                          nestingCohortTable = "cohort",
                          getDbSccsDataArgs
) {
  errorMessages <- checkmate::makeAssertCollection()
  if (is(connectionDetails, "connectionDetails")) {
    checkmate::assertClass(connectionDetails, "connectionDetails", add = errorMessages)
  } else {
    checkmate::assertClass(connectionDetails, "ConnectionDetails", add = errorMessages)
  }
  checkmate::assertCharacter(cdmDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(tempEmulationSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(outcomeDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(outcomeTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureDatabaseSchema, len = 1, add = errorMessages)
  checkmate::assertCharacter(exposureTable, len = 1, add = errorMessages)
  checkmate::assertCharacter(customCovariateDatabaseSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(customCovariateTable, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(nestingCohortDatabaseSchema, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertCharacter(nestingCohortTable, len = 1, null.ok = TRUE, add = errorMessages)
  checkmate::assertClass(getDbSccsDataArgs, "GetDbSccsDataArgs", add = errorMessages)
  checkmate::assertIntegerish(getDbSccsDataArgs$exposureIds, null.ok = TRUE, add = errorMessages)
  checkmate::reportAssertions(collection = errorMessages)

  useNestingCohort <- !is.null(getDbSccsDataArgs$nestingCohortId)
  useCustomCovariates <- !is.null(getDbSccsDataArgs$customCovariateIds)
  DatabaseConnector::assertTempEmulationSchemaSet(dbms = connectionDetails$dbms,
                                                  tempEmulationSchema = tempEmulationSchema)

  start <- Sys.time()
  conn <- DatabaseConnector::connect(connectionDetails)
  on.exit(DatabaseConnector::disconnect(conn))

  if (is.null(getDbSccsDataArgs$exposureIds) || length(getDbSccsDataArgs$exposureIds) == 0) {
    hasExposureIds <- FALSE
  } else {
    hasExposureIds <- TRUE
    DatabaseConnector::insertTable(conn,
                                   tableName = "#exposure_ids",
                                   data = data.frame(concept_id = as.integer(getDbSccsDataArgs$exposureIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   tempEmulationSchema = tempEmulationSchema
    )
  }
  DatabaseConnector::insertTable(conn,
                                 tableName = "#outcome_ids",
                                 data = data.frame(outcome_id = as.integer(outcomeIds)),
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = TRUE,
                                 tempEmulationSchema = tempEmulationSchema
  )

  if (useCustomCovariates) {
    DatabaseConnector::insertTable(conn,
                                   tableName = "#custom_cov_ids",
                                   data = data.frame(concept_id = as.integer(customCovariateIds)),
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   tempEmulationSchema = tempEmulationSchema,
                                   camelCaseToSnakeCase = TRUE
    )
  }

  if (length(getDbSccsDataArgs$studyStartDates) == 0) {
    hasStudyPeriods <- FALSE
    studyPeriods <- NULL
  } else {
    hasStudyPeriods <- TRUE
    studyPeriods <- tibble(studyStartDate = getDbSccsDataArgs$studyStartDates,
                           studyEndDate = getDbSccsDataArgs$studyEndDates) |>
      mutate(studyStartDate = if_else(.data$studyStartDate == "", "18000101", .data$studyStartDate),
             studyEndDate = if_else(.data$studyEndDate == "", "220000101", .data$studyEndDate)) |>
      mutate(studyStartDate = as.Date(.data$studyStartDate, format = "%Y%m%d"),
             studyEndDate = as.Date(.data$studyEndDate, format = "%Y%m%d"))
    DatabaseConnector::insertTable(conn,
                                   tableName = "#study_periods",
                                   data = studyPeriods,
                                   dropTableIfExists = TRUE,
                                   createTable = TRUE,
                                   tempTable = TRUE,
                                   tempEmulationSchema = tempEmulationSchema,
                                   camelCaseToSnakeCase = TRUE
    )
  }

  message("Selecting outcomes")
  sql <- SqlRender::loadRenderTranslateSql("SelectOutcomes.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           tempEmulationSchema = tempEmulationSchema,
                                           outcome_database_schema = outcomeDatabaseSchema,
                                           outcome_table = outcomeTable
  )
  DatabaseConnector::executeSql(conn, sql)

  message("Creating cases")
  sql <- SqlRender::loadRenderTranslateSql("CreateCases.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           tempEmulationSchema = tempEmulationSchema,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           use_nesting_cohort = useNestingCohort,
                                           nesting_cohort_database_schema = nestingCohortDatabaseSchema,
                                           nesting_cohort_table = nestingCohortTable,
                                           nesting_cohort_id = getDbSccsDataArgs$nestingCohortId,
                                           has_study_periods = hasStudyPeriods
  )
  if (useNestingCohort) {
    caseTable <- "#cases_in_nesting"
  } else if (hasStudyPeriods) {
    caseTable <- "#cases_in_periods"
  } else {
    caseTable <- "#cases"
  }
  DatabaseConnector::executeSql(conn, sql)

  message("Counting outcomes")
  outcomeCounts <- countOutcomesInDb(
    connection = conn,
    hasStudyPeriods = hasStudyPeriods,
    useNestingCohort = useNestingCohort,
    tempEmulationSchema = tempEmulationSchema)

  # Need to estimate prevalence to evaluate the rare outcome assumption:
  message("Counting base population for estimating prevalence")
  sql <- SqlRender::loadRenderTranslateSql("CountBasePopulation.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           cdm_database_schema = cdmDatabaseSchema,
                                           use_nesting_cohort = useNestingCohort,
                                           nesting_cohort_database_schema = nestingCohortDatabaseSchema,
                                           nesting_cohort_table = nestingCohortTable,
                                           nesting_cohort_id = getDbSccsDataArgs$nestingCohortId,
                                           has_study_periods = hasStudyPeriods)
  basePopulationCount <- DatabaseConnector::querySql(conn, sql, snakeCaseToCamelCase = TRUE)
  prevalences <- outcomeCounts |>
    group_by(.data$outcomeId) |>
    summarise(outcomeSubjects = min(.data$outcomeSubjects),
              outcomeEvents = min(.data$outcomeEvents)) |>
    transmute(outcomeId = .data$outcomeId,
              outcomeProportion = .data$outcomeSubjects / basePopulationCount$windowCount,
              outcomeRate = .data$outcomeEvents / basePopulationCount$daysCount,
              probablyFirstOutcomeOnly = .data$outcomeSubjects == .data$outcomeEvents)

  sampledCases <- FALSE
  if (getDbSccsDataArgs$maxCasesPerOutcome != 0) {
    for (outcomeId in unique(outcomeCounts$outcomeId)) {
      count <- min(outcomeCounts$outcomeObsPeriods[outcomeCounts$outcomeId == outcomeId])
      if (count > getDbSccsDataArgs$maxCasesPerOutcome) {
        message(paste0("Downsampling cases for outcome ID ", outcomeId, " from ", count, " to ", getDbSccsDataArgs$maxCasesPerOutcome))
        sampledCases <- TRUE
      }
    }
    if (sampledCases) {
      sql <- SqlRender::loadRenderTranslateSql("SampleCases.sql",
                                               packageName = "SelfControlledCaseSeries",
                                               dbms = connectionDetails$dbms,
                                               tempEmulationSchema = tempEmulationSchema,
                                               max_cases_per_outcome = getDbSccsDataArgs$maxCasesPerOutcome,
                                               case_table = caseTable)
      DatabaseConnector::executeSql(conn, sql)
      caseTable <- "#sampled_cases"
    }
  }

  message("Creating eras")
  sql <- SqlRender::loadRenderTranslateSql("CreateEras.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           tempEmulationSchema = tempEmulationSchema,
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
                                           has_custom_covariate_ids = useCustomCovariates,
                                           delete_covariates_small_count = getDbSccsDataArgs$deleteCovariatesSmallCount,
                                           case_table = caseTable
  )
  DatabaseConnector::executeSql(conn, sql)

  message("Fetching data from server")
  sccsData <- Andromeda::andromeda()
  sql <- SqlRender::loadRenderTranslateSql(
    "QueryCases.sql",
    packageName = "SelfControlledCaseSeries",
    dbms = connectionDetails$dbms,
    tempEmulationSchema = tempEmulationSchema,
    case_table = caseTable
  )
  DatabaseConnector::querySqlToAndromeda(
    connection = conn,
    sql = sql,
    andromeda = sccsData,
    andromedaTableName = "cases",
    snakeCaseToCamelCase = TRUE
  )
  ParallelLogger::logDebug("Fetched ", sccsData$cases |> count() |> pull(), " cases from server")
  sccsData <- ensureAgePositive(sccsData)

  sql <- SqlRender::loadRenderTranslateSql("QueryEras.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           tempEmulationSchema = tempEmulationSchema
  )
  DatabaseConnector::querySqlToAndromeda(
    connection = conn,
    sql = sql,
    andromeda = sccsData,
    andromedaTableName = "eras",
    snakeCaseToCamelCase = TRUE
  )

  sql <- "SELECT * FROM #era_ref"
  sql <- SqlRender::translate(sql = sql, targetDialect = connectionDetails$dbms, tempEmulationSchema = tempEmulationSchema)
  DatabaseConnector::querySqlToAndromeda(
    connection = conn,
    sql = sql,
    andromeda = sccsData,
    andromedaTableName = "eraRef",
    snakeCaseToCamelCase = TRUE
  )

  # Delete temp tables
  sql <- SqlRender::loadRenderTranslateSql("RemoveTempTables.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = connectionDetails$dbms,
                                           tempEmulationSchema = tempEmulationSchema,
                                           has_study_periods = hasStudyPeriods,
                                           sampled_cases = sampledCases,
                                           has_exposure_ids = hasExposureIds,
                                           use_nesting_cohort = useNestingCohort,
                                           has_custom_covariate_ids = useCustomCovariates
  )
  DatabaseConnector::executeSql(conn, sql, progressBar = FALSE, reportOverallTime = FALSE)

  if (sampledCases) {
    sampledCounts <- sccsData$eras |>
      filter(.data$eraType == "hoi") |>
      inner_join(sccsData$cases, by = join_by("caseId")) |>
      group_by(.data$eraId) |>
      summarise(
        outcomeSubjects = n_distinct(.data$personId),
        outcomeEvents = count(),
        outcomeObsPeriods = n_distinct(.data$observationPeriodId),
        observedDays = sum(.data$endDay - .data$startDay + 1, na.rm = TRUE),
        .groups = "drop_last"
      ) |>
      rename(outcomeId = "eraId") |>
      mutate(description = "Random sample") |>
      collect()
    if (nrow(sampledCounts) > 0) {
      # If no rows then description becomes a logical, causing an error here:
      outcomeCounts <- bind_rows(outcomeCounts, sampledCounts)
    }
  }

  attr(sccsData, "metaData") <- list(
    exposureIds = getDbSccsDataArgs$exposureIds,
    outcomeIds = outcomeIds,
    attrition = outcomeCounts,
    studyPeriods = studyPeriods,
    prevalences = prevalences
  )
  class(sccsData) <- "SccsData"
  attr(class(sccsData), "package") <- "SelfControlledCaseSeries"

  delta <- Sys.time() - start
  message("Getting SCCS data from server took ", signif(delta, 3), " ", attr(delta, "units"))
  return(sccsData)
}

countOutcomesInDb <- function(connection, hasStudyPeriods, useNestingCohort, tempEmulationSchema) {
  sql <- SqlRender::loadRenderTranslateSql("CountOutcomes.sql",
                                           packageName = "SelfControlledCaseSeries",
                                           dbms = DatabaseConnector::dbms(connection),
                                           tempEmulationSchema = tempEmulationSchema,
                                           case_table = "#cases")
  outcomeCounts <- DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE) |>
    mutate(description = "All outcome occurrences")
  if (hasStudyPeriods) {
    sql <- SqlRender::loadRenderTranslateSql("CountOutcomes.sql",
                                             packageName = "SelfControlledCaseSeries",
                                             dbms = DatabaseConnector::dbms(connection),
                                             tempEmulationSchema = tempEmulationSchema,
                                             case_table = "#cases_in_periods")
    outcomeCounts <- outcomeCounts |>
      bind_rows(
        DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE) |>
          mutate(description = "Outcomes in study period(s)")
      )
  }
  if (useNestingCohort) {
    sql <- SqlRender::loadRenderTranslateSql("CountOutcomes.sql",
                                             packageName = "SelfControlledCaseSeries",
                                             dbms = DatabaseConnector::dbms(connection),
                                             tempEmulationSchema = tempEmulationSchema,
                                             case_table = "#cases_in_nesting")
    outcomeCounts <- outcomeCounts |>
      bind_rows(
        DatabaseConnector::querySql(connection, sql, snakeCaseToCamelCase = TRUE) |>
          mutate(description = "Outcomes in nesting cohort")
      )
  }
  return(outcomeCounts)
}

ensureAgePositive <- function(sccsData) {
  # If there are no cases, some platforms will convert ageAtObsStart to logical,
  # which will throw an error when compared to 0 on DuckDb:
  if (pull(count(sccsData$cases)) == 0) {
    return(sccsData)
  }
  countNegativeAges <- sccsData$cases |>
    filter(.data$ageAtObsStart < 0) |>
    count() |>
    pull()

  if (countNegativeAges > 0) {
    warning("There are ", countNegativeAges, " cases with negative ages at observation start. Setting their starting age to 0. Please review your data.")
    sccsData$cases <- sccsData$cases |>
      mutate(ageAtObsStart = case_when(
        .data$ageAtObsStart < 0 ~ 0,
        TRUE ~ .data$ageAtObsStart
      ))
  }
  return(sccsData)
}
