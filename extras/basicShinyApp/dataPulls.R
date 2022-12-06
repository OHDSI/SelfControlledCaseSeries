getExposuresOutcomes <- function(connectionPool, resultsDatabaseSchema, includeControls = FALSE) {
  # Note: probably a bad idea to use the connectionPool directly to create dbplyr table objects that later will
  # need to be joined, so checking out a stable connection from the pool:
  connection <- pool::poolCheckout(connectionPool)
  on.exit(pool::poolReturn(connection))

  sccsExposuresOutcomeSet <- tbl(connection, inDatabaseSchema(resultsDatabaseSchema, "sccs_exposures_outcome_set"))
  sccsExposure <- tbl(connection, inDatabaseSchema(resultsDatabaseSchema, "sccs_exposure"))
  cohortDefinition <- tbl(connection, inDatabaseSchema(resultsDatabaseSchema, "cg_cohort_definition"))
  sccsEra <- tbl(connection, inDatabaseSchema(resultsDatabaseSchema, "sccs_era"))

  exposureOutcomes <- sccsExposuresOutcomeSet %>%
    inner_join(sccsExposure %>%
                 rename(exposure_id = "era_id"),
               by = "exposures_outcome_set_id") %>%
    inner_join(cohortDefinition %>%
                 select(outcome_id = "cohort_definition_id",
                        outcome_name = "cohort_name"),
               by = "outcome_id") %>%
    left_join(
      sccsEra %>%
        rename(exposure_id = "era_id",
               exposure_name = "era_name") %>%
        distinct(exposures_outcome_set_id, exposure_id, exposure_name),
      by = c("exposures_outcome_set_id","exposure_id")) %>%
    left_join(cohortDefinition %>%
                select(exposure_id = "cohort_definition_id",
                       exposure_name_2 = "cohort_name"),

              by = "exposure_id") %>%
    mutate(exposure_name = if_else(is.na(exposure_name), exposure_name_2, exposure_name)) %>%
    select(-"exposure_name_2")


  if (!includeControls) {
    exposureOutcomes <- exposureOutcomes %>%
      filter(is.na(.data$true_effect_size))
  }
  exposureOutcomes %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}

getTable <- function(connectionPool, resultsDatabaseSchema, tableName) {
  tbl(connectionPool, inDatabaseSchema(resultsDatabaseSchema, tableName)) %>%
    collect() %>%
    SqlRender::snakeCaseToCamelCaseNames() %>%
    return()
}
