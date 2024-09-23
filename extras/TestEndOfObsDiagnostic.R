library(SelfControlledCaseSeries)
options(andromedaTempFolder = "e:/andromedaTemp")

folder <- "e:/temp/edoTest"
connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3045"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "sccs_edo_test"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")

# Create outcome cohorts -------------------------------------------------------
outcomes <- tibble(
  outcomeId = c(2072,
                2088,
                3429,
                10650,
                12480,
                14964,
                16881,
                16916),
  outcomeName = c("AMI",
                  "Hemorrhagic stroke",
                  "Suicide ideation, attempt, including drug poisoning",
                  "COVID-19",
                  "End-stage renal disease",
                  "Earliest event of Non-small cell lung cancer (NSCLC), with two diagnosis",
                  "Motion sickness",
                  "Multiple myeloma")
)

ROhdsiWebApi::authorizeWebApi(baseUrl = Sys.getenv("baseUrl"),
                              authMethod = "windows")
cohorts <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = Sys.getenv("baseUrl"),
                                                   cohortIds = outcomes$outcomeId)

connection <- DatabaseConnector::connect(connectionDetails)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
CohortGenerator::createCohortTables(connection = connection,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTableNames = cohortTableNames)
counts <- CohortGenerator::generateCohortSet(connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTableNames = cohortTableNames,
                                             cohortDefinitionSet = cohorts)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id;"
sql <- SqlRender::render(sql,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)


# Compute statistics -----------------------------------------------------------
aspirin <- 1112807

if (!file.exists(folder))
  dir.create(folder)

sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortTable,
                          outcomeIds = outcomes$outcomeId,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = aspirin,
                          studyStartDates = "20100101",
                          studyEndDates = "21000101",
                          maxCasesPerOutcome = 100000)
saveSccsData(sccsData, file.path(folder, "data1.zip"))
sccsData <- loadSccsData(file.path(folder, "data1.zip"))

i <- 1
for (i in seq_len(nrow(outcomes))) {
  studyPop <- createStudyPopulation(sccsData = sccsData,
                                    outcomeId = outcomes$outcomeId[i],
                                    firstOutcomeOnly = FALSE,
                                    naivePeriod = 180)
  covarAspirin <- createEraCovariateSettings(label = "Exposure of interest",
                                             includeEraIds = aspirin,
                                             start = 0,
                                             end = 0,
                                             endAnchor = "era end")
  sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                             sccsData,
                                             eraCovariateSettings = covarAspirin)
  model <- fitSccsModel(sccsIntervalData)
  p <- computeEventDependentObservationP(model)
  outcomes$p[i] <- sprintf("%0.4f", p)
  outcomes$estimate[i] <- model$estimates |>
    filter(covariateId == 99) |>
    mutate(estimate = sprintf("%0.2f (%0.2f - %0.2f)", exp(logRr), exp(logLb95), exp(logUb95))) |>
    pull(estimate)
}
outcomes

readr::write_csv(outcomes, file.path(folder,"Results.csv"))
