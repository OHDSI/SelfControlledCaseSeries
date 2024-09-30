library(SelfControlledCaseSeries)
options(andromedaTempFolder = "e:/andromedaTemp")

folder <- "e:/temp/edoTest"
connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
databases <- tibble(
  name = c("AustraliaLpd",
           "CCAE",
           "FranceDa",
           "MDCD",
           "MDCR",
           "Pharmetrics",
           "OptumDoD",
           "OptumEhr",
           "JMDC"),
  cdmDatabaseSchema = c("iqvia_australia.cdm_iqvia_australia_v3006",
                        "merative_ccae.cdm_merative_ccae_v3046",
                        "iqvia_france.cdm_iqvia_france_v2914",
                        "merative_mdcd.cdm_merative_mdcd_v3038",
                        "merative_mdcr.cdm_merative_mdcr_v3045",
                        "iqvia_pharmetrics.cdm_iqvia_pharmetrics_v3043",
                        "optum_extended_dod.cdm_optum_extended_dod_v3039",
                        "optum_ehr.cdm_optum_ehr_v3037",
                        "jmdc.cdm_jmdc_v3044")
) |>
  mutate(cohortTable = paste("sccs_edo_test", name, sep = "_"),
         cohortDatabaseSchema = "scratch.scratch_mschuemi")

outcomes <- tibble(
  outcomeId = c(2072,
                2088,
                3429,
                10650,
                12480,
                10707,
                14964,
                17151,
                16751,
                16916,
                18523,
                10739,
                2081,
                14027),
  outcomeName = c("AMI",
                  "Hemorrhagic stroke",
                  "Suicide ideation, attempt",
                  "COVID-19",
                  "End-stage renal disease",
                  "Sudden cardiac arrest or death",
                  "Non-small cell lung cancer (NSCLC)",
                  "GI bleeding",
                  "Motion sickness",
                  "Multiple myeloma",
                  "Acute hepatic failure",
                  "Epistaxis",
                  "Narcolepsy",
                  "Hip fracture"),
  covid = c(FALSE,
            FALSE,
            FALSE,
            TRUE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE,
            FALSE),
  firstOnly = c(TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                FALSE,
                TRUE,
                TRUE,
                FALSE,
                TRUE,
                TRUE)
)

# Create outcome cohorts -------------------------------------------------------
ROhdsiWebApi::authorizeWebApi(baseUrl = Sys.getenv("baseUrl"),
                              authMethod = "windows")
cohorts <- ROhdsiWebApi::exportCohortDefinitionSet(baseUrl = Sys.getenv("baseUrl"),
                                                   cohortIds = outcomes$outcomeId)

connection <- DatabaseConnector::connect(connectionDetails)

dbi = 1
for (dbi in 1:nrow(databases)) {
  database <- databases[dbi, ]
  writeLines(sprintf("*** Creating outcomes in %s ***", database$name))
  cohortTableNames <- CohortGenerator::getCohortTableNames(database$cohortTable)
  CohortGenerator::createCohortTables(connection = connection,
                                      cohortDatabaseSchema = database$cohortDatabaseSchema,
                                      cohortTableNames = cohortTableNames)
  counts <- CohortGenerator::generateCohortSet(connection = connection,
                                               cdmDatabaseSchema = database$cdmDatabaseSchema,
                                               cohortDatabaseSchema = database$cohortDatabaseSchema,
                                               cohortTableNames = cohortTableNames,
                                               cohortDefinitionSet = cohorts)

  # Check number of subjects per cohort:
  sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id;"
  sql <- SqlRender::render(sql,
                           cohortDatabaseSchema = database$cohortDatabaseSchema,
                           cohortTable = database$cohortTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  DatabaseConnector::querySql(connection, sql)
}
DatabaseConnector::disconnect(connection)


# Run analyses -----------------------------------------------------------------
aspirin <- 1112807

if (!file.exists(folder))
  dir.create(folder)
connection <- DatabaseConnector::connect(connectionDetails)

for (dbi in 1:nrow(databases)) {
  database <- databases[dbi, ]
  writeLines(sprintf("***Performing analyses in %s ***", database$name))
  fileName <- file.path(folder, sprintf("SccsDataCovid_%s.zip", database$name))
  if (!file.exists(fileName)) {
    sccsDataCovid <- getDbSccsData(connectionDetails = connectionDetails,
                                   cdmDatabaseSchema = database$cdmDatabaseSchema,
                                   outcomeDatabaseSchema = database$cohortDatabaseSchema,
                                   outcomeTable = database$cohortTable,
                                   outcomeIds = outcomes$outcomeId[outcomes$covid],
                                   exposureDatabaseSchema = database$cdmDatabaseSchema,
                                   exposureTable = "drug_era",
                                   exposureIds = aspirin,
                                   studyStartDates = "20200101",
                                   studyEndDates = "20221231",
                                   maxCasesPerOutcome = 100000)
    saveSccsData(sccsDataCovid, fileName)
  }
  sccsDataCovid <- loadSccsData(fileName)

  fileName <- file.path(folder, sprintf("SccsDataNonCovid_%s.zip", database$name))
  if (!file.exists(fileName)) {
    sccsDataNonCovid <- getDbSccsData(connectionDetails = connectionDetails,
                                      cdmDatabaseSchema = database$cdmDatabaseSchema,
                                      outcomeDatabaseSchema = database$cohortDatabaseSchema,
                                      outcomeTable = database$cohortTable,
                                      outcomeIds = outcomes$outcomeId[!outcomes$covid],
                                      exposureDatabaseSchema = database$cdmDatabaseSchema,
                                      exposureTable = "drug_era",
                                      exposureIds = aspirin,
                                      studyStartDates = c("20100101", "20220101"),
                                      studyEndDates =  c("20191231", "21001231"),
                                      maxCasesPerOutcome = 100000)
    saveSccsData(sccsDataNonCovid, fileName)
  }
  sccsDataNonCovid <- loadSccsData(fileName)

  outcomes$p <- NA
  outcomes$estimate <- NA
  outcomes$cases <- NA
  i <- 1
  for (i in seq_len(nrow(outcomes))) {
    outcome <- outcomes[i, ]
    fileName <- file.path(folder, sprintf("SccsModel_o%d_%s.rds", outcome$outcomeId, database$name))
    if (file.exists(fileName)) {
      model <- readRDS(fileName)
    } else {
      if (outcome$covid) {
        d <- sccsDataCovid
      } else {
        d <- sccsDataNonCovid
      }
      studyPop <- createStudyPopulation(sccsData = d,
                                        outcomeId = outcome$outcomeId,
                                        firstOutcomeOnly = outcome$firstOnly,
                                        naivePeriod = 180)
      covarAspirin <- createEraCovariateSettings(label = "Exposure of interest",
                                                 includeEraIds = aspirin,
                                                 start = 0,
                                                 end = 0,
                                                 endAnchor = "era end")
      covarPreAspirin <- createEraCovariateSettings(label = "Pre-exposure",
                                                    includeEraIds = aspirin,
                                                    start = -60,
                                                    end = -1,
                                                    endAnchor = "era start")
      sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                                 d,
                                                 seasonalityCovariateSettings = createSeasonalityCovariateSettings(),
                                                 calendarTimeCovariateSettings = createCalendarTimeCovariateSettings(),
                                                 eraCovariateSettings = list(covarPreAspirin, covarAspirin),
                                                 endOfObservationEraLength = 30)
      control <- createControl(cvType = "auto",
                               selectorType = "byPid",
                               startingVariance = 0.1,
                               seed = 1,
                               resetCoefficients = TRUE,
                               noiseLevel = "quiet",
                               threads = 4)
      # exclude <- sccsIntervalData$covariateRef |>
      #   pull(covariateId)
      # exclude <- c(1000, 1001)#exclude[exclude != 99]
      # prior <- createPrior(priorType = "laplace", variance = 0.1, exclude = exclude)
      model <- fitSccsModel(sccsIntervalData, control = control, profileBounds = NULL)
      saveRDS(model, fileName)
    }
    p <- computeEventDependentObservationP(model)
    # plotEventObservationDependence(studyPop)
    # plotEventToCalendarTime(studyPop, model)
    # stability <- computeTimeStability(studyPop, model)
    # plotCalendarTimeEffect(model)
    # plotSeasonality(model)
    outcomes$p[i] <- sprintf("%0.4f", p)
    if (!is.null(model$estimates)) {
      outcomes$estimate[i] <- model$estimates |>
        filter(covariateId == 99) |>
        mutate(estimate = sprintf("%0.2f (%0.2f - %0.2f)", exp(logRr), exp(logLb95), exp(logUb95))) |>
        pull(estimate)
    }
    outcomes$cases[i] <- min(model$metaData$attrition$outcomeSubjects)
  }
  outcomes <- outcomes |>
    mutate(database = !!database$name)
  saveRDS(outcomes, file.path(folder, sprintf("Results_%s.rds", database$name)))
}
DatabaseConnector::disconnect(connection)

# Combine results --------------------------------------------------------------
results <- lapply(databases$name, function(name) readRDS(file.path(folder, sprintf("Results_%s.rds", name))))
results <- bind_rows(results)
readr::write_csv(results, file.path(folder,"Results.csv"))

# Compute incidence rate per month ---------------------------------------------
library(dplyr)
library(ggplot2)

startDates <-  seq(as.Date("2010-01-01"),
                   as.Date("2025-01-01"),
                   by = "month")
months <- tibble(
  startDate = startDates[-length(startDates)],
  endDate = startDates[-1] - 1
)

connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::insertTable(connection = connection,
                               tableName = "#months",
                               data = months,
                               camelCaseToSnakeCase = TRUE,
                               createTable =  TRUE)
sql <- "
  SELECT patient_time.start_date,
    patient_time.end_date,
    days,
    month_days,
    outcomes
  FROM (
    SELECT start_date,
        end_date,
        SUM(DATEDIFF(DAY,
                     CASE WHEN observation_period_start_date < start_date THEN start_date ELSE observation_period_start_date END,
                     CASE WHEN observation_period_end_date > end_date THEN end_date ELSE observation_period_end_date END) + 1) AS days,
        SUM(DATEDIFF(DAY, start_date, end_date) + 1) AS month_days
    FROM @cdm_database_schema.observation_period
    INNER JOIN #months
      ON start_date <= observation_period_end_date
          AND end_date >= observation_period_start_date
    GROUP BY start_date,
      end_date
  ) patient_time
  LEFT JOIN
  (
    SELECT start_date,
      end_date,
      COUNT(*) as outcomes
    FROM @cohort_database_schema.@cohort_table outcome
    INNER JOIN #months
      ON start_date <= cohort_start_date
        AND end_date >= cohort_start_date
    WHERE outcome.cohort_definition_id = @outcome_id
    GROUP BY start_date,
      end_date
  ) outcome_counts
    ON patient_time.start_date = outcome_counts.start_date
      AND patient_time.end_date = outcome_counts.end_date;
"
data <- DatabaseConnector::renderTranslateQuerySql(connection,
                                                   sql,
                                                   cdm_database_schema = database$cdmDatabaseSchema,
                                                   cohort_database_schema = database$cohortDatabaseSchema,
                                                   cohort_table = database$cohortTable,
                                                   outcome_id = outcome$outcomeId,
                                                   snakeCaseToCamelCase = TRUE)
alpha = 0.05
data <- data |>
  mutate(centuries = days/36523) |>
  mutate(ir = outcomes/centuries,
         irLb = qchisq(alpha / 2, 2 * outcomes) / (2 * centuries),
         irUb = qchisq(1 - alpha / 2, 2 * (outcomes + 1)) / (2 * centuries)) |>
  mutate(irNoCensor = outcomes/(monthDays/36525)) |>
  arrange(startDate)

ggplot(data, aes(x = startDate + 15, y = ir)) +
  geom_ribbon(aes(ymin = irLb, ymax = irUb), alpha = 0.2) +
  geom_line() +
  geom_point()

# Simulations --------------------------------------------------------------------------------------
library(SelfControlledCaseSeries)

rw <- createSimulationRiskWindow(start = 0,
                                 end = 0,
                                 endAnchor = "era end",
                                 relativeRisks = 2)

settings <- createSccsSimulationSettings(minBaselineRate = 0.001,
                                         maxBaselineRate = 0.01,
                                         eraIds = 1,
                                         simulationRiskWindows = list(rw),
                                         includeAgeEffect = FALSE,
                                         includeSeasonality = FALSE,
                                         includeCalendarTimeEffect = FALSE)

simulateOne <- function(seed, settings, censorFunction) {
  set.seed(seed)
  sccsData <- simulateSccsData(1000, settings)
  sccsData$cases |>
    count()
  firstOutcomeEra <- sccsData$eras |>
    filter(eraId == 10) |>
    group_by(caseId) |>
    filter(row_number(eraStartDay) == 1) |>
    ungroup() |>
    select(caseId, outcomeDay = eraStartDay)

  # Censor observation at outcome:
  sccsData$cases <- sccsData$cases |>
    inner_join(firstOutcomeEra, by = join_by(caseId)) |>
    collect() |>
    mutate(endDay = censorFunction(endDay, outcomeDay)) |>
    select(-outcomeDay)


  covarSettings <- createEraCovariateSettings(label = "Exposures of interest",
                                              includeEraIds = 1,
                                              stratifyById = FALSE,
                                              start = 0,
                                              end = 0,
                                              endAnchor = "era end")

  studyPop <- createStudyPopulation(sccsData = sccsData,
                                    outcomeId = settings$outcomeId,
                                    firstOutcomeOnly = TRUE,
                                    naivePeriod = 0)

  sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                             sccsData = sccsData,
                                             eraCovariateSettings = covarSettings)

  model <- fitSccsModel(sccsIntervalData)
  estimates <- model$estimates
  return(tibble(ci95Lb = exp(estimates$logLb95[2]),
                ci95Ub = exp(estimates$logUb95[2]),
                diagnosticEstimate = exp(estimates$logRr[1])))
}

# One change at dying in next week:
censorFunction <- function(endDay, outcomeDay) {
  if_else(runif(length(endDay)) < 0.9, round(pmin(endDay, outcomeDay + runif(length(endDay), 0, 7))), endDay)
}
# Added hazard of dying for rest of time:
censorFunction <- function(endDay, outcomeDay) {
  pmin(endDay, outcomeDay + rexp(length(endDay), 0.05))
}

cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")



results <- ParallelLogger::clusterApply(cluster, 1:100, simulateOne, settings = settings, censorFunction = censorFunction)
results <- bind_rows(results)
coverage <- results |>
  mutate(coverage = ci95Lb < rw$relativeRisks & ci95Ub > rw$relativeRisks) |>
  summarise(mean(coverage)) |>
  pull()
diagnosticEstimate <- results |>
  mutate(diagnosticEstimate = log(diagnosticEstimate)) |>
  summarise(exp(mean(diagnosticEstimate))) |>
  pull()
writeLines(sprintf("Coverage: %0.3f, mean diagnostic estimate: %0.2f", coverage, diagnosticEstimate))




ParallelLogger::stopCluster(cluster)

print(sprintf("True IRR = %0.2f", rw$relativeRisks))

h <- 0.0003
1-(1-h)^365

