library(SelfControlledCaseSeries)
options(andromedaTempFolder = "e:/andromedaTemp")

folder <- "e:/SccsEdoTestRwd"
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

dbi = 8
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

outcome = outcomes[5, ]
fitAndSaveModel <- function(outcome, database, folder, aspirin) {
  sccsModelFileName <- file.path(folder, sprintf("SccsModel_o%d_%s.rds", outcome$outcomeId, database$name))
  diagnosticFileName <- file.path(folder, sprintf("Diagnostics_o%d_%s.rds", outcome$outcomeId, database$name))
  if (!file.exists(sccsModelFileName) || !file.exists(diagnosticFileName)) {
    if (outcome$covid) {
      sccsDataFileName <- file.path(folder, sprintf("SccsDataCovid_%s.zip", database$name))
      d <- loadSccsData(sccsDataFileName)
    } else {
      sccsDataFileName <- file.path(folder, sprintf("SccsDataNonCovid_%s.zip", database$name))
      d <- loadSccsData(sccsDataFileName)
    }
    studyPop <- createStudyPopulation(sccsData = d,
                                      outcomeId = outcome$outcomeId,
                                      firstOutcomeOnly = outcome$firstOnly,
                                      naivePeriod = 180)

    if (file.exists(sccsModelFileName)) {
      model <- readRDS(sccsModelFileName)
    } else {

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
                               threads = 2)
      model <- fitSccsModel(sccsIntervalData, control = control, profileBounds = NULL)
      saveRDS(model, sccsModelFileName)
    }
    if (!file.exists(diagnosticFileName)) {
      edo <- computeEventDependentObservation(sccsModel = model)
      ede <- computeExposureChange(sccsData = d,
                                   studyPopulation = studyPop,
                                   exposureEraId = aspirin)
      ede2 <- computeExposureChange(sccsData = d,
                                    studyPopulation = studyPop,
                                    exposureEraId = aspirin,
                                    ignoreExposureStarts = TRUE)
      preExposure <- computePreExposureGainP(sccsData = d,
                                            studyPopulation = studyPop,
                                            exposureEraId = aspirin)
      timeTrend <- computeTimeStability(studyPopulation = studyPop,
                                        sccsModel = model)
      if (model$status != "OK" || !1000 %in% model$estimates$covariateId) {
        preExposure2 <- tibble(logRr = NA,
                               logLb95 = NA,
                               logUb95 = NA)
      } else {
        preExposure2 <- model$estimates |>
          filter(covariateId == 1000) |>
          select("logRr", "logLb95", "logUb95")
      }
      diagnostics <- list(edo = edo,
                          ede = ede,
                          ede2 = ede2,
                          preExposure = preExposure,
                          preExposure2 = preExposure2,
                          timeTrend = timeTrend)
      saveRDS(diagnostics, diagnosticFileName)
    }
  }
}

connection <- DatabaseConnector::connect(connectionDetails)

for (dbi in 1:nrow(databases)) {
  database <- databases[dbi, ]
  resultsFileName <- file.path(folder, sprintf("Results_%s.rds", database$name))
  if (!file.exists(resultsFileName)) {
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

    cluster <- ParallelLogger::makeCluster(8)
    ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")
    ParallelLogger::clusterApply(cluster, split(outcomes, seq_len(nrow(outcomes))), fitAndSaveModel, database = database, folder = folder, aspirin = aspirin)
    ParallelLogger::stopCluster(cluster)

    outcomes$cases <- NA
    outcomes$edoRatio <- NA
    outcomes$edoP <- NA
    outcomes$edeRatio <- NA
    outcomes$edeP <- NA
    outcomes$ede2Ratio <- NA
    outcomes$ede2P <- NA
    outcomes$preExposureRatio <- NA
    outcomes$preExposureP <- NA
    outcomes$preExposure2Rr <- NA
    outcomes$preExposure2Lb <- NA
    outcomes$preExposure2Ub <- NA
    outcomes$timeTrendRatio <- NA
    outcomes$timeTrendP <- NA

    i <- 1
    for (i in seq_len(nrow(outcomes))) {
      outcome <- outcomes[i, ]
      fileName <- file.path(folder, sprintf("SccsModel_o%d_%s.rds", outcome$outcomeId, database$name))
      model <- readRDS(fileName)
      # p <- computeEventDependentObservationP(model)
      # plotEventObservationDependence(studyPop)
      # plotEventToCalendarTime(studyPop, model)
      # stability <- computeTimeStability(studyPop, model)
      # plotCalendarTimeEffect(model)
      # plotSeasonality(model)
      # outcomes$edoP[i] <- sprintf("%0.4f", p)
      # if (!is.null(model$estimates)) {
      #   outcomes$edoEstimate[i] <- model$estimates |>
      #     filter(covariateId == 99) |>
      #     mutate(estimate = sprintf("%0.2f (%0.2f - %0.2f)", exp(logRr), exp(logLb95), exp(logUb95))) |>
      #     pull(estimate)
      # }
      outcomes$cases[i] <- min(model$metaData$attrition$outcomeSubjects)
      diagnosticFileName <- file.path(folder, sprintf("Diagnostics_o%d_%s.rds", outcome$outcomeId, database$name))
      diagnostics <- readRDS(diagnosticFileName)
      outcomes$edoRatio[i] <- diagnostics$edo$ratio
      outcomes$edoP[i] <- diagnostics$edo$p
      outcomes$edeRatio[i] <- diagnostics$ede$ratio
      outcomes$edeP[i] <- diagnostics$ede$p
      outcomes$ede2Ratio[i] <- diagnostics$ede2$ratio
      outcomes$ede2P[i] <- diagnostics$ede2$p
      outcomes$preExposureRatio[i] <- diagnostics$preExposure$ratio
      outcomes$preExposureP[i] <- diagnostics$preExposure$p
      outcomes$preExposure2Rr[i] <- exp(diagnostics$preExposure2$logRr)
      outcomes$preExposure2Lb[i] <- exp(diagnostics$preExposure2$logLb95)
      outcomes$preExposure2Ub[i] <- exp(diagnostics$preExposure2$logUb95)
      outcomes$timeTrendRatio[i] <- diagnostics$timeTrend$ratio
      outcomes$timeTrendP[i] <- diagnostics$timeTrend$p

    }
    outcomes <- outcomes |>
      mutate(database = !!database$name)
    saveRDS(outcomes, resultsFileName)
  }
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

