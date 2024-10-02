library(SelfControlledCaseSeries)


# Define simulation scenarios ----------------------------------------------------------------------

scenarios <- list()
for (trueRr in c(1, 2, 4)) {
  for (baseLineRate in c(0.01, 0.001, 0.0001)) {
    for (usageRateSlope in c(-0.0001, 0, 0.0001)) {
      for (censorType in c("Next week", "Gradual", "None")) {
        for (censorStrength in if (censorType == "None") c("None") else c("Weak", "Strong")) {
          rw <- createSimulationRiskWindow(start = 0,
                                           end = 0,
                                           endAnchor = "era end",
                                           relativeRisks = trueRr)
          settings <- createSccsSimulationSettings(minBaselineRate = baseLineRate / 10,
                                                   maxBaselineRate = baseLineRate,
                                                   eraIds = 1,
                                                   usageRate = if (usageRateSlope < 0) 0.01 - 3000 * usageRateSlope else 0.01,
                                                   usageRateSlope = usageRateSlope,
                                                   simulationRiskWindows = list(rw),
                                                   includeAgeEffect = FALSE,
                                                   includeSeasonality = FALSE,
                                                   includeCalendarTimeEffect = FALSE)
          if (censorType == "Next week") {
            # One change of dying in next week:
            if (censorStrength == "Weak") {
              censorFunction <- function(endDay, outcomeDay) {
                if_else(runif(length(endDay)) < 0.05, round(pmin(endDay, outcomeDay + runif(length(endDay), 0, 7))), endDay)
              }
            } else {
              censorFunction <- function(endDay, outcomeDay) {
                if_else(runif(length(endDay)) < 0.25, round(pmin(endDay, outcomeDay + runif(length(endDay), 0, 7))), endDay)
              }
            }
          } else if (censorType == "Gradual") {
            # Added hazard of dying for rest of time:
            if (censorStrength == "Weak") {
              censorFunction <- function(endDay, outcomeDay) {
                pmin(endDay, outcomeDay + rexp(length(endDay), 0.001))
              }
            } else {
              censorFunction <- function(endDay, outcomeDay) {
                pmin(endDay, outcomeDay + rexp(length(endDay), 0.01))
              }
            }
          } else {
            censorFunction <- function(endDay, outcomeDay) {
              endDay
            }
          }
          scenario <- list(settings = settings,
                           censorFunction = censorFunction,
                           trueRr = trueRr,
                           baselineRate = baseLineRate,
                           usageRateSlope = usageRateSlope,
                           censorType = censorType,
                           censorStrength = censorStrength)
          scenarios[[length(scenarios) + 1]] <- scenario

        }
      }
    }
  }
}
writeLines(sprintf("Number of simulation scenarios: %d", length(scenarios)))

# Run simulations ----------------------------------------------------------------------------------
folder <- "e:/SccsEdoSimulations1000"

scenario = scenarios[[10]]
simulateOne <- function(seed, scenario) {
  set.seed(seed)
  sccsData <- simulateSccsData(1000, scenario$settings)
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
    mutate(endDay = scenario$censorFunction(endDay, outcomeDay)) |>
    select(-outcomeDay)


  covarSettings <- createEraCovariateSettings(label = "Exposure of interest",
                                              includeEraIds = 1,
                                              stratifyById = FALSE,
                                              start = 0,
                                              end = 0,
                                              endAnchor = "era end")

  preCovarSettings <- createEraCovariateSettings(label = "Pre-exposure",
                                                 includeEraIds = 1,
                                                 stratifyById = FALSE,
                                                 start = -30,
                                                 end = -1,
                                                 endAnchor = "era start")

  studyPop <- createStudyPopulation(sccsData = sccsData,
                                    outcomeId = scenario$settings$outcomeId,
                                    firstOutcomeOnly = TRUE,
                                    naivePeriod = 0)

  sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                             sccsData = sccsData,
                                             eraCovariateSettings = list(covarSettings, preCovarSettings))

  model <- fitSccsModel(sccsIntervalData, profileBounds = NULL)
  estimates <- model$estimates
  idx1 <- which(estimates$covariateId == 1000)
  idx2 <- which(estimates$covariateId == 99)
  scenario$settings <- NULL
  scenario$censorFunction <- NULL

  row <- tibble(logRr = estimates$logRr[idx1],
                ci95Lb = exp(estimates$logLb95[idx1]),
                ci95Ub = exp(estimates$logUb95[idx1]),
                diagnosticEstimate = exp(estimates$logRr[idx2]),
                diagnosticP = computeEventDependentObservationP(model))
  return(row)
}

cluster <- ParallelLogger::makeCluster(10)
ParallelLogger::clusterRequire(cluster, "SelfControlledCaseSeries")

dir.create(folder)
rows <- list()
for (i in seq_along(scenarios)) {
  writeLines(sprintf("Processing scenario %d of %d", i, length(scenarios)))
  scenario <- scenarios[[i]]
  scenarioKey <- scenario
  scenarioKey$settings <- NULL
  scenarioKey$censorFunction <- NULL
  fileName <- paste0(paste(gsub("__", "", gsub("[^a-zA-Z0-9]", "_", paste(names(scenarioKey), scenarioKey, sep = "_"))), collapse = "_"), ".rds")
  fileName <- file.path(folder, fileName)
  if (file.exists(fileName)) {
    results <- readRDS(fileName)
  } else {
    results <- ParallelLogger::clusterApply(cluster, 1:1000, simulateOne, scenario = scenario)
    results <- bind_rows(results)
    saveRDS(results, fileName)
  }
  metrics <- results |>
    mutate(coverage = ci95Lb < scenario$trueRr & ci95Ub > scenario$trueRr,
           diagnosticEstimate = log(diagnosticEstimate),
           failDiagnostic = diagnosticP < 0.05) |>
    summarise(coverage = mean(coverage, na.rm = TRUE),
              bias = mean(logRr - log(scenario$trueRr), na.rm = TRUE),
              meanDiagnosticEstimate = exp(mean(diagnosticEstimate, na.rm = TRUE)),
              fractionFailingDiagnostic = mean(failDiagnostic, na.rm = TRUE))
  row <- as_tibble(scenarioKey) |>
    bind_cols(metrics)
  rows[[length(rows) + 1]] <- row
}
rows <- bind_rows(rows)

ParallelLogger::stopCluster(cluster)
readr::write_csv(rows, file.path(folder, "Results.csv"))
