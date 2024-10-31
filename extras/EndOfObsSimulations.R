library(SelfControlledCaseSeries)

# Define simulation scenarios ----------------------------------------------------------------------
scenarios <- list()
for (trueRr in c(1, 2, 4)) {
  for (baseLineRate in c(0.001, 0.0001)) {
    for (usageRateSlope in c(0, 0.00001)) {
      for (censorType in c("Next week", "Gradual", "First to last", "None")) {
        for (censorStrength in if (censorType == "None") c("None") else c("Weak", "Strong")) {
          rw <- createSimulationRiskWindow(start = 0,
                                           end = 0,
                                           endAnchor = "era end",
                                           relativeRisks = trueRr)
          if (usageRateSlope > 0) {
            usageRate <- 0.001
          } else if (usageRateSlope < 0) {
            usageRate <- 0.001 - 3000 * usageRateSlope
          } else {
            usageRate <- 0.01
          }
          settings <- createSccsSimulationSettings(minBaselineRate = baseLineRate / 10,
                                                   maxBaselineRate = baseLineRate,
                                                   eraIds = 1,
                                                   patientUsages = 0.8,
                                                   usageRate = if (usageRateSlope < 0) 0.001 - 3000 * usageRateSlope else 0.001,
                                                   usageRateSlope = usageRateSlope,
                                                   simulationRiskWindows = list(rw),
                                                   includeAgeEffect = FALSE,
                                                   includeSeasonality = FALSE,
                                                   includeCalendarTimeEffect = FALSE)
          scenario <- list(settings = settings,
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
folder <- "e:/SccsEdoSimulations100"

scenario = scenarios[[11]]
scenario
simulateOne <- function(seed, scenario) {
  set.seed(seed)
  sccsData <- simulateSccsData(1000, scenario$settings)

  # Censor observation period:
  firstOutcomeEra <- sccsData$eras |>
    filter(eraId == 10) |>
    group_by(caseId) |>
    filter(row_number(eraStartDay) == 1) |>
    ungroup() |>
    select(caseId, outcomeDay = eraStartDay)

  firstObservation <- sccsData$eras |>
    group_by(caseId) |>
    filter(row_number(eraStartDay) == 1) |>
    ungroup() |>
    select(caseId, firstDay = eraStartDay)

  lastObservation <- sccsData$eras |>
    group_by(caseId) |>
    filter(row_number(-eraStartDay) == 1) |>
    ungroup() |>
    select(caseId, lastDay = eraStartDay)

  cases <- sccsData$cases |>
    inner_join(firstOutcomeEra, by = join_by(caseId)) |>
    inner_join(firstObservation, by = join_by(caseId)) |>
    inner_join(lastObservation, by = join_by(caseId)) |>
    collect() |>
    mutate(noninformativeEndCensor = as.numeric(runif(n()) < 0.8))

  if (scenario$censorType == "Next week") {
    # One change of dying in next week:
    probability <- if_else(scenario$censorStrength == "Weak", 0.05, 0.25)
    sccsData$cases <- cases |>
      mutate(newEndDay = if_else(runif(length(endDay)) < 0.05, round(pmin(endDay, outcomeDay + runif(length(endDay), 0, 7))), endDay)) |>
      mutate(noninformativeEndCensor = if_else(endDay == newEndDay, noninformativeEndCensor, 0)) |>
      mutate(endDay = newEndDay) |>
      select(-outcomeDay, -firstDay, -lastDay, -newEndDay)
  } else if (scenario$censorType == "Gradual") {
    # Added hazard of dying for rest of time:
    rate <- if_else(scenario$censorStrength == "Weak", 0.001, 0.01)
    sccsData$cases <- cases |>
      mutate(newEndDay = pmin(endDay, outcomeDay + rexp(length(endDay), rate))) |>
      mutate(noninformativeEndCensor = if_else(endDay == newEndDay, noninformativeEndCensor, 0)) |>
      mutate(endDay = newEndDay) |>
      select(-outcomeDay, -firstDay, -lastDay, -newEndDay)
  } else if (scenario$censorType == "First to last") {
    # Mimic censoring when observation time is defined as first to last event:
    probability <- if_else(scenario$censorStrength == "Weak", 0.2, 1.0)
    sccsData$cases <- cases |>
      mutate(startDay = if_else(runif(length(startDay)) < probability, firstDay, startDay),
             newEndDay = if_else(runif(length(endDay)) < probability, lastDay, endDay)) |>
      mutate(noninformativeEndCensor = if_else(endDay == newEndDay, noninformativeEndCensor, 0)) |>
      mutate(endDay = newEndDay) |>
      select(-outcomeDay, -firstDay, -lastDay, -newEndDay)
  } else if (scenario$censorType == "None") {
    # Do nothing
  } else {
    stop("Unknown censoring type: ", scenario$censorType)
  }

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
                                    naivePeriod = 365)

  sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                             sccsData = sccsData,
                                             eraCovariateSettings = list(covarSettings, preCovarSettings))

  model <- fitSccsModel(sccsIntervalData, profileBounds = NULL)
  estimates <- model$estimates
  # estimates
  idx1 <- which(estimates$covariateId == 1000)
  idx2 <- which(estimates$covariateId == 99)

  # Create interaction between exposure and censor status:
  censoredCases <- sccsData$cases |>
    filter(noninformativeEndCensor == 0) |>
    distinct(caseId)

  interactionEras <- sccsData$eras |>
    filter(eraId == 1) |>
    inner_join(censoredCases, join_by("caseId")) |>
    mutate(eraId = 11)
  writeLines(sprintf("Found %d censored cases having %d exposures", pull(count(censoredCases)), pull(count(interactionEras))))

  sccsData$eras <- union_all(
    sccsData$eras,
    interactionEras
  ) |>
    arrange(caseId, eraStartDay)

  interactionCovarSettings <- createEraCovariateSettings(label = "Interaction exposure x censoring",
                                                         includeEraIds = 11,
                                                         stratifyById = FALSE,
                                                         start = 0,
                                                         end = 0,
                                                         endAnchor = "era end")
  sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                             sccsData = sccsData,
                                             eraCovariateSettings = list(covarSettings, preCovarSettings, interactionCovarSettings),
                                             endOfObservationEraLength = 0)

  model2 <- fitSccsModel(sccsIntervalData, profileBounds = NULL)
  if (model2$status != "OK") {
    estimates2 <- tibble(logRr = NA, logLb95 = NA, logUb95 = NA)
    idx3 <- 1
  } else {
    estimates2 <- model2$estimates
    idx3 <- which(estimates2$covariateId == 1002)
  }
  edo <- computeEventDependentObservation(model)
  exposureStability <- computeExposureStability(studyPop, sccsData, 1)
  row <- tibble(logRr = estimates$logRr[idx1],
                ci95Lb = exp(estimates$logLb95[idx1]),
                ci95Ub = exp(estimates$logUb95[idx1]),
                diagnosticEstimate = edo$ratio,
                diagnosticP = edo$p,
                exposureStabilityEstimate = exposureStability$ratio,
                exposureStabilityP = exposureStability$p,
                diagnostic2Estimate = exp(estimates2$logRr[idx3]),
                diagnostic2Lb = estimates2$logLb95[idx3],
                diagnostic2Ub = estimates2$logUb95[idx3])
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
  scenarioKey$startCensorFunction <- NULL
  scenarioKey$endCensorFunction <- NULL
  fileName <- paste0(paste(gsub("__", "", gsub("[^a-zA-Z0-9]", "_", paste(names(scenarioKey), scenarioKey, sep = "_"))), collapse = "_"), ".rds")
  fileName <- file.path(folder, fileName)
  if (file.exists(fileName)) {
    results <- readRDS(fileName)
  } else {
    results <- ParallelLogger::clusterApply(cluster, 1:100, simulateOne, scenario = scenario)
    results <- bind_rows(results)
    saveRDS(results, fileName)
  }
  metrics <- results |>
    mutate(coverage = ci95Lb < scenario$trueRr & ci95Ub > scenario$trueRr,
           diagnosticEstimate = log(diagnosticEstimate),
           failDiagnostic = diagnosticP < 0.05,
           diagnostic2Estimate = log(diagnostic2Estimate),
           failDiagnostic2 = diagnostic2Lb > log(1.25) | diagnostic2Ub < log(0.75)) |>
    summarise(coverage = mean(coverage, na.rm = TRUE),
              bias = mean(logRr - log(scenario$trueRr), na.rm = TRUE),
              meanDiagnosticEstimate = exp(mean(diagnosticEstimate, na.rm = TRUE)),
              fractionFailingDiagnostic = mean(failDiagnostic, na.rm = TRUE),
              meanDiagnostic2Estimate = exp(mean(diagnostic2Estimate, na.rm = TRUE)),
              fractionFailingDiagnostic2= mean(failDiagnostic2, na.rm = TRUE))
  row <- as_tibble(scenarioKey) |>
    bind_cols(metrics)
  rows[[length(rows) + 1]] <- row
}
rows <- bind_rows(rows)

ParallelLogger::stopCluster(cluster)
readr::write_csv(rows, file.path(folder, "Results.csv"))
