# Simulate the effect of exposure ending after the event
library(SelfControlledCaseSeries)

# Types of censoring:
# - Temporary: Having the event temporarily prevents (re-starting) the exposure
# - Permanent: Having the event forever prevents (re-starting) the exposure
# - Permanent when exposed: Having the event during exposure terminates the exposure, and it never comes back


# Define simulation scenarios ----------------------------------------------------------------------

scenarios <- list()
for (trueRr in c(1, 2, 4)) {
  for (baseLineRate in c(0.001, 0.0001)) {
    for (usageRateSlope in c(0, 0.00001)) {
      for (censorType in c("Temporary", "Permanent", "Permanent when exposed", "None")) {
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
                                                   usageRate = usageRate,
                                                   usageRateSlope = usageRateSlope,
                                                   meanPrescriptionDurations = 30,
                                                   sdPrescriptionDurations = 14,
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
folder <- "e:/SccsEdeSimulations100"

scenario = scenarios[[1]]
scenario$censorType

simulateOne <- function(seed, scenario) {
  set.seed(seed)
  sccsData <- simulateSccsData(1000, scenario$settings)

  # Merge overlapping eras:
  sccsData$eras <- sccsData$eras |>
    collect() |>
    arrange(caseId, eraType, eraId, eraStartDay) |>
    group_by(caseId, eraType, eraId) |>
    mutate(newGroup = cumsum(lag(eraEndDay, default = first(eraEndDay)) < eraStartDay)) |>
    group_by(caseId, eraType, eraId, newGroup) |>
    summarise(
      eraStartDay = min(eraStartDay),
      eraEndDay = max(eraEndDay),
      .groups = 'drop'
    ) |>
    select(caseId, eraType, eraId, eraStartDay, eraEndDay) |>
    mutate(eraValue = 1)

  outcomeEras <- sccsData$eras |>
    filter(eraType == "hoi") |>
    select(caseId, outcomeDay = eraStartDay)
  if (scenario$censorType == "Temporary") {
    probability <- if_else(scenario$censorStrength == "Strong", 0.8, 0.25)
    beforeCount <- sccsData$eras |> filter(eraType == "rx") |> count() |> pull()
    filteredExposureEras <- sccsData$eras |>
      filter(eraType == "rx") |>
      left_join(outcomeEras, by = join_by(caseId)) |>
      mutate(outcomeInWindow = outcomeDay > eraStartDay - 30 & outcomeDay < eraStartDay) |>
      group_by(eraType, caseId, eraId, eraValue, eraStartDay, eraEndDay) |>
      summarise(outcomeInWindow = any(outcomeInWindow, na.rm = TRUE), .groups = "drop") |>
      filter(!outcomeInWindow | runif() > probability) |>
      select(-outcomeInWindow)
    afterCount <- filteredExposureEras |> count() |> pull()
    writeLines(sprintf("Removed %d of %d exposures (%0.1f%%)", beforeCount-afterCount, beforeCount, 100*(beforeCount-afterCount) / beforeCount))
    sccsData$eras <- union_all(
      sccsData$eras |>
        filter(eraType == "hoi"),
      filteredExposureEras
    ) |>
      arrange(caseId, eraStartDay)
  } else if (scenario$censorType == "Permanent") {
    probability <- if_else(scenario$censorStrength == "Strong", 0.8, 0.25)
    outcomeEras <- outcomeEras |>
      filter(runif() > probability)

    filteredExposureEras <- sccsData$eras |>
      filter(eraType == "rx") |>
      left_join(outcomeEras, by = join_by(caseId)) |>
      mutate(outcomeInWindow = outcomeDay < eraStartDay) |>
      group_by(eraType, caseId, eraId, eraValue, eraStartDay, eraEndDay) |>
      summarise(outcomeInWindow = any(outcomeInWindow), .groups = "drop") |>
      filter(!outcomeInWindow) |>
      select(-outcomeInWindow)

    sccsData$eras <- union_all(
      sccsData$eras |>
        filter(eraType == "hoi"),
      filteredExposureEras
    ) |>
      arrange(caseId, eraStartDay)
  } else if (scenario$censorType == "Permanent when exposed") {
    probability <- if_else(scenario$censorStrength == "Strong", 0.8, 0.25)
    # Only outcomes during exposure can lead to exposure censoring:
    outcomeEras <- outcomeEras |>
      inner_join(sccsData$eras |>
                   filter(eraType == "rx"),
                 by = join_by(caseId, between(outcomeDay, eraStartDay, eraEndDay))) |>
      select(caseId, outcomeDay) |>
      filter(runif() > probability)

    filteredExposureEras <- sccsData$eras |>
      filter(eraType == "rx") |>
      left_join(outcomeEras, by = join_by(caseId)) |>
      mutate(outcomeInWindow = outcomeDay <= eraStartDay) |>
      group_by(eraType, caseId, eraId, eraValue, eraStartDay, eraEndDay) |>
      summarise(outcomeInWindow = any(outcomeInWindow),
                minOutcomeDay = min(outcomeDay),
                .groups = "drop") |>
      filter(!outcomeInWindow) |>
      mutate(eraEndDay = if_else(minOutcomeDay >= eraStartDay & minOutcomeDay < eraEndDay, minOutcomeDay, eraEndDay)) |>
      select(-outcomeInWindow, -minOutcomeDay)

    sccsData$eras <- union_all(
      sccsData$eras |>
        filter(eraType == "hoi"),
      filteredExposureEras
    ) |>
      arrange(caseId, eraStartDay)
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
                                                 start = -60,
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
  # x <- sccsData$eras |> collect()
  studyPopulation = studyPop
  idx1 <- which(estimates$covariateId == 1000)
  idx2 <- which(estimates$covariateId == 1001)
  p <- computeExposureChangeP(sccsData, studyPop, 1)
  p
  p2 <- computeExposureChangeP(sccsData, studyPop, 1, ignoreExposureStarts = FALSE)
  p2
  # plotExposureCentered(studyPop, sccsData, 1)
  # plotOutcomeCentered(studyPop, sccsData, 1)

  row <- tibble(logRr = estimates$logRr[idx1],
                ci95Lb = exp(estimates$logLb95[idx1]),
                ci95Ub = exp(estimates$logUb95[idx1]),
                diagnosticEstimate = exp(estimates$logRr[idx2]),
                diagnosticP = p,
                diagnosticP2 = p2)
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
           failDiagnostic2 = diagnosticP2 < 0.05) |>
    summarise(coverage = mean(coverage, na.rm = TRUE),
              bias = mean(logRr - log(scenario$trueRr), na.rm = TRUE),
              meanDiagnosticEstimate = exp(mean(diagnosticEstimate, na.rm = TRUE)),
              fractionFailingDiagnostic = mean(failDiagnostic, na.rm = TRUE),
              fractionFailingDiagnostic2 = mean(failDiagnostic2, na.rm = TRUE))
  metrics
  row <- as_tibble(scenarioKey) |>
    bind_cols(metrics)
  rows[[length(rows) + 1]] <- row
}
rows <- bind_rows(rows)

ParallelLogger::stopCluster(cluster)
readr::write_csv(rows, file.path(folder, "Results.csv"))
