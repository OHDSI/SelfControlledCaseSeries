# Some code to explore performance issues with likelihood profiling

library(SelfControlledCaseSeries)

# Simulate data ----------------------------------------------------------------
settings <- createSccsSimulationSettings(includeAgeEffect = FALSE,
                                         includeCalendarTimeEffect = TRUE,
                                         includeSeasonality = TRUE)
set.seed(123)
sccsData <- simulateSccsData(10000, settings)

seasonalitySettings <- createSeasonalityCovariateSettings()
calendarTimeSettings <- createCalendarTimeCovariateSettings()
covarSettings <- createEraCovariateSettings(label = "Exposure of interest",
                                            includeEraIds = c(1, 2),
                                            stratifyById = TRUE,
                                            start = 0,
                                            end = 0,
                                            endAnchor = "era end")
studyPopulation <- createStudyPopulation(sccsData = sccsData,
                                         outcomeId = 10,
                                         firstOutcomeOnly = FALSE,
                                         naivePeriod = 0)
sccsIntervalData <- createSccsIntervalData(
  studyPopulation = studyPopulation,
  sccsData = sccsData,
  eraCovariateSettings = covarSettings,
  seasonalityCovariateSettings = seasonalitySettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)
# Save data so we can re-use simulated data:
saveRDS(collect(sccsIntervalData$outcomes), "~/Data/temp/outcomes.rds")
saveRDS(collect(sccsIntervalData$covariates), "~/Data/temp/covariates.rds")

# Fit model --------------------------------------------------------------------
library(Cyclops)
outcomes <- readRDS("~/Data/temp/outcomes.rds")
covariates <- readRDS("~/Data/temp/covariates.rds")
cyclopsData <- Cyclops::convertToCyclopsData(outcomes,
                                             covariates,
                                             modelType = "cpr",
                                             addIntercept = FALSE,
                                             checkRowIds = FALSE,
                                             quiet = TRUE)
system.time(
  fit <- Cyclops::fitCyclopsModel(cyclopsData,
                                  control = createControl(threads = 10,
                                                          noiseLevel = "quiet"))
)
# user  system elapsed
# 8.524   0.190   2.755

# Compute likelihood profile
system.time(
  logLikelihoodProfile <- getCyclopsProfileLogLikelihood(
    object = fit,
    parm = 1000,
    bounds = c(log(0.1), log(10)),
    tolerance = 0.1
  )
)
# user  system elapsed
# 371.697   0.847  58.086


