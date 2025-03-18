library(testthat)

test_that("Setting object serializaiton deserialization", {
  # EraCovariateSettings
  settings <- createEraCovariateSettings(includeEraIds = 1)
  settings2 <- EraCovariateSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createEraCovariateSettings(includeEraIds = c(1, 2 ,3), excludeEraIds = c(4, 5, 6))
  settings2 <- EraCovariateSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # AgeCovariateSettings
  settings <- createAgeCovariateSettings()
  settings2 <- AgeCovariateSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # SeasonalityCovariateSettings
  settings <- createSeasonalityCovariateSettings()
  settings2 <- SeasonalityCovariateSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # CalendarTimeCovariateSettings
  settings <- createCalendarTimeCovariateSettings()
  settings2 <- CalendarTimeCovariateSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # ControlIntervalSettings
  settings <- createControlIntervalSettings()
  settings2 <- ControlIntervalSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # ControlIntervalSettings
  settings <- createControlIntervalSettings(includeEraIds = c(1, 2 ,3), excludeEraIds = c(4, 5, 6))
  settings2 <- ControlIntervalSettings$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # GetDbSccsDataArgs
  settings <- createGetDbSccsDataArgs()
  settings2 <- GetDbSccsDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # CreateStudyPopulationArgs
  settings <- createCreateStudyPopulationArgs()
  settings2 <- CreateStudyPopulationArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # CreateSccsIntervalDataArgs
  settings <- createCreateSccsIntervalDataArgs(createEraCovariateSettings(includeEraIds = 1))
  settings2 <-CreateSccsIntervalDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createCreateSccsIntervalDataArgs(
    eraCovariateSettings = createEraCovariateSettings(includeEraIds = 1),
    ageCovariateSettings = createAgeCovariateSettings(),
    seasonalityCovariateSettings = createSeasonalityCovariateSettings(),
    calendarTimeCovariateSettings = createCalendarTimeCovariateSettings()
  )
  settings2 <- CreateSccsIntervalDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createCreateSccsIntervalDataArgs(
    eraCovariateSettings = list(createEraCovariateSettings(includeEraIds = 1), createEraCovariateSettings(includeEraIds = 2)),
    ageCovariateSettings = createAgeCovariateSettings(),
    seasonalityCovariateSettings = createSeasonalityCovariateSettings(),
    calendarTimeCovariateSettings = createCalendarTimeCovariateSettings()
  )
  settings2 <- CreateSccsIntervalDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # CreateScriIntervalDataArgs
  settings <- createCreateScriIntervalDataArgs(createEraCovariateSettings(includeEraIds = 1),
                                               controlIntervalSettings = createControlIntervalSettings(includeEraIds = 1))
  settings2 <- CreateScriIntervalDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createCreateScriIntervalDataArgs(
    eraCovariateSettings = list(createEraCovariateSettings(includeEraIds = 1), createEraCovariateSettings(includeEraIds = 2)),
    controlIntervalSettings = createControlIntervalSettings(includeEraIds = 1)
  )
  settings2 <- CreateScriIntervalDataArgs$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # FitSccsModelArgs
  settings <- createFitSccsModelArgs()
  settings2 <- FitSccsModelArgs$new(json = settings$toJson())
  # Note: profileBounds and profileGrids are rounded to 4 digits in JSON. This is not a problem.
  expect_equal(settings, settings2, tolerance = 1e-4)

  settings <- createFitSccsModelArgs(profileGrid = seq(log(0.1), log(10), length.out = 1000), profileBounds = NULL)
  settings2 <- FitSccsModelArgs$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 1e-4)

  # SccsAnalysis
  settings <- createSccsAnalysis(
    analysisId = 1,
    description = "SCCS",
    getDbSccsDataArgs = createGetDbSccsDataArgs(),
    createStudyPopulationArgs = createCreateStudyPopulationArgs(),
    createIntervalDataArgs = createCreateSccsIntervalDataArgs(
      eraCovariateSettings = createEraCovariateSettings(includeEraIds = "exposureId", exposureOfInterest = TRUE)),
    fitSccsModelArgs = createFitSccsModelArgs()
  )
  settings2 <- SccsAnalysis$new(json = settings$toJson())
  expect_equal(settings, settings2, tolerance = 1e-4)

  # Save and load SccsAnalysisList
  sccsAnalysisList <- list(
    createSccsAnalysis(
      analysisId = 1,
      description = "SCCS",
      getDbSccsDataArgs = createGetDbSccsDataArgs(),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(),
      createIntervalDataArgs = createCreateSccsIntervalDataArgs(
        eraCovariateSettings = createEraCovariateSettings(includeEraIds = "exposureId", exposureOfInterest = TRUE)),
      fitSccsModelArgs = createFitSccsModelArgs()
    ),
    createSccsAnalysis(
      analysisId = 2,
      description = "SCCS 2",
      getDbSccsDataArgs = createGetDbSccsDataArgs(),
      createStudyPopulationArgs = createCreateStudyPopulationArgs(),
      createIntervalDataArgs = createCreateSccsIntervalDataArgs(
        eraCovariateSettings = createEraCovariateSettings(includeEraIds = "exposureId", exposureOfInterest = TRUE)),
      fitSccsModelArgs = createFitSccsModelArgs()
    )
  )
  tempFile <- tempfile(fileext = ",json")
  saveSccsAnalysisList(sccsAnalysisList, tempFile)
  sccsAnalysisList2 <- loadSccsAnalysisList(tempFile)
  expect_equal(sccsAnalysisList, sccsAnalysisList2, tolerance = 1e-4)
  unlink(tempFile)

  # Exposure
  settings <- createExposure(exposureId = 1)
  settings2 <- Exposure$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createExposure(exposureId = 1, trueEffectSize = 1)
  settings2 <- Exposure$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # ExposuresOutcome
  settings <- createExposuresOutcome(
    outcomeId = 2,
    exposures = list(createExposure(exposureId = 1))
  )
  settings2 <- ExposuresOutcome$new(json = settings$toJson())
  expect_equal(settings, settings2)

  settings <- createExposuresOutcome(
    outcomeId = 2,
    exposures = list(createExposure(exposureId = 1)),
    nestingCohortId = 3
  )
  settings2 <- ExposuresOutcome$new(json = settings$toJson())
  expect_equal(settings, settings2)

  # Save and load ExposuresOutcomeList
  exposuresOutcomeList <- list(
    createExposuresOutcome(
      outcomeId = 2,
      exposures = list(createExposure(exposureId = 1))
    ),
    createExposuresOutcome(
      outcomeId = 3,
      exposures = list(createExposure(exposureId = 1))
    )
  )
  tempFile <- tempfile(fileext = ",json")
  saveExposuresOutcomeList(exposuresOutcomeList, tempFile)
  exposuresOutcomeList2 <- loadExposuresOutcomeList(tempFile)
  expect_equal(exposuresOutcomeList, exposuresOutcomeList2, tolerance = 1e-4)
  unlink(tempFile)

  # SccsDiagnosticThresholds
  settings <- createSccsDiagnosticThresholds()
  settings2 <- SccsDiagnosticThresholds$new(json = settings$toJson())
  expect_equal(settings, settings2)
})
