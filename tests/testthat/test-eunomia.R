library(SelfControlledCaseSeries)
library(testthat)
library(Eunomia)

connectionDetails <- getEunomiaConnectionDetails()
createCohorts(connectionDetails)

# connection <- connect(connectionDetails)

test_that("Running multiple analyses against Eunomia", {
  outputFolder <- tempfile(pattern = "sccsOutput")
  on.exit(unlink(outputFolder, recursive = TRUE))

  # Adding empty exposure and outcome cohorts:
  exposuresOutcomeList <- list(
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 1)),
      outcomeId = 3
    ),
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 1),
                       createExposure(exposureId = 2, exposureIdRef = "exposureId2")),
      outcomeId = 3
    ),
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 1)),
      outcomeId = 4
    ),
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 999)),
      outcomeId = 4
    ),
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 1)),
      outcomeId = 999
    )
  )

  getDbSccsDataArgs <- createGetDbSccsDataArgs(deleteCovariatesSmallCount = 1)

  createStudyPopulationArgs <- createCreateStudyPopulationArgs(
    naivePeriod = 180,
    firstOutcomeOnly = FALSE
  )

  covarExposureOfInt <- createEraCovariateSettings(
    label = "Exposure of interest",
    includeEraIds = "exposureId",
    start = 0,
    end = 7,
    endAnchor = "era start"
  )

  # All outcomes occur at almost the same age, causing issues. Disable for now:
  # ageSettings <- createAgeCovariateSettings(ageKnots = 5)
  #
  # seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

  covarPreExp <- createEraCovariateSettings(
    label = "Pre-exposure",
    includeEraIds = "exposureId",
    start = -30,
    end = -1,
    endAnchor = "era start"
  )

  createSccsIntervalDataArgs <- createCreateSccsIntervalDataArgs(
    eraCovariateSettings = list(
      covarExposureOfInt,
      covarPreExp
    )
  )

  fitSccsModelArgs <- createFitSccsModelArgs()

  sccsAnalysis1 <- createSccsAnalysis(
    analysisId = 1,
    description = "SCCS",
    getDbSccsDataArgs = getDbSccsDataArgs,
    createStudyPopulationArgs = createStudyPopulationArgs,
    createIntervalDataArgs = createSccsIntervalDataArgs,
    fitSccsModelArgs = fitSccsModelArgs
  )

  controlIntervalSettings <- createControlIntervalSettings(
    start = -180,
    end = -1,
    endAnchor = "era start"
  )

  createScriIntervalDataArgs <- createCreateScriIntervalDataArgs(
    eraCovariateSettings = covarExposureOfInt,
    controlIntervalSettings = controlIntervalSettings
  )

  sccsAnalysis2 <- createSccsAnalysis(
    analysisId = 2,
    description = "SCRI",
    getDbSccsDataArgs = getDbSccsDataArgs,
    createStudyPopulationArgs = createStudyPopulationArgs,
    createIntervalDataArgs = createScriIntervalDataArgs,
    fitSccsModelArgs = fitSccsModelArgs
  )

  sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2)

  analysesToExclude <- data.frame(
    exposureId = c(1),
    outcomeId = c(3)
  )
  expect_warning(
    {
      result <- runSccsAnalyses(
        connectionDetails = connectionDetails,
        cdmDatabaseSchema = "main",
        exposureDatabaseSchema = "main",
        exposureTable = "cohort",
        outcomeDatabaseSchema = "main",
        outcomeTable = "cohort",
        outputFolder = outputFolder,
        exposuresOutcomeList = exposuresOutcomeList,
        sccsAnalysisList = sccsAnalysisList,
        analysesToExclude = analysesToExclude
      )
    }, "No cases left in study population"
  )
  expect_equal(sum(result$exposureId == 1 & result$outcomeId == 3), 0)

  analysisSum <- summarizeSccsAnalyses(result, outputFolder)

  expect_equal(nrow(analysisSum), 4)

  # unlink(outputFolder, recursive = TRUE)
})

test_that("Fetching data from drug_era and condition_era tables from Eunomia", {
  # 192671 = Gastrointestinal haemorrhage
  # 1118084 = Celecoxib
  sccsData <- SelfControlledCaseSeries::getDbSccsData(connectionDetails = connectionDetails,
                                                      cdmDatabaseSchema = "main",
                                                      exposureTable = "drug_era",
                                                      outcomeTable = "condition_era",
                                                      outcomeIds = 192671,
                                                      exposureIds = 1118084)
  expect_s4_class(sccsData, "SccsData")
})

# Remove the Eunomia database:
unlink(connectionDetails$server())
