library(SelfControlledCaseSeries)
library(testthat)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

# connection <- connect(connectionDetails)

test_that("Running multiple analyses against Eunomia", {
  outputFolder <- tempfile(pattern = "sccsOutput")
  on.exit(unlink(outputFolder, recursive = TRUE))

  exposuresOutcomeList <- list(
    createExposuresOutcome(
      exposures = list(
        createExposure(exposureId = 1),
        createExposure(exposureId = 2, exposureIdRef = "exposureId2")
      ),
      outcomeId = 3
    ),
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 1)),
      outcomeId = 4
    ),
    createExposuresOutcome(
      exposures = list(createExposure(exposureId = 1)),
      outcomeId = 3,
      nestingCohortId = 4
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

  getDbSccsDataArgs <- createGetDbSccsDataArgs(deleteCovariatesSmallCount = 1,
                                               studyStartDates = "20000101")

  createStudyPopulationArgs1 <- createCreateStudyPopulationArgs(
    naivePeriod = 180,
    firstOutcomeOnly = FALSE,
    genderConceptIds = 8507,
    restrictTimeToEraId = "exposureId"
  )

  createStudyPopulationArgs2 <- createCreateStudyPopulationArgs(
    naivePeriod = 180,
    firstOutcomeOnly = FALSE
  )

  covarExposureOfInt <- createEraCovariateSettings(
    label = "Exposure of interest",
    includeEraIds = "exposureId",
    start = 0,
    end = 7,
    endAnchor = "era start",
    profileLikelihood = TRUE,
    exposureOfInterest = TRUE
  )

  covarExposureOfInt2 <- createEraCovariateSettings(
    label = "Exposure of interest 2",
    includeEraIds = "exposureId2",
    start = 0,
    end = 7,
    endAnchor = "era start"
  )

  calendarTimeSettings <- createCalendarTimeCovariateSettings(calendarTimeKnots = 5)

  seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

  covarPreExp <- createEraCovariateSettings(
    label = "Pre-exposure",
    includeEraIds = c("exposureId", "exposureId2"),
    start = -30,
    end = -1,
    endAnchor = "era start",
    exposureOfInterest = FALSE
  )

  createSccsIntervalDataArgs <- createCreateSccsIntervalDataArgs(
    eraCovariateSettings = list(
      covarExposureOfInt,
      covarExposureOfInt2,
      covarPreExp
    ),
    calendarTimeCovariateSettings = calendarTimeSettings,
    seasonalityCovariateSettings = seasonalitySettings
  )

  fitSccsModelArgs <- createFitSccsModelArgs()

  sccsAnalysis1 <- createSccsAnalysis(
    analysisId = 1,
    description = "SCCS",
    getDbSccsDataArgs = getDbSccsDataArgs,
    createStudyPopulationArgs = createStudyPopulationArgs1,
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
    createStudyPopulationArgs = createStudyPopulationArgs2,
    createIntervalDataArgs = createScriIntervalDataArgs,
    fitSccsModelArgs = fitSccsModelArgs
  )

  sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2)

  analysesToExclude <- data.frame(
    exposureId = c(1),
    outcomeId = c(4)
  )

  # Expect warning because outcome 999 does not exist in data:
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
    },
    "No cases left in study population"
  )
  ref <- getFileReference(outputFolder)
  expect_equal(nrow(ref), 8)

  # analysesToExclude was enforced:
  expect_false(any(ref$exposureId == 1 & ref$outcomeId == 4))

  model <- readRDS(file.path(outputFolder, result$sccsModelFile[1]))
  expect_true(max(grepl("gender", getAttritionTable(model)$description)) == 1)

  model <- readRDS(file.path(outputFolder, pull(filter(ref, nestingCohortId  == 4), sccsModelFile)[1]))
  expect_true(max(grepl("nesting", getAttritionTable(model)$description)) == 1)

  analysisSum <- getResultsSummary(outputFolder)

  expect_equal(nrow(analysisSum), 8)

  # sccsData <- loadSccsData(file.path(outputFolder, ref$sccsDataFile[5]))
  # sccsData$eraRef

  # Assert appropriate designs:
  sccsIntervalData <- loadSccsIntervalData(file.path(outputFolder, pull(filter(ref, analysisId == 1), sccsIntervalDataFile)[1]))
  expect_equal(attr(sccsIntervalData, "metaData")$design, "SCCS")

  sccsIntervalData <- loadSccsIntervalData(file.path(outputFolder, pull(filter(ref, analysisId == 2), sccsIntervalDataFile)[1]))
  expect_equal(attr(sccsIntervalData, "metaData")$design, "SCRI")

  # Test export to CSV:
  exportToCsv(outputFolder)

  # Workaround for issue https://github.com/tidyverse/vroom/issues/519:
  readr::local_edition(1)
  diagnosticsSummary <- readr::read_csv(file.path(outputFolder, "export", "sccs_diagnostics_summary.csv"), show_col_types = FALSE)
  expect_true(all(diagnosticsSummary$ease_diagnostic == "NOT EVALUATED"))

  # Make sure exposures_outcome_set_id is consistent across table:
  exposure <- readr::read_csv(file.path(outputFolder, "export", "sccs_exposure.csv"), show_col_types = FALSE)
  eos <- readr::read_csv(file.path(outputFolder, "export", "sccs_exposures_outcome_set.csv"), show_col_types = FALSE)
  expect_length(setdiff(unique(diagnosticsSummary$exposures_outcome_set_id),
                        unique(exposure$exposures_outcome_set_id)),
                0)
  expect_length(setdiff(unique(eos$exposures_outcome_set_id),
                        unique(exposure$exposures_outcome_set_id)),
                0)

  specs <- readr::read_csv(
    file = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"),
    show_col_types = FALSE
  ) %>%
    SqlRender::snakeCaseToCamelCaseNames()

  specs <- split(specs, specs$tableName)
  # tableSpecs = specs[[1]]
  for (tableSpecs in specs) {
    fileName <- file.path(outputFolder, "export", sprintf("%s.csv", tableSpecs$tableName[1]))
    expect_true(file.exists(fileName))
    table <- readr::read_csv(fileName, show_col_types = FALSE)
    expect_setequal(colnames(table), tableSpecs$columnName)
  }

  # unlink(outputFolder, recursive = TRUE)
})

test_that("Fetching data from drug_era and condition_era tables from Eunomia", {
  # 192671 = Gastrointestinal haemorrhage
  # 1118084 = Celecoxib
  sccsData <- SelfControlledCaseSeries::getDbSccsData(
    connectionDetails = connectionDetails,
    cdmDatabaseSchema = "main",
    exposureTable = "drug_era",
    outcomeTable = "condition_era",
    outcomeIds = 192671,
    exposureIds = 1118084
  )
  expect_s4_class(sccsData, "SccsData")
})

# Remove the Eunomia database:
unlink(connectionDetails$server())
