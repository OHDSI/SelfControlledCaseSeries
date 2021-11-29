library(SelfControlledCaseSeries)
library(testthat)
library(Eunomia)

connectionDetails <- getEunomiaConnectionDetails()
createCohorts(connectionDetails)

# connection <- connect(connectionDetails)

test_that("Running multiple analyses against Eunomia", {
  skip_if(compareVersion(as.character(packageVersion("SqlRender")), "1.8.0") < 0)

  # Adding empty exposure and outcome cohorts:
  exposureOutcomeList <- list(createExposureOutcome(exposureId = 1,
                                                    outcomeId = 3),
                              createExposureOutcome(exposureId = 2,
                                                    outcomeId = 3),
                              createExposureOutcome(exposureId = 1,
                                                    outcomeId = 4),
                              createExposureOutcome(exposureId = 999,
                                                    outcomeId = 4),
                              createExposureOutcome(exposureId = 1,
                                                    outcomeId = 999))

  getDbSccsDataArgs1 <- createGetDbSccsDataArgs(deleteCovariatesSmallCount = 1)

  createStudyPopulationArgs1 <- createCreateStudyPopulationArgs(naivePeriod = 180,
                                                                firstOutcomeOnly = FALSE)

  covarExposureOfInt <- createEraCovariateSettings(label = "Exposure of interest",
                                                   includeEraIds = "exposureId",
                                                   start = 0,
                                                   end = 7,
                                                   endAnchor = "era start")

  # All outcomes occur at almost the same age, causing issues. Disable for now:
  # ageSettings <- createAgeCovariateSettings(ageKnots = 5)
  #
  # seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

  covarPreExp <- createEraCovariateSettings(label = "Pre-exposure",
                                            includeEraIds = "exposureId",
                                            start = -30,
                                            end = -1,
                                            endAnchor = "era start")

  createSccsIntervalDataArgs1 <- createCreateSccsIntervalDataArgs(eraCovariateSettings = list(covarExposureOfInt,
                                                                                              covarPreExp))

  fitSccsModelArgs <- createFitSccsModelArgs()

  sccsAnalysis1 <- createSccsAnalysis(analysisId = 1,
                                      description = "Including pre-exposure",
                                      getDbSccsDataArgs = getDbSccsDataArgs1,
                                      createStudyPopulationArgs = createStudyPopulationArgs1,
                                      createSccsIntervalDataArgs = createSccsIntervalDataArgs1,
                                      fitSccsModelArgs = fitSccsModelArgs)

  sccsAnalysisList <- list(sccsAnalysis1)

  outputFolder <- tempfile(pattern = "sccsOutput")

  analysesToExclude <- data.frame(exposureId = c(1),
                                  outcomeId = c(3))
  suppressWarnings(
    result <- runSccsAnalyses(connectionDetails = connectionDetails,
                              cdmDatabaseSchema = "main",
                              exposureDatabaseSchema = "main",
                              exposureTable = "cohort",
                              outcomeDatabaseSchema = "main",
                              outcomeTable = "cohort",
                              outputFolder = outputFolder,
                              exposureOutcomeList = exposureOutcomeList,
                              sccsAnalysisList = sccsAnalysisList,
                              analysesToExclude = analysesToExclude)
  )
  #
  #   studyPop <- readRDS(file.path(outputFolder, result$studyPopFile[1]))
  #   sccsData <- loadSccsData(file.path(outputFolder, result$sccsDataFile[1]))
  #
  #   plotAgeSpans(studyPop)
  #   SelfControlledCaseSeries::plotExposureCentered(studyPop, sccsData, 1)

  analysisSum <- summarizeSccsAnalyses(result, outputFolder)

  expect_equal(nrow(analysisSum), 5)

  unlink(outputFolder, recursive = TRUE)
})

# Remove the Eunomia database:
unlink(connectionDetails$server())
