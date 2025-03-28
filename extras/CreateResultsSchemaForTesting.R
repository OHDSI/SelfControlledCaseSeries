# Code to create the inst/Results_Eunomia.zip file used in testing

library(SelfControlledCaseSeries)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)

outputFolder <- tempfile(pattern = "sccsOutput")

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
    exposures = list(createExposure(exposureId = 999)),
    outcomeId = 4
  ),
  createExposuresOutcome(
    exposures = list(createExposure(exposureId = 1)),
    outcomeId = 999
  )
)

getDbSccsDataArgs <- createGetDbSccsDataArgs(deleteCovariatesSmallCount = 1)

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
covarPreExp <- createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = c("exposureId", "exposureId2"),
  start = -30,
  end = -1,
  endAnchor = "era start",
  exposureOfInterest = FALSE,
  preExposure = TRUE
)

createSccsIntervalDataArgs <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarExposureOfInt2,
    covarPreExp
  )
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

result <- runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  exposureDatabaseSchema = "main",
  exposureTable = "cohort",
  outcomeDatabaseSchema = "main",
  outcomeTable = "cohort",
  outputFolder = outputFolder,
  sccsAnalysesSpecifications = createSccsAnalysesSpecifications(
    exposuresOutcomeList = exposuresOutcomeList,
    sccsAnalysisList = sccsAnalysisList,
    analysesToExclude = analysesToExclude
  )
)


exportToCsv(outputFolder, databaseId = "Eunomia")

file.rename(file.path(outputFolder, "export", "Results_Eunomia.zip"),
            file.path("inst", "Results_Eunomia.zip"))

unlink(connectionDetails$server())
unlink(outputFolder, recursive = TRUE)
