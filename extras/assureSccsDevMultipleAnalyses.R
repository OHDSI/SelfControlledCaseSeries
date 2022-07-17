install.packages("S:/Git/GitHub/OHDSI/SelfControlledCaseSeries",
                 repos = NULL,
                 type = "source")

# multiple analysis ADHD =======================================================

library(magrittr)
options(fftempdir = "G:/andromedaTemp",
        sqlRenderTempEmulationSchema = NULL)
baseUrl <- Sys.getenv("BASE_URL")
ROhdsiWebApi::authorizeWebApi(baseUrl, "windows")

outputFolder <- "G:/assureSccsMultiple"
if(!file.exists(outputFolder)){
  dir.create(outputFolder)
}

## generate target-outcome cohorts =============================================
databaseName <- "optum_extended_dod"
cdmDatabaseSchema <- "cdm_optum_extended_dod_v2012"
cohortDatabaseSchema <- "scratch_jweave17"
cohortTable <- "assure_sccs_adhd"

nestingCohortId <- 8300
exposureCohortIds <- c(7820, 7334, 7127)
outcomeCohortId <- 8307
cohortIds <- c(nestingCohortId, exposureCohortIds, outcomeCohortId)

cohortDefinitionSet <- ROhdsiWebApi::exportCohortDefinitionSet(
  baseUrl = baseUrl,
  cohortIds = cohortIds
)

CohortGenerator::saveCohortDefinitionSet(
  cohortDefinitionSet = cohortDefinitionSet,
  settingsFileName = file.path("inst/settings/CohortsToCreate.csv"),
  jsonFolder = file.path("inst/cohorts"),
  sqlFolder = file.path("inst/sql/sql_server")
)

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = Sys.getenv("DBMS"),
  server = paste0(Sys.getenv("OHDA_SERVER"), databaseName),
  extraSettings = Sys.getenv("EXTRA_SETTINGS"),
  port = Sys.getenv("port"),
  user = Sys.getenv("OHDA_USER"),
  password = Sys.getenv("OHDA_PASSWORD")
)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTable)

CohortGenerator::createCohortTables(
  connectionDetails = connectionDetails,
  cohortTableNames = cohortTableNames,
  cohortDatabaseSchema = cohortDatabaseSchema
)

cohortsGenerated <- CohortGenerator::generateCohortSet(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  cohortTableNames = cohortTableNames,
  cohortDefinitionSet = cohortDefinitionSet
)

## generate negative control outcome cohorts ===================================
pathToCsv <- "S:/Git/GitHub/OHDSI/SelfControlledCaseSeries/inst/settings/NegativeControls.csv"
negativeControls <- readr::read_csv(pathToCsv, col_types = readr::cols())

ncSql <- SqlRender::loadRenderTranslateSql(
  sqlFilename = "NegativeControlOutcomes.sql",
  packageName = "SelfControlledCaseSeries",
  dbms = connectionDetails$dbms,
  cdm_database_schema = cdmDatabaseSchema,
  cohort_database_schema = cohortDatabaseSchema,
  cohort_table = cohortTable,
  outcome_ids = unique(negativeControls$outcomeId)
)
connection <- DatabaseConnector::connect(connectionDetails)
DatabaseConnector::executeSql(connection, ncSql)



## specify analyses ===========================================================
nestingCohortId <- 8300
exposureCohortIds <- c(7820, 7334, 7127) # methylphenidate, lisdexamfetamine, dexmethylphenidate
outcomeCohortId <- 8307
negativeControlOutcomeIds <- unique(negativeControls$outcomeId)

outcomeCohortIds <- unique(c(outcomeCohortId, negativeControlOutcomeIds))
exposureOutcomeList <- list()
for (exposureCohortId in exposureCohortIds) {
  for (outcomeCohortId in outcomeCohortIds){
    exposureOutcome <- SelfControlledCaseSeries::createExposureOutcome(
      exposureId = exposureCohortId,
      outcomeId = outcomeCohortId,
      trueEffectSize = ifelse(outcomeCohortId %in% negativeControlOutcomeIds, 1, NA)
    )
    exposureOutcomeList[[length(exposureOutcomeList) + 1]] <- exposureOutcome
  }
}
SelfControlledCaseSeries::saveExposureOutcomeList(
  exposureOutcomeList = exposureOutcomeList,
  file = file.path(outputFolder, "exposureOutcomeList.json")
)

getDbSccsDataArgs <- SelfControlledCaseSeries::createGetDbSccsDataArgs(
  useCustomCovariates = FALSE,
  useNestingCohort = TRUE,
  nestingCohortId = nestingCohortId,
  deleteCovariatesSmallCount = 100,
  studyStartDate = "20170101",
  studyEndDate = "",
  maxCasesPerOutcome = 0,
  exposureIds = "exposureId"
)

createStudyPopulationArgs <- SelfControlledCaseSeries::createCreateStudyPopulationArgs(
  firstOutcomeOnly = FALSE,
  naivePeriod = 180
)

covarExposureOfInt <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Exposure of interest",
  includeEraIds = "exposureId",
  stratifyById = TRUE,
  start = 1,
  startAnchor = "era start",
  end = 0,
  endAnchor = "era end",
  firstOccurrenceOnly = FALSE,
  profileLikelihood = TRUE,
  trueEffectSize = "trueEffectSize"
)

covarPreExp <- SelfControlledCaseSeries::createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = "exposureId",
  start = -30,
  end = -1,
  endAnchor = "era start",
  trueEffectSize = NA
)

seasonalitySettings <- SelfControlledCaseSeries::createSeasonalityCovariateSettings(
  seasonKnots = 5
)

fitSccsModelArgs <- SelfControlledCaseSeries::createFitSccsModelArgs()

createSccsIntervalDataArgs1 <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
  eraCovariateSettings = covarExposureOfInt
)

createSccsIntervalDataArgs2 <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(covarPreExp,
                              covarExposureOfInt)
)

createSccsIntervalDataArgs3 <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
  eraCovariateSettings = covarExposureOfInt,
  seasonalityCovariateSettings = seasonalitySettings,
  minCasesForTimeCovariates = 10000,
  eventDependentObservation = FALSE
)

createSccsIntervalDataArgs4 <- SelfControlledCaseSeries::createCreateSccsIntervalDataArgs(
  eraCovariateSettings = covarExposureOfInt,
  seasonalityCovariateSettings = NULL,
  eventDependentObservation = TRUE
)


sccsAnalysis1 <- SelfControlledCaseSeries::createSccsAnalysis(
  analysisId = 1,
  description = "EOI only",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createSccsIntervalDataArgs = createSccsIntervalDataArgs1,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysis2 <- SelfControlledCaseSeries::createSccsAnalysis(
  analysisId = 2,
  description = "EOI and pre-exposure",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createSccsIntervalDataArgs = createSccsIntervalDataArgs2,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysis3 <- SelfControlledCaseSeries::createSccsAnalysis(
  analysisId = 3,
  description = "EOI and seasonality",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createSccsIntervalDataArgs = createSccsIntervalDataArgs3,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysis4 <- SelfControlledCaseSeries::createSccsAnalysis(
  analysisId = 4,
  description = "EOI and event dep obsv",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createSccsIntervalDataArgs = createSccsIntervalDataArgs4,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysisList <- list(sccsAnalysis1,
                         sccsAnalysis2,
                         sccsAnalysis3,
                         sccsAnalysis4)

SelfControlledCaseSeries::saveSccsAnalysisList(
  sccsAnalysisList = sccsAnalysisList,
  file = file.path(outputFolder, "sccsAnalysisList.json")
)



## run analyses ================================================================

sccsAnalysisList <- SelfControlledCaseSeries::loadSccsAnalysisList(
  file = file.path(outputFolder, "sccsAnalysisList.json")
)

exposureOutcomeList <- SelfControlledCaseSeries::loadExposureOutcomeList(
  file = file.path(outputFolder, "exposureOutcomeList.json")
)

result <- SelfControlledCaseSeries::runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  exposureDatabaseSchema = cohortDatabaseSchema,
  exposureTable = cohortTable,
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  nestingCohortDatabaseSchema = cohortDatabaseSchema,
  nestingCohortTable = cohortTable,
  outputFolder = outputFolder,
  sccsAnalysisList = sccsAnalysisList,
  exposureOutcomeList = exposureOutcomeList,
  getDbSccsDataThreads = 1,
  createStudyPopulationThreads = 1,
  createSccsIntervalDataThreads = 1,
  fitSccsModelThreads = 1
)

sccsSummary <- SelfControlledCaseSeries::summarizeSccsAnalyses(
  referenceTable = result,
  outputFolder = outputFolder
)

sccsSummary <- addCohortNames(sccsSummary, "exposureId", "exposureName")
sccsSummary <- addCohortNames(sccsSummary, "outcomeId", "outcomeName")
sccsSummary <- addAnalysisDescription(sccsSummary, sccsAnalysisList, "analysisId", "analysisDescription")

sccsSummary <- sccsSummary %>%
  dplyr::arrange(analysisId, exposureId, outcomeId) %>%
  dplyr::relocate(analysisId, analysisDescription, exposureId, exposureName)

sccsSummaryFile <- file.path(outputFolder, "sccs_summary.csv")
readr::write_csv(sccsSummary, sccsSummaryFile)



addCohortNames <- function(data,
                           IdColumnName = "cohortDefinitionId",
                           nameColumnName = "cohortName") {
  # pathToCsv <- system.file("settings", "CohortsToCreate.csv", package = "SelfControlledCaseSeries")
  pathToCsv <- "S:/Git/GitHub/OHDSI/SelfControlledCaseSeries/inst/settings/CohortsToCreate.csv"
  cohortsToCreate <- read.csv(pathToCsv)
  names(cohortsToCreate) <- SqlRender::snakeCaseToCamelCase(names(cohortsToCreate))

  # pathToCsv <- system.file("settings", "NegativeControls.csv", package = "SelfControlledCaseSeries")
  pathToCsv <- "S:/Git/GitHub/OHDSI/SelfControlledCaseSeries/inst/settings/NegativeControls.csv"
  negativeControls <- read.csv(pathToCsv)

  idToName <- data.frame(
    cohortId = c(
      cohortsToCreate$cohortId,
      negativeControls$outcomeId
    ),
    cohortName = c(
      as.character(cohortsToCreate$cohortName),
      as.character(negativeControls$outcomeName)
    )
  )
  idToName <- idToName[order(idToName$cohortId), ]
  idToName <- idToName[!duplicated(idToName$cohortId), ]
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  # Change order of columns:
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data), (idCol + 1):(ncol(data) - 1))]
  }
  return(data)
}

addAnalysisDescription <- function(data,
                                   analysisSettingsList,
                                   IdColumnName = "analysisId",
                                   nameColumnName = "analysisDescription") {
  idToName <- lapply(analysisSettingsList, function(x) data.frame(analysisId = x$analysisId, description = as.character(x$description)))
  idToName <- do.call("rbind", idToName)
  names(idToName)[1] <- IdColumnName
  names(idToName)[2] <- nameColumnName
  data <- merge(data, idToName, all.x = TRUE)
  # Change order of columns:
  idCol <- which(colnames(data) == IdColumnName)
  if (idCol < ncol(data) - 1) {
    data <- data[, c(1:idCol, ncol(data), (idCol + 1):(ncol(data) - 1))]
  }
  return(data)
}









