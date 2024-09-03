# Copyright 2024 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCaseSeries
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This code should be used to fetch the data that is used in the vignettes.
library(SelfControlledCaseSeries)
options(andromedaTempFolder = "e:/andromedaTemp")

connectionDetails <- createConnectionDetails(
  dbms = "spark",
  connectionString = keyring::key_get("databricksConnectionString"),
  user = "token",
  password = keyring::key_get("databricksToken")
)
cdmDatabaseSchema <- "merative_mdcr.cdm_merative_mdcr_v3045"
cohortDatabaseSchema <- "scratch.scratch_mschuemi"
cohortTable  <- "sccs_vignette"
options(sqlRenderTempEmulationSchema = "scratch.scratch_mschuemi")
outputFolder <- "e:/temp/sccsVignette2"


# Create cohorts ---------------------------------------------------------------
connection <- DatabaseConnector::connect(connectionDetails)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
CohortGenerator::createCohortTables(connection = connection,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTableNames = cohortTableNames)
cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(77)
counts <- CohortGenerator::generateCohortSet(connection = connection,
                                             cdmDatabaseSchema = cdmDatabaseSchema,
                                             cohortDatabaseSchema = cohortDatabaseSchema,
                                             cohortTableNames = cohortTableNames,
                                             cohortDefinitionSet = cohortDefinitionSet)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@cohortTable GROUP BY cohort_definition_id;"
sql <- SqlRender::render(sql,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         cohortTable = cohortTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)


# Create settings --------------------------------------------------------------
negativeControls <- c(
  705178,
  705944,
  710650,
  714785,
  719174,
  719311,
  735340,
  742185,
  780369,
  781182,
  924724,
  990760,
  1110942,
  1111706,
  1136601,
  1317967,
  1501309,
  1505346,
  1551673,
  1560278,
  1584910,
  19010309,
  40163731
)
diclofenac <- 1124300
ppis <- c(911735, 929887, 923645, 904453, 948078, 19039926)
giBleed <- 77

exposuresOutcomeList <- list()
exposuresOutcomeList[[1]] <- createExposuresOutcome(
  outcomeId = giBleed,
  exposures = list(createExposure(exposureId = diclofenac))
)
for (exposureId in c(negativeControls)) {
  exposuresOutcome <- createExposuresOutcome(
    outcomeId = giBleed,
    exposures = list(createExposure(exposureId = exposureId, trueEffectSize = 1))
  )
  exposuresOutcomeList[[length(exposuresOutcomeList) + 1]] <- exposuresOutcome
}

getDbSccsDataArgs <- createGetDbSccsDataArgs(
  useCustomCovariates = FALSE,
  deleteCovariatesSmallCount = 100,
  exposureIds = c(),
  maxCasesPerOutcome = 100000
)

createStudyPopulationArgs <- createCreateStudyPopulationArgs(
  naivePeriod = 180,
  firstOutcomeOnly = FALSE
)

covarExposureOfInt <- createEraCovariateSettings(
  label = "Exposure of interest",
  includeEraIds = "exposureId",
  start = 1,
  end = 0,
  endAnchor = "era end",
  profileLikelihood = TRUE,
  exposureOfInterest = TRUE
)

createSccsIntervalDataArgs1 <- createCreateSccsIntervalDataArgs(eraCovariateSettings = covarExposureOfInt)

fitSccsModelArgs <- createFitSccsModelArgs()

sccsAnalysis1 <- createSccsAnalysis(
  analysisId = 1,
  description = "Simplest model",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs1,
  fitSccsModelArgs = fitSccsModelArgs
)


covarPreExp <- createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = "exposureId",
  start = -30,
  end = -1,
  endAnchor = "era start"
)

covarProphylactics <- createEraCovariateSettings(
  label = "Prophylactics",
  includeEraIds = ppis,
  start = 1,
  end = 0,
  endAnchor = "era end"
)

createSccsIntervalDataArgs2 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  )
)

sccsAnalysis2 <- createSccsAnalysis(
  analysisId = 2,
  description = "Including prophylactics and pre-exposure",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs2,
  fitSccsModelArgs = fitSccsModelArgs
)

seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

calendarTimeSettings <- createCalendarTimeCovariateSettings(calendarTimeKnots = 5)

createSccsIntervalDataArgs3 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  ),
  seasonalityCovariateSettings = seasonalitySettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)

sccsAnalysis3 <- createSccsAnalysis(
  analysisId = 3,
  description = "Including prophylactics, season, calendar time, and pre-exposure",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs3,
  fitSccsModelArgs = fitSccsModelArgs
)

covarAllDrugs <- createEraCovariateSettings(
  label = "Other exposures",
  includeEraIds = c(),
  excludeEraIds = "exposureId",
  stratifyById = TRUE,
  start = 1,
  end = 0,
  endAnchor = "era end",
  allowRegularization = TRUE
)

createSccsIntervalDataArgs4 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarAllDrugs
  ),
  seasonalityCovariateSettings = seasonalitySettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)

sccsAnalysis4 <- createSccsAnalysis(
  analysisId = 4,
  description = "Including all other drugs",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs4,
  fitSccsModelArgs = fitSccsModelArgs
)

createSccsIntervalDataArgs5 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  ),
  eventDependentObservation = TRUE
)

sccsAnalysis5 <- createSccsAnalysis(
  analysisId = 5,
  description = "Adjusting for event-dependent obs. end",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs5,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2, sccsAnalysis3, sccsAnalysis4, sccsAnalysis5)

saveExposuresOutcomeList(exposuresOutcomeList, file.path(outputFolder, "exposuresOutcomeList.txt"))
saveSccsAnalysisList(sccsAnalysisList, file.path(outputFolder, "sccsAnalysisList.txt"))

# Run analyses --------------------------------------------------------
exposuresOutcomeList <- loadExposuresOutcomeList(file.path(outputFolder, "exposuresOutcomeList.txt"))
sccsAnalysisList <- loadSccsAnalysisList(file.path(outputFolder, "sccsAnalysisList.txt"))
multiThreadingSettings <- createDefaultSccsMultiThreadingSettings(parallel::detectCores() - 1)

runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = cohortTable,
  outputFolder = outputFolder,
  combineDataFetchAcrossOutcomes = TRUE,
  exposuresOutcomeList = exposuresOutcomeList,
  sccsAnalysisList = sccsAnalysisList,
  sccsMultiThreadingSettings = multiThreadingSettings,
  controlType = "exposure"
)

referenceTable <- getFileReference(outputFolder)
resultsSummary <- getResultsSummary(outputFolder)

# Export results ---------------------------------------------------------------
exportToCsv(
  outputFolder = outputFolder,
  exportFolder = file.path(outputFolder, "export"),
  databaseId = "MDCD",
  minCellCount = 5,
  maxCores = 1
)

# Upload results to SQLite -----------------------------------------------------
library(SelfControlledCaseSeries)
databaseFile <-  file.path(outputFolder, "export", "SccsResults.sqlite")
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = databaseFile
)
createResultsDataModel(
  connectionDetails = connectionDetails,
  databaseSchema = "main",
  tablePrefix = ""
)
uploadResults(
  connectionDetails = connectionDetails,
  schema = "main",
  zipFileName = file.path(outputFolder, "export", "Results_MDCD.zip"),
  purgeSiteDataBeforeUploading = FALSE
)
# Add cohort and database tables:
connection <- DatabaseConnector::connect(connectionDetails)
cohorts <- tibble(
  cohortDefinitionId = c(diclofenac, giBleed, ppis, negativeControls),
  cohortName = c("Diclofenac", "GI Bleed", paste0("PPI_", seq_along(ppis)), paste0("NegativeControl_", seq_along(negativeControls))),
  isCohort = 0,
  description = "",
  json = "{}",
  sqlCommand = ""
)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "cg_cohort_definition",
  data = cohorts,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE
)
databases <- tibble(
  database_id = "MDCD",
  cdm_source_name = "Merative Marketscan MDCD",
  cdm_source_abbreviation = "MDCD"
)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "database_meta_data",
  data = databases,
  dropTableIfExists = TRUE,
  createTable = TRUE
)
DatabaseConnector::disconnect(connection)

# Launch Shiny app -------------------------------------------------------------
library(dplyr)
outputFolder <- "e:/temp/sccsVignette2"
databaseFile <-  file.path(outputFolder, "export", "SccsResults.sqlite")
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "sqlite",
  server = databaseFile
)
# conn <- DatabaseConnector::connect(connectionDetails)
resultDatabaseDetails <- list(
  dbms = connectionDetails$dbms,
  tablePrefix = 'sccs_',
  cohortTablePrefix = 'cg_',
  databaseTablePrefix = '',
  schema = "main",
  databaseTable = 'database_meta_data'
)
estimationModule <- ShinyAppBuilder::createDefaultEstimationConfig()
aboutModule <- ShinyAppBuilder::createDefaultAboutConfig()
shinyAppConfig <- ShinyAppBuilder::initializeModuleConfig() %>%
  ShinyAppBuilder::addModuleConfig(aboutModule) %>%
  ShinyAppBuilder::addModuleConfig(estimationModule)
connectionHandler <- ResultModelManager::ConnectionHandler$new(connectionDetails)
ShinyAppBuilder::viewShiny(shinyAppConfig, connectionHandler)
connectionHandler$closeConnection()
