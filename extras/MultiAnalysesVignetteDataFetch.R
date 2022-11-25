# Copyright 2022 Observational Health Data Sciences and Informatics
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
library(SqlRender)
library(DatabaseConnector)
library(SelfControlledCaseSeries)
options(andromedaTempFolder = "s:/andromedaTemp")

connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
  user = keyring::key_get("redShiftUserName"),
  password = keyring::key_get("redShiftPassword")
)
cdmDatabaseSchema <- "cdm_truven_mdcd_v1734"
cohortDatabaseSchema <- "scratch_mschuemi"
outcomeTable <- "sccs_vignette"
options(sqlRenderTempEmulationSchema = NULL)
outcomeTable <- "sccs_vignette"
cdmVersion <- "5"
outputFolder <- "s:/temp/sccsVignette2"


# Create cohorts ---------------------------------------
connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("vignette.sql",
  packageName = "SelfControlledCaseSeries",
  dbms = connectionDetails$dbms,
  cdmDatabaseSchema = cdmDatabaseSchema,
  cohortDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = outcomeTable
)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id"
sql <- SqlRender::render(sql,
  cohortDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = outcomeTable
)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

# Get all PPIs:
# sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21600095 AND concept_class_id = 'Ingredient'"
# sql <- SqlRender::render(sql, cdmDatabaseSchema = cdmDatabaseSchema)
# sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
# ppis <- DatabaseConnector::querySql(connection, sql)
# ppis <- ppis$CONCEPT_ID

DatabaseConnector::disconnect(connection)


# Create settings -----------------------------
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

exposuresOutcomeList <- list()
exposuresOutcomeList[[1]] <- createExposuresOutcome(
  outcomeId = 1,
  exposures = list(createExposure(exposureId = diclofenac))
)
for (exposureId in c(negativeControls)) {
  exposuresOutcome <- createExposuresOutcome(
    outcomeId = 1,
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

covarProphylactics <- createEraCovariateSettings(
  label = "Prophylactics",
  includeEraIds = ppis,
  start = 1,
  end = 0,
  endAnchor = "era end"
)

createSccsIntervalDataArgs2 <- createCreateSccsIntervalDataArgs(eraCovariateSettings = list(
  covarExposureOfInt,
  covarProphylactics
))

sccsAnalysis2 <- createSccsAnalysis(
  analysisId = 2,
  description = "Including prophylactics",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs2,
  fitSccsModelArgs = fitSccsModelArgs
)

ageSettings <- createAgeCovariateSettings(ageKnots = 5)

seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

covarPreExp <- createEraCovariateSettings(
  label = "Pre-exposure",
  includeEraIds = "exposureId",
  start = -30,
  end = -1,
  endAnchor = "era start"
)

createSccsIntervalDataArgs3 <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(
    covarExposureOfInt,
    covarPreExp,
    covarProphylactics
  ),
  ageCovariateSettings = ageSettings,
  seasonalityCovariateSettings = seasonalitySettings,
  eventDependentObservation = TRUE
)

sccsAnalysis3 <- createSccsAnalysis(
  analysisId = 3,
  description = "Including prophylactics, age, season, pre-exposure, and censoring",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs3,
  fitSccsModelArgs = fitSccsModelArgs
)

covarAllDrugs <- createEraCovariateSettings(
  label = "Other exposures",
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
  ageCovariateSettings = ageSettings,
  seasonalityCovariateSettings = seasonalitySettings,
  eventDependentObservation = TRUE
)

sccsAnalysis4 <- createSccsAnalysis(
  analysisId = 4,
  description = "Including all other drugs",
  getDbSccsDataArgs = getDbSccsDataArgs,
  createStudyPopulationArgs = createStudyPopulationArgs,
  createIntervalDataArgs = createSccsIntervalDataArgs4,
  fitSccsModelArgs = fitSccsModelArgs
)

sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2, sccsAnalysis3, sccsAnalysis4)

saveExposuresOutcomeList(exposuresOutcomeList, file.path(outputFolder, "exposuresOutcomeList.txt"))
saveSccsAnalysisList(sccsAnalysisList, file.path(outputFolder, "sccsAnalysisList.txt"))

# exposuresOutcomeList <- loadExposuresOutcomeList(file.path(outputFolder, "exposuresOutcomeList.txt"))
# sccsAnalysisList <- loadSccsAnalysisList(file.path(outputFolder, "sccsAnalysisList.txt"))

# Run analyses --------------------------------------------------------
multiThreadingSettings <- createDefaultSccsMultiThreadingSettings(parallel::detectCores() - 1)

referenceTable <- runSccsAnalyses(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = cdmDatabaseSchema,
  exposureDatabaseSchema = cdmDatabaseSchema,
  exposureTable = "drug_era",
  outcomeDatabaseSchema = cohortDatabaseSchema,
  outcomeTable = outcomeTable,
  cdmVersion = cdmVersion,
  outputFolder = outputFolder,
  combineDataFetchAcrossOutcomes = TRUE,
  exposuresOutcomeList = exposuresOutcomeList,
  sccsAnalysisList = sccsAnalysisList,
  sccsMultiThreadingSettings = multiThreadingSettings
)

referenceTable <- getFileReference(outputFolder)

resultsSummary <- getResultsSummary(outputFolder)


sccsData <- loadSccsData(file.path(outputFolder, result$sccsDataFile[1]))
summary(sccsData)



#
# for (i in seq_len(nrow(referenceTable))) {
#   refRow <- referenceTable[i, ]
#   sccsModel <- readRDS(file.path(outputFolder, as.character(refRow$sccsModelFile)))
#   for (j in seq_along(sccsModel$metaData$covariateSettingsList))
#     if (sccsModel$metaData$covariateSettingsList[[j]]$label == "Prophylactics") {
#       writeLines(paste("Correcting", i ))
#       sccsModel$metaData$covariateSettingsList[[j]]$exposureOfInterest <- FALSE
#       saveRDS(sccsModel, file.path(outputFolder, as.character(refRow$sccsModelFile)))
#       break
#     }
# }

