# @file MultiAnalysesVignetteDataFetch.R
#
# Copyright 2016 Observational Health Data Sciences and Informatics
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
setwd("s:/temp")
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- "JRDUSAPSCTL01"
cdmDatabaseSchema <- "cdm_truven_mdcd_v5.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
outcomeTable <- "mschuemi_sccs_vignette"
port <- 17001
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("vignette.sql",
                              packageName = "SelfControlledCaseSeries",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              outcomeTable = outcomeTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            outcomeTable = outcomeTable)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

# Get all PPIs:
sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21600095 AND concept_class_id = 'Ingredient'"
sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
ppis <- DatabaseConnector::querySql(connection, sql)
ppis <- ppis$CONCEPT_ID

negativeControls <- c(705178,
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
                      19044727,
                      40163731)
diclofenac <- 1124300
ppis <- c(911735, 929887, 923645, 904453, 948078, 19039926)

exposureOutcomeList <- list()
for (exposureId in c(diclofenac, negativeControls)) {
  exposureOutcome <- createExposureOutcome(exposureId = exposureId,
                                           outcomeId = 1,
                                           prophylactics = ppis)
  exposureOutcomeList[[length(exposureOutcomeList) + 1]] <- exposureOutcome
}

getDbSccsDataArgs1 <- createGetDbSccsDataArgs(useCustomCovariates = FALSE,
                                              deleteCovariatesSmallCount = 100,
                                              studyStartDate = "",
                                              studyEndDate = "",
                                              exposureIds = c())

covarExposureOfInt <- createCovariateSettings(label = "Exposure of interest",
                                              includeCovariateIds = "exposureId",
                                              start = 0,
                                              end = 0,
                                              addExposedDaysToEnd = TRUE)

createSccsEraDataArgs1 <- createCreateSccsEraDataArgs(naivePeriod = 180,
                                                      firstOutcomeOnly = FALSE,
                                                      covariateSettings = covarExposureOfInt)

fitSccsModelArgs <- createFitSccsModelArgs()

sccsAnalysis1 <- createSccsAnalysis(analysisId = 1,
                                    description = "Simplest model",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createSccsEraDataArgs = createSccsEraDataArgs1,
                                    fitSccsModelArgs = fitSccsModelArgs)

covarProphylactics <- createCovariateSettings(label = "Prophylactics",
                                              includeCovariateIds = "prophylactics",
                                              start = 0,
                                              end = 0,
                                              addExposedDaysToEnd = TRUE)

createSccsEraDataArgs2 <- createCreateSccsEraDataArgs(naivePeriod = 180,
                                                      firstOutcomeOnly = FALSE,
                                                      covariateSettings = list(covarExposureOfInt,
                                                                               covarProphylactics))

sccsAnalysis2 <- createSccsAnalysis(analysisId = 2,
                                    description = "Including prophylactics",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createSccsEraDataArgs = createSccsEraDataArgs2,
                                    fitSccsModelArgs = fitSccsModelArgs)

ageSettings <- createAgeSettings(includeAge = TRUE, ageKnots = 5)

seasonalitySettings <- createSeasonalitySettings(includeSeasonality = TRUE, seasonKnots = 5)

createSccsEraDataArgs3 <- createCreateSccsEraDataArgs(naivePeriod = 180,
                                                      firstOutcomeOnly = FALSE,
                                                      covariateSettings = list(covarExposureOfInt,
                                                                               covarProphylactics),
                                                      ageSettings = ageSettings,
                                                      seasonalitySettings = seasonalitySettings,
                                                      eventDependentObservation = TRUE)

sccsAnalysis3 <- createSccsAnalysis(analysisId = 3,
                                    description = "Including prophylactics, age, season, and censoring",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createSccsEraDataArgs = createSccsEraDataArgs3,
                                    fitSccsModelArgs = fitSccsModelArgs)

covarAllDrugs <- createCovariateSettings(label = "Other exposures",
                                         excludeCovariateIds = "exposureId",
                                         stratifyById = TRUE,
                                         start = 1,
                                         end = 0,
                                         addExposedDaysToEnd = TRUE,
                                         allowRegularization = TRUE)

createSccsEraDataArgs4 <- createCreateSccsEraDataArgs(naivePeriod = 180,
                                                      firstOutcomeOnly = FALSE,
                                                      covariateSettings = list(covarExposureOfInt,
                                                                               covarAllDrugs))

sccsAnalysis4 <- createSccsAnalysis(analysisId = 4,
                                    description = "Including all other drugs",
                                    getDbSccsDataArgs = getDbSccsDataArgs1,
                                    createSccsEraDataArgs = createSccsEraDataArgs4,
                                    fitSccsModelArgs = fitSccsModelArgs)

sccsAnalysisList <- list(sccsAnalysis1, sccsAnalysis2, sccsAnalysis3, sccsAnalysis4)

saveExposureOutcomeList(exposureOutcomeList, "s:/temp/sccsVignette2/exposureOutcomeList.txt")
saveSccsAnalysisList(sccsAnalysisList, "s:/temp/sccsVignette2/sccsAnalysisList.txt")

# exposureOutcomeList <- loadExposureOutcomeList('s:/temp/sccsVignette2/exposureOutcomeList.txt')
# sccsAnalysisList <- loadSccsAnalysisList('s:/temp/sccsVignette2/sccsAnalysisList.txt')

result <- runSccsAnalyses(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = cdmDatabaseSchema,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          cdmVersion = cdmVersion,
                          outputFolder = "s:/temp/sccsVignette2",
                          combineDataFetchAcrossOutcomes = TRUE,
                          exposureOutcomeList = exposureOutcomeList,
                          sccsAnalysisList = sccsAnalysisList,
                          getDbSccsDataThreads = 1,
                          createSccsEraDataThreads = 5,
                          fitSccsModelThreads = 3,
                          cvThreads = 10)

# result <- readRDS('s:/temp/sccsVignette2/outcomeModelReference.rds')

analysisSum <- summarizeSccsAnalyses(result)
saveRDS(analysisSum, "s:/temp/sccsVignette2/analysisSummary.rds")

