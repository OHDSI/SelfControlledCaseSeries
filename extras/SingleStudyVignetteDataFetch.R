# @file SingleStudyVignetteDataFetch.R
#
# Copyright 2023 Observational Health Data Sciences and Informatics
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
options(andromedaTempFolder = "d:/andromedaTemp")

folder <- "d:/temp/vignetteSccs"
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "redshift",
  connectionString = keyring::key_get("redShiftConnectionStringOhdaMdcd"),
  user = keyring::key_get("redShiftUserName"),
  password = keyring::key_get("redShiftPassword")
)
cdmDatabaseSchema <- "cdm_truven_mdcd_v1734"
cohortDatabaseSchema <- "scratch_mschuemi"
outcomeTable <- "sccs_vignette"
cdmVersion <- "5"
options(sqlRenderTempEmulationSchema = NULL)

# Create cohorts ---------------------------------------------------------------
connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("vignette.sql",
                              packageName = "SelfControlledCaseSeries",
                              dbms = connectionDetails$dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              outcomeTable = outcomeTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id;"
sql <- SqlRender::render(sql,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         outcomeTable = outcomeTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)


# Simple model -----------------------------------------------------------------
diclofenac <- 1124300
ppis <- c(911735, 929887, 923645, 904453, 948078, 19039926)

if (!file.exists(folder))
  dir.create(folder)


sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = diclofenac,
                          cdmVersion = cdmVersion)
saveSccsData(sccsData, file.path(folder, "data1.zip"))
sccsData <- loadSccsData(file.path(folder, "data1.zip"))
sccsData
summary(sccsData)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 1,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)

saveRDS(studyPop, file.path(folder, "studyPop.rds"))

plotAgeSpans(studyPop)

plotCalendarTimeSpans(studyPop)

plotEventObservationDependence(studyPop)

plotExposureCentered(studyPop, sccsData, exposureEraId = diclofenac)

plotEventToCalendarTime(studyPop)

getAttritionTable(studyPop)


covarDiclofenac <- createEraCovariateSettings(label = "Exposure of interest",
                                              includeEraIds = diclofenac,
                                              start = 0,
                                              end = 0,
                                              endAnchor = "era end")

sccsIntervalData <- createSccsIntervalData(studyPop,
                                           sccsData,
                                           eraCovariateSettings = covarDiclofenac)


saveSccsIntervalData(sccsIntervalData, file.path(folder, "intervalData1.zip"))
sccsIntervalData <- loadSccsIntervalData(file.path(folder, "intervalData1.zip"))
sccsIntervalData
summary(sccsIntervalData)

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, file.path(folder, "simpleModel.rds"))

model

computeMdrr(sccsIntervalData,
            exposureCovariateId = 1000,
            alpha = 0.05,
            power = 0.8,
            twoSided = TRUE,
            method = "binomial")


metaData <- attr(sccsIntervalData, "metaData")
metaData$covariateStatistics
getAttritionTable(sccsIntervalData)
# Risk windows: Adding pre-exposure window --------------------------------------------------------

covarPreDiclofenac <- createEraCovariateSettings(label = "Pre-exposure",
                                                 includeEraIds = diclofenac,
                                                 start = -60,
                                                 end = -1,
                                                 endAnchor = "era start")

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData,
                                           eraCovariateSettings = list(covarDiclofenac,
                                                                       covarPreDiclofenac))

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, file.path(folder, "preExposureModel.rds"))
model

# Adding seasonality, and calendar time -------------------------------------------------
seasonalityCovariateSettings <- createSeasonalityCovariateSettings()

calendarTimeSettings <- createCalendarTimeCovariateSettings()

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarDiclofenac,
                                                                       covarPreDiclofenac),
                                           seasonalityCovariateSettings = seasonalityCovariateSettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings)

model <- fitSccsModel(sccsIntervalData,
                      control = createControl(cvType = "auto",
                                              selectorType = "byPid",
                                              startingVariance = 0.1,
                                              seed = 1,
                                              resetCoefficients = TRUE,
                                              noiseLevel = "quiet",
                                              fold = 10,
                                              cvRepetitions = 1,
                                              threads = 10))
saveRDS(model, file.path(folder, "seasonCalendarTimeModel.rds"))
# model <- readRDS('s:/temp/vignetteSccs/ageSeasonCalendarTimeModel.rds')
model

plotSeasonality(model)

plotCalendarTimeEffect(model)

# Adding time-dependent observation periods ----------------------------------------------
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarDiclofenac,
                                                                       covarPreDiclofenac),
                                           eventDependentObservation = TRUE)

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, file.path(folder, "eventDepModel.rds"))

model

# Add PPIs ------------------------------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(diclofenac, ppis),
                          cdmVersion = cdmVersion)
saveSccsData(sccsData, file.path(folder, "data2.zip"))
sccsData <- loadSccsData(file.path(folder, "data2.zip"))
summary(sccsData)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 1,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)

covarPpis <- createEraCovariateSettings(label = "PPIs",
                                        includeEraIds = ppis,
                                        stratifyById = FALSE,
                                        start = 1,
                                        end = 0,
                                        endAnchor = "era end")

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarDiclofenac,
                                                                       covarPreDiclofenac,
                                                                       covarPpis),
                                           seasonalityCovariateSettings = seasonalityCovariateSettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings)

model <- fitSccsModel(sccsIntervalData, control = createControl(cvType = "auto",
                                                                selectorType = "byPid",
                                                                startingVariance = 0.1,
                                                                noiseLevel = "quiet",
                                                                threads = 30))
saveRDS(model, file.path(folder, "ppiModel.rds"))
model
# Add all drugs -------------------------------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          cdmVersion = cdmVersion)
saveSccsData(sccsData, file.path(folder, "data3.zip"))
sccsData <- loadSccsData(file.path(folder, "data3.zip"))

summary(sccsData)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 1,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)

covarAllDrugs <- createEraCovariateSettings(label = "Other exposures",
                                            excludeEraIds = diclofenac,
                                            stratifyById = TRUE,
                                            start = 1,
                                            end = 0,
                                            endAnchor = "era end",
                                            allowRegularization = TRUE)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarDiclofenac,
                                                                       covarPreDiclofenac,
                                                                       covarAllDrugs),
                                           seasonalityCovariateSettings = seasonalityCovariateSettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings)

saveSccsIntervalData(sccsIntervalData, file.path(folder, "sccsIntervalDataAllDrugs.zip"))
sccsIntervalData <- loadSccsIntervalData(file.path(folder, "sccsIntervalDataAllDrugs.zip"))
summary(sccsIntervalData)

control <- createControl(cvType = "auto",
                         selectorType = "byPid",
                         startingVariance = 0.001,
                         noiseLevel = "quiet",
                         threads = 30)

model <- fitSccsModel(sccsIntervalData, control = control)
saveRDS(model, file.path(folder, "allDrugsModel.rds"))
model
estimates <- getModel(model)
estimates[estimates$originalEraId == diclofenac, ]
estimates[estimates$originalEraId %in% ppis, ]
