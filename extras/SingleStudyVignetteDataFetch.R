# @file SingleStudyVignetteDataFetch.R
#
# Copyright 2021 Observational Health Data Sciences and Informatics
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

folder <- "s:/temp/vignetteSccs"
pw <- NULL
dbms <- "pdw"
user <- NULL
server <- Sys.getenv("PDW_SERVER")
cdmDatabaseSchema <- "CDM_IBM_MDCD_V1153.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
outcomeTable <- "mschuemi_sccs_vignette"
port <- Sys.getenv("PDW_PORT")
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
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id;"
sql <- SqlRender::render(sql,
                         cohortDatabaseSchema = cohortDatabaseSchema,
                         outcomeTable = outcomeTable)
sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
DatabaseConnector::querySql(connection, sql)

DatabaseConnector::disconnect(connection)

diclofenac <- 1124300
ppis <- c(911735, 929887, 923645, 904453, 948078, 19039926)

# Main section: one drug in the model ### Simple model ### ----------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
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

plotAgeSpans(studyPop)

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


saveSccsIntervalData(sccsIntervalData, "s:/temp/vignetteSccs/intervalData1.zip")
sccsIntervalData <- loadSccsIntervalData("s:/temp/vignetteSccs/intervalData1.zip")
sccsIntervalData
summary(sccsIntervalData)

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, "s:/temp/vignetteSccs/simpleModel.rds")

coef(model)
confint(model)

# Risk windows: Adding pre-exposure window --------------------------------------------------------

covarPreDiclofenac <- createEraCovariateSettings(label = "Pre-exposure",
                                                 includeEraIds = diclofenac,
                                                 start = -60,
                                                 end = -1,
                                                 endAnchor = "era start")

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData,
                                           eraCovariateSettings = list(covarDiclofenac, covarPreDiclofenac))

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, "s:/temp/vignetteSccs/preExposureModel.rds")
coef(model)

# Risk windows: Adding window splits --------------------------------

covarDiclofenacSplit <- createEraCovariateSettings(label = "Exposure of interest",
                                                   includeEraIds = diclofenac,
                                                   start = 0,
                                                   end = 0,
                                                   endAnchor = "era end",
                                                   splitPoints = c(7, 14))

covarPreDiclofenacSplit <- createEraCovariateSettings(label = "Pre-exposure",
                                                      includeEraIds = diclofenac,
                                                      start = -60,
                                                      end = -1,
                                                      endAnchor = "era start",
                                                      splitPoints = c(-30))

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData,
                                           eraCovariateSettings = list(covarDiclofenacSplit,
                                                                       covarPreDiclofenacSplit))

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, "s:/temp/vignetteSccs/splitModel.rds")
coef(model)

# Adding age and seasonality -------------------------------------------------

ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)

seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarDiclofenacSplit,
                                                                       covarPreDiclofenacSplit),
                                           ageCovariateSettings = ageCovariateSettings,
                                           seasonalityCovariateSettings = seasonalityCovariateSettings)

model <- fitSccsModel(sccsIntervalData, control = createControl(cvType = "auto",
                                                                selectorType = "byPid",
                                                                startingVariance = 0.1,
                                                                noiseLevel = "quiet",
                                                                threads = 30))
saveRDS(model, "s:/temp/vignetteSccs/ageAndSeasonModel.rds")
# model <- readRDS('s:/temp/vignetteSccs/ageAndSeasonModel.rds')
model

plotAgeEffect(model)

plotSeasonality(model)

# Adding time-dependent observation periods ----------------------------------------------

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarDiclofenacSplit,
                                                                       covarPreDiclofenacSplit),
                                           ageCovariateSettings = ageCovariateSettings,
                                           seasonalityCovariateSettings = seasonalityCovariateSettings,
                                           eventDependentObservation = TRUE)

model <- fitSccsModel(sccsIntervalData, control = createControl(cvType = "auto",
                                                                selectorType = "byPid",
                                                                startingVariance = 0.1,
                                                                noiseLevel = "quiet",
                                                                threads = 30))
saveRDS(model, "s:/temp/vignetteSccs/eventDepModel.rds")

model


# Add PPIs ------------------------------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(diclofenac, ppis),
                          cdmVersion = cdmVersion)
saveSccsData(sccsData, "s:/temp/vignetteSccs/data2.zip")
sccsData <- loadSccsData('s:/temp/vignetteSccs/data2.zip')
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
                                           eraCovariateSettings = list(covarDiclofenacSplit,
                                                                       covarPreDiclofenacSplit,
                                                                       covarPpis),
                                           ageCovariateSettings = ageCovariateSettings,
                                           seasonalityCovariateSettings = seasonalityCovariateSettings,
                                           eventDependentObservation = TRUE)

model <- fitSccsModel(sccsIntervalData, control = createControl(cvType = "auto",
                                                                selectorType = "byPid",
                                                                startingVariance = 0.1,
                                                                noiseLevel = "quiet",
                                                                threads = 30))
saveRDS(model, "s:/temp/vignetteSccs/ppiModel.rds")
model
# Add all drugs -------------------------------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          cdmVersion = cdmVersion)
saveSccsData(sccsData, "s:/temp/vignetteSccs/data3.zip")
sccsData <- loadSccsData('s:/temp/vignetteSccs/data3.zip')

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
                                           eraCovariateSettings = list(covarDiclofenacSplit,
                                                                       covarPreDiclofenacSplit,
                                                                       covarAllDrugs),
                                           ageCovariateSettings = ageCovariateSettings,
                                           seasonalityCovariateSettings = seasonalityCovariateSettings,
                                           eventDependentObservation = TRUE)

saveSccsIntervalData(sccsIntervalData, "s:/temp/vignetteSccs/sccsIntervalDataAllDrugs.zip")
sccsIntervalData <- loadSccsIntervalData("s:/temp/vignetteSccs/sccsIntervalDataAllDrugs.zip")
summary(sccsIntervalData)

control <- createControl(cvType = "auto",
                         selectorType = "byPid",
                         startingVariance = 0.001,
                         noiseLevel = "quiet",
                         threads = 30)

model <- fitSccsModel(sccsIntervalData, control = control)
saveRDS(model, "s:/temp/vignetteSccs/allDrugsModel.rds")
model
estimates <- getModel(model)
estimates[estimates$originalEraId == diclofenac, ]
estimates[estimates$originalEraId %in% ppis, ]

sccsIntervalData$outcomes %>%
  arrange(time)
