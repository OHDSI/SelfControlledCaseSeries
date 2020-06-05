# @file SingleStudyVignetteDataFetch.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
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

### Main section: one drug in the model ### Simple model ###
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = diclofenac,
                          cdmVersion = cdmVersion,
                          maxCasesPerOutcome = 1000)
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

plotExposureCentered(studyPop, sccsData)

plotEventToCalendarTime(studyPop)

getAttritionTable(studyPop)


covarDiclofenac <- createEraCovariateSettings(label = "Exposure of interest",
                                              includeCovariateIds = diclofenac,
                                              start = 0,
                                              end = 0,
                                              addExposedDaysToEnd = TRUE)

sccsEraData <- createSccsEraData(studyPop,
                                 sccsData,
                                 eraCovariateSettings = covarDiclofenac)



saveSccsEraData(sccsEraData, "s:/temp/vignetteSccs/eraData1")
sccsEraData <- loadSccsEraData("s:/temp/vignetteSccs/eraData1")
sccsEraData
summary(sccsEraData)

model <- fitSccsModel(sccsEraData, control = createControl(threads = 10))
saveRDS(model, "s:/temp/vignetteSccs/simpleModel.rds")

coef(model)
summary(model)

### Risk windows: Adding pre-exposure window ###

covarPreDiclofenac <- createCovariateSettings(label = "Pre-exposure",
                                              includeCovariateIds = diclofenac,
                                              start = -60,
                                              end = -1)

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = list(covarDiclofenac, covarPreDiclofenac))

model <- fitSccsModel(sccsEraData)
saveRDS(model, "s:/temp/vignetteSccs/preExposureModel.rds")
coef(model)
summary(model)
### Risk windows: Adding window splits ###

covarDiclofenacSplit <- createCovariateSettings(label = "Exposure of interest",
                                                includeCovariateIds = diclofenac,
                                                start = 0,
                                                end = 0,
                                                addExposedDaysToEnd = TRUE,
                                                splitPoints = c(7, 14))

covarPreDiclofenacSplit <- createCovariateSettings(label = "Pre-exposure",
                                                   includeCovariateIds = diclofenac,
                                                   start = -60,
                                                   end = -1,
                                                   splitPoints = c(-30))

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = list(covarDiclofenacSplit,
                                                          covarPreDiclofenacSplit))

model <- fitSccsModel(sccsEraData)
saveRDS(model, "s:/temp/vignetteSccs/splitModel.rds")
coef(model)
summary(model)

### Adding age and seasonality ###

ageCovariateSettings <- createAgeCovariateSettings(ageKnots = 5)

seasonalityCovariateSettings <- createSeasonalityCovariateSettings(seasonKnots = 5)

sccsEraData <- createSccsEraData(studyPopulation = studyPop,
                                 sccsData = sccsData,
                                 eraCovariateSettings = list(covarDiclofenacSplit,
                                                          covarPreDiclofenacSplit),

                                 ageSettings = ageSettings,
                                 seasonalitySettings = seasonalitySettings)

model <- fitSccsModel(sccsEraData, control = createControl(cvType = "auto",
                                                           selectorType = "byPid",
                                                           startingVariance = 0.1,
                                                           noiseLevel = "quiet",
                                                           threads = 30))
saveRDS(model, "s:/temp/vignetteSccs/ageAndSeasonModel.rds")
# model <- readRDS('s:/temp/vignetteSccs/ageAndSeasonModel.rds')
summary(model)

plotAgeEffect(model)

plotSeasonality(model)

### Adding time-dependent observation periods

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = list(covarDiclofenacSplit,
                                                          covarPreDiclofenacSplit),
                                 ageSettings = ageSettings,
                                 seasonalitySettings = seasonalitySettings,
                                 eventDependentObservation = TRUE)
model <- fitSccsModel(sccsEraData, control = createControl(cvType = "auto",
                                                           selectorType = "byPid",
                                                           startingVariance = 0.1,
                                                           noiseLevel = "quiet",
                                                           threads = 30))
saveRDS(model, "s:/temp/vignetteSccs/eventDepModel.rds")

summary(model)


### Add PPIs ###
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
saveSccsData(sccsData, "s:/temp/vignetteSccs/data2")
# sccsData <- loadSccsData('s:/temp/vignetteSccs/data2')
covarPpis <- createCovariateSettings(label = "PPIs",
                                     includeCovariateIds = ppis,
                                     stratifyById = FALSE,
                                     start = 1,
                                     end = 0,
                                     addExposedDaysToEnd = TRUE)

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = list(covarDiclofenacSplit,
                                                          covarPreDiclofenacSplit,
                                                          covarPpis),
                                 ageSettings = ageSettings,
                                 seasonalitySettings = seasonalitySettings,
                                 eventDependentObservation = TRUE)

model <- fitSccsModel(sccsEraData, control = createControl(cvType = "auto",
                                                           selectorType = "byPid",
                                                           startingVariance = 0.1,
                                                           noiseLevel = "quiet",
                                                           threads = 30))
saveRDS(model, "s:/temp/vignetteSccs/ppiModel.rds")
summary(model)
### Add all drugs ###
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
saveSccsData(sccsData, "s:/temp/vignetteSccs/data3")
# sccsData <- loadSccsData('s:/temp/vignetteSccs/data3')

covarAllDrugs <- createCovariateSettings(label = "Other exposures",
                                         excludeCovariateIds = diclofenac,
                                         stratifyById = TRUE,
                                         start = 1,
                                         end = 0,
                                         addExposedDaysToEnd = TRUE,
                                         allowRegularization = TRUE)

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = list(covarDiclofenacSplit,
                                                          covarPreDiclofenacSplit,
                                                          covarAllDrugs),
                                 ageSettings = ageSettings,
                                 seasonalitySettings = seasonalitySettings,
                                 eventDependentObservation = TRUE)

saveSccsEraData(sccsEraData, "s:/temp/vignetteSccs/sccsEraDataAllDrugs")
#sccsEraData <- loadSccsEraData("s:/temp/vignetteSccs/sccsEraDataAllDrugs")
control <- createControl(cvType = "auto",
                         selectorType = "byPid",
                         startingVariance = 0.1,
                         noiseLevel = "quiet",
                         threads = 30)
# variance <- 0.0120081
model <- fitSccsModel(sccsEraData, control = control)
saveRDS(model, "s:/temp/vignetteSccs/allDrugsModel.rds")
summary(model)
estimates <- getModel(model)
estimates[estimates$originalCovariateId == diclofenac, ]
estimates[estimates$originalCovariateId %in% ppis, ]
grep("diclofenac", as.character(model$estimates$covariateName))

