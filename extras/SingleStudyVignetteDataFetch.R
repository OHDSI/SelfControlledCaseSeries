# Copyright 2025 Observational Health Data Sciences and Informatics
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

folder <- "e:/temp/vignetteSccs"
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

# Create cohorts ---------------------------------------------------------------
connection <- DatabaseConnector::connect(connectionDetails)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable)
CohortGenerator::createCohortTables(connection = connection,
                                    cohortDatabaseSchema = cohortDatabaseSchema,
                                    cohortTableNames = cohortTableNames)
cohortDefinitionSet <- PhenotypeLibrary::getPlCohortDefinitionSet(c(356, 70))
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


# Simple model -----------------------------------------------------------------
aspirin <- 1112807
epistaxis <- 356

if (!file.exists(folder))
  dir.create(folder)

getDbSccsDataArgs = createGetDbSccsDataArgs(
  studyStartDates = "20100101",
  studyEndDates = "21000101",
  maxCasesPerOutcome = 100000,
  exposureIds = aspirin
)

sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortTable,
                          outcomeIds = epistaxis,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          getDbSccsDataArgs = getDbSccsDataArgs)


saveSccsData(sccsData, file.path(folder, "data1.zip"))
sccsData <- loadSccsData(file.path(folder, "data1.zip"))
sccsData
summary(sccsData)


createStudyPopulationArgs <- createCreateStudyPopulationArgs(
  firstOutcomeOnly = FALSE,
  naivePeriod = 180
)
studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = epistaxis,
                                  createStudyPopulationArgs = createStudyPopulationArgs
)
saveRDS(studyPop, file.path(folder, "studyPop.rds"))
studyPop <- readRDS(file.path(folder, "studyPop.rds"))

# plotAgeSpans(studyPop, maxPersons = 100)
#
# plotCalendarTimeSpans(studyPop, maxPersons = 100)
#
# plotEventObservationDependence(studyPop)
#
# plotExposureCentered(studyPop, sccsData, exposureEraId = aspirin)
#
# plotEventToCalendarTime(studyPop)
#
# getAttritionTable(studyPop)
#
# computePreExposureGain(sccsData = sccsData,
#                        studyPopulation = studyPop,
#                        exposureEraId = aspirin)
#
# stability <- computeTimeStability(studyPop)
# stability |>
#   filter(!pass)
# checkRareOutcomeAssumption(studyPop)

covarAspirin <- createEraCovariateSettings(label = "Exposure of interest",
                                           includeEraIds = aspirin,
                                           start = 0,
                                           end = 0,
                                           endAnchor = "era end")
createSccsIntervalDataArgs <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = covarAspirin
)
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData,
                                           createSccsIntervalDataArgs = createSccsIntervalDataArgs)
saveSccsIntervalData(sccsIntervalData, file.path(folder, "intervalData1.zip"))
sccsIntervalData <- loadSccsIntervalData(file.path(folder, "intervalData1.zip"))
sccsIntervalData
summary(sccsIntervalData)
metaData <- attr(sccsIntervalData, "metaData")

fitSccsModelArgs <- createFitSccsModelArgs()
model <- fitSccsModel(sccsIntervalData, fitSccsModelArgs)
saveRDS(model, file.path(folder, "simpleModel.rds"))
model

# Pre-exposure -----------------------------------------------------------------
covarPreAspirin <- createEraCovariateSettings(label = "Pre-exposure",
                                              includeEraIds = aspirin,
                                              start = -60,
                                              end = -1,
                                              endAnchor = "era start",
                                              preExposure = TRUE)
createSccsIntervalDataArgs <- createCreateSccsIntervalDataArgs(
  eraCovariateSettings = list(covarAspirin,
                              covarPreAspirin)
)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData,
                                           createSccsIntervalDataArgs = createSccsIntervalDataArgs)
model <- fitSccsModel(sccsIntervalData, fitSccsModelArgs)
saveRDS(model, file.path(folder, "preExposureModel.rds"))
model

# Adding seasonality, and calendar time -------------------------------------------------
seasonalityCovariateSettings <- createSeasonalityCovariateSettings()

calendarTimeSettings <- createCalendarTimeCovariateSettings()

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarAspirin,
                                                                       covarPreAspirin),
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
# model <- readRDS(file.path(folder, "seasonCalendarTimeModel.rds"))
model

plotSeasonality(model)

plotCalendarTimeEffect(model)

plot <- plotEventToCalendarTime(studyPopulation = studyPop,
                                sccsModel = model)
saveRDS(plot, file.path(folder, "stabilityPlot.rds"))
stability <- checkTimeStabilityAssumption(studyPopulation = studyPop)
saveRDS(stability, file.path(folder, "stabilityA.rds"))

stability <- checkTimeStabilityAssumption(studyPopulation = studyPop,
                                          sccsModel = model)
saveRDS(stability, file.path(folder, "stabilityB.rds"))
stability

# Remove COVID blip ------------------------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortTable,
                          outcomeIds = epistaxis,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = aspirin,
                          maxCasesPerOutcome = 100000,
                          studyStartDates = c("20100101", "20220101"),
                          studyEndDates = c("20191231", "21001231"))
saveSccsData(sccsData, file.path(folder, "data2.zip"))
sccsData <- loadSccsData(file.path(folder, "data2.zip"))
studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = epistaxis,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)
sccsIntervalData <- createSccsIntervalData(
  studyPopulation = studyPop,
  sccsData = sccsData,
  eraCovariateSettings = list(covarAspirin,
                              covarPreAspirin),
  seasonalityCovariateSettings = seasonalityCovariateSettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)

model <- fitSccsModel(sccsIntervalData,
                      # prior = createPrior("laplace", variance = 1),
                      control = createControl(cvType = "auto",
                                              selectorType = "byPid",
                                              startingVariance = 0.1,
                                              seed = 1,
                                              resetCoefficients = TRUE,
                                              noiseLevel = "quiet",
                                              fold = 10,
                                              cvRepetitions = 1,
                                              threads = 10))
saveRDS(model, file.path(folder, "seasonCalendarTimeCovidBlipModel.rds"))
model <- readRDS(file.path(folder, "seasonCalendarTimeCovidBlipModel.rds"))
model

plot <- plotEventToCalendarTime(studyPopulation = studyPop,
                                sccsModel = model)
saveRDS(plot, file.path(folder, "stabilityPlot2.rds"))
stability <- computeTimeStability(studyPopulation = studyPop,
                                  sccsModel = model)
saveRDS(stability, file.path(folder, "stability2.rds"))
stability


# Adding time-dependent observation periods ------------------------------------
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarAspirin,
                                                                       covarPreAspirin),
                                           eventDependentObservation = TRUE)

model <- fitSccsModel(sccsIntervalData)
saveRDS(model, file.path(folder, "eventDepModel.rds"))

model

# Add SSRIs ------------------------------------------------------------------
ssris <- c(715939, 722031, 739138, 751412, 755695, 797617, 40799195)
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortTable,
                          outcomeIds = epistaxis,
                          maxCasesPerOutcome = 100000,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(aspirin, ssris),
                          studyStartDates = c("20100101", "20220101"),
                          studyEndDates = c("20191231", "21001231"))
saveSccsData(sccsData, file.path(folder, "data2.zip"))
sccsData <- loadSccsData(file.path(folder, "data2.zip"))
summary(sccsData)
studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = epistaxis,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)
covarSsris <- createEraCovariateSettings(label = "SSRIs",
                                         includeEraIds = ssris,
                                         stratifyById = FALSE,
                                         start = 1,
                                         end = 0,
                                         endAnchor = "era end")
sccsIntervalData <- createSccsIntervalData(
  studyPopulation = studyPop,
  sccsData = sccsData,
  eraCovariateSettings = list(covarAspirin,
                              covarPreAspirin,
                              covarSsris),
  seasonalityCovariateSettings = seasonalityCovariateSettings,
  calendarTimeCovariateSettings = calendarTimeSettings
)

model <- fitSccsModel(sccsIntervalData, control = createControl(cvType = "auto",
                                                                selectorType = "byPid",
                                                                startingVariance = 0.1,
                                                                noiseLevel = "quiet",
                                                                cvRepetitions = 1,
                                                                threads = 30))
saveRDS(model, file.path(folder, "ssriModel.rds"))
model

# Add all drugs -------------------------------------------------------------------
sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = cohortTable,
                          outcomeIds = epistaxis,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          maxCasesPerOutcome = 100000,
                          studyStartDates = c("19000101", "20220101"),
                          studyEndDates = c("20191231", "21001231"))
saveSccsData(sccsData, file.path(folder, "data3.zip"))
sccsData <- loadSccsData(file.path(folder, "data3.zip"))

summary(sccsData)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = epistaxis,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)

covarAllDrugs <- createEraCovariateSettings(label = "Other exposures",
                                            includeEraIds = c(),
                                            excludeEraIds = aspirin,
                                            stratifyById = TRUE,
                                            start = 1,
                                            end = 0,
                                            endAnchor = "era end",
                                            allowRegularization = TRUE)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarAspirin,
                                                                       covarPreAspirin,
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
estimates[estimates$originalEraId == aspirin, ]
