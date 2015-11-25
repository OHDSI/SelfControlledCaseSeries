# @file VignetteDataFetch.R
#
# Copyright 2015 Observational Health Data Sciences and Informatics
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

# This function should be used to fetch the data that is used in the vignettes.
.vignetteDataFetch <- function() {
  # library(SqlRender);library(DatabaseConnector) ;library(SelfControlledCaseSeries);
  setwd("s:/temp")
  options("fftempdir" = "s:/fftemp")

  pw <- NULL
  dbms <- "pdw"
  user <- NULL
  server <- "JRDUSAPSCTL01"
  cdmDatabaseSchema <- "cdm_truven_mdcd.dbo"
  cohortDatabaseSchema <- "scratch.dbo"
  oracleTempSchema <- NULL
  outcomeTable <- "mschuemi_sccs_vignette"
  port <- 17001
  cdmVersion <- "4"

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
  sql <- "SELECT cohort_concept_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_concept_id"
  sql <- SqlRender::renderSql(sql, cohortDatabaseSchema = cohortDatabaseSchema, outcomeTable = outcomeTable)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  DatabaseConnector::querySql(connection, sql)

  # Get all PPIs:
  sql <- "SELECT concept_id FROM @cdmDatabaseSchema.concept_ancestor INNER JOIN @cdmDatabaseSchema.concept ON descendant_concept_id = concept_id WHERE ancestor_concept_id = 21600095 AND concept_class = 'Ingredient'"
  sql <- SqlRender::renderSql(sql, cdmDatabaseSchema = cdmDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  ppis <- DatabaseConnector::querySql(connection, sql)
  ppis <- ppis$CONCEPT_ID

  diclofenac <- 1124300

  ### Main section: one drug in the model ###
  ### Simple model ###
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
  saveSccsData(sccsData, "s:/temp/vignetteSccs/data1")
  #sccsData <- loadSccsData("s:/temp/vignetteSccs/data1")
  covarDiclofenac = createCovariateSettings(label = "Exposure of interest",
                                            includeCovariateIds = diclofenac,
                                            start = 0,
                                            end = 0,
                                            addExposedDaysToEnd = TRUE)

  sccsEraData <- createSccsEraData(sccsData,
                                   naivePeriod = 180,
                                   firstOutcomeOnly = FALSE,
                                   covariateSettings = covarDiclofenac)
  summary(sccsEraData)

  model <- fitSccsModel(sccsEraData)
  saveRDS(model, "s:/temp/vignetteSccs/simpleModel.rds")

  coef(model)
  summary(model)

  ### Risk windows: Adding pre-exposure window ###

  covarPreDiclofenac = createCovariateSettings(label = "Pre-exposure",
                                               includeCovariateIds = diclofenac,
                                               start = -60,
                                               end = -1)

  sccsEraData <- createSccsEraData(sccsData,
                                   naivePeriod = 180,
                                   firstOutcomeOnly = FALSE,
                                   covariateSettings = list(covarDiclofenac,
                                                            covarPreDiclofenac))

  model <- fitSccsModel(sccsEraData)
  saveRDS(model, "s:/temp/vignetteSccs/preExposureModel.rds")
  coef(model)
  summary(model)
  ### Risk windows: Adding window splits ###

  covarDiclofenacSplit = createCovariateSettings(label = "Exposure of interest",
                                                 includeCovariateIds = diclofenac,
                                                 start = 0,
                                                 end = 0,
                                                 addExposedDaysToEnd = TRUE,
                                                 splitPoints = c(7,14))

  covarPreDiclofenacSplit = createCovariateSettings(label = "Pre-exposure",
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

  ageSettings <- createAgeSettings(includeAge = TRUE,
                                   ageKnots = 5)

  seasonalitySettings <- createSeasonalitySettings(includeSeasonality = TRUE,
                                                   seasonKnots = 5)

  sccsEraData <- createSccsEraData(sccsData,
                                   naivePeriod = 180,
                                   firstOutcomeOnly = FALSE,
                                   covariateSettings = list(covarDiclofenacSplit,
                                                            covarPreDiclofenacSplit),
                                   ageSettings = ageSettings,
                                   seasonalitySettings = seasonalitySettings)

  model <- fitSccsModel(sccsEraData)
  saveRDS(model, "s:/temp/vignetteSccs/ageAndSeasonModel.rds")
  #model <- readRDS("s:/temp/vignetteSccs/ageAndSeasonModel.rds")
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
  model <- fitSccsModel(sccsEraData)
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
  #sccsData <- loadSccsData("s:/temp/vignetteSccs/data2")
  covarPpis = createCovariateSettings(label = "PPIs",
                                      includeCovariateIds = ppis,
                                      stratifyByID = FALSE,
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

  model <- fitSccsModel(sccsEraData)
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
  #sccsData <- loadSccsData("s:/temp/vignetteSccs/data3")

  covarAllDrugs = createCovariateSettings(label = "Other exposures",
                                          excludeCovariateIds = diclofenac,
                                          stratifyByID = TRUE,
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

  control = createControl(cvType = "auto",
                          selectorType = "byPid",
                          startingVariance = 0.1,
                          noiseLevel = "quiet",
                          threads = 10)
  model <- fitSccsModel(sccsEraData, control = control)
  saveRDS(model, "s:/temp/vignetteSccs/allDrugsModel.rds")
  summary(model)
  estimates <- getModel(model)
  estimates[estimates$originalCovariateId == diclofenac,]
  estimates[estimates$originalCovariateId %in% ppis,]
  grep("diclofenac",as.character(model$estimates$covariateName))
}
