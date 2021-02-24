library(SelfControlledCaseSeries)
options(andromedaTempFolder = "s:/andromedaTemp")

rw <- createSimulationRiskWindow(start = 0,
                                 end = 0,
                                 endAnchor = "era end",
                                 relativeRisks = 2)

settings <- createSccsSimulationSettings(eraIds = 1,
                                         simulationRiskWindows = list(rw))

sccsData <- simulateSccsData(10000, settings)

covarSettings <- createEraCovariateSettings(label = "Exposures of interest",
                                            includeEraIds = 1,
                                            stratifyById = FALSE,
                                            start = 0,
                                            end = 0,
                                            endAnchor = "era end")

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = settings$outcomeId,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           ageCovariateSettings = createAgeCovariateSettings(ageKnots = 5),
                                           seasonalityCovariateSettings = createSeasonalityCovariateSettings(seasonKnots = 5))

model <- fitSccsModel(sccsIntervalData)

print(model)
print(sprintf("True IRR = %0.2f", rw$relativeRisks))



