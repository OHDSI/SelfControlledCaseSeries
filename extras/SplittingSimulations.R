library(SelfControlledCaseSeries)
options(andromedaTempFolder = "s:/andromedaTemp")

rw <- createSimulationRiskWindow(start = 0,
                                 end = 0,
                                 endAnchor = "era end",
                                 relativeRisks = 2)

settings <- createSccsSimulationSettings(eraIds = c(1, 2),
                                         simulationRiskWindows = list(rw, rw))

sccsData <- simulateSccsData(10000, settings)

covarSettings <- createEraCovariateSettings(label = "Exposures of interest",
                                            includeEraIds = c(1,2),
                                            stratifyById = FALSE,
                                            start = 0,
                                            end = 0,
                                            endAnchor = "era end",
                                            splitPoints = 7)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = settings$outcomeId,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings)

model <- fitSccsModel(sccsIntervalData)

print(model)
print(sprintf("True IRR = %0.2f", rw$relativeRisks))
