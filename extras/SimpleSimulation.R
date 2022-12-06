library(SelfControlledCaseSeries)
options(andromedaTempFolder = "d:/andromedaTemp")

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

preExpCovarSettings <- createEraCovariateSettings(label = "Pre exp",
                                            includeEraIds = 1,
                                            stratifyById = FALSE,
                                            start = -14,
                                            end = -1,
                                            endAnchor = "era start")

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = settings$outcomeId,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           # ageCovariateSettings = createAgeCovariateSettings(ageKnots = 5),
                                           calendarTimeCovariateSettings = createCalendarTimeCovariateSettings(calendarTimeKnots = 5),
                                           seasonalityCovariateSettings = createSeasonalityCovariateSettings(seasonKnots = 5))

model <- fitSccsModel(sccsIntervalData)

print(model)
print(sprintf("True IRR = %0.2f", rw$relativeRisks))

controlIntervalSettings <- createControlIntervalSettings(includeEraIds = 1,
                                                         start = -28,
                                                         startAnchor = "era start",
                                                         end = -1,
                                                         endAnchor = "era start")

sccsIntervalData <- createScriIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = list(covarSettings, preExpCovarSettings),
                                           controlIntervalSettings = controlIntervalSettings)

model <- fitSccsModel(sccsIntervalData)

print(model)
print(sprintf("True IRR = %0.2f", rw$relativeRisks))



