library(SelfControlledCaseSeries)
options(fftempdir = "c:/fftemp")

rw <- createSimulationRiskWindow(start = 0,
                                  end = 0,
                                  addExposedDaysToEnd = TRUE,
                                  relativeRisks = 2)

settings <- createSccsSimulationSettings(covariateIds = c(1,2),
                                         simulationRiskWindows = list(rw, rw))

sccsData <- simulateSccsData(10000, settings)

covarSettings <- createCovariateSettings(label = "Exposures of interest",
                                         includeCovariateIds = c(1,2),
                                         stratifyById = FALSE,
                                         start = 0,
                                         end = 0,
                                         addExposedDaysToEnd = TRUE,
                                         splitPoints = 7)

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 0,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = covarSettings)

model <- fitSccsModel(sccsEraData)

summary(model)
