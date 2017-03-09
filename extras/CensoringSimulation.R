# This script demonstrates why censoring at the event time is a REALLY BAD IDEA

library(SelfControlledCaseSeries)
setwd("s:/temp")
options(fftempdir = "s:/fftemp")
settings <- createSccsSimulationSettings(covariateIds = 1,
                                         patientUsages = 0.2,
                                         usageRate = 0.01,
                                         meanPrescriptionDurations = 14,
                                         sdPrescriptionDurations = 7,
                                         simulationRiskWindows = list(createSimulationRiskWindow(relativeRisks = 2)),
                                         includeAgeEffect = FALSE,
                                         includeSeasonality = FALSE,
                                         outcomeId = 10)

sccsData <- simulateSccsData(1000, settings)

covarSettings <- createCovariateSettings(label = "Exposure of interest",
                                         includeCovariateIds = 1,
                                         start = 0,
                                         end = 0,
                                         addExposedDaysToEnd = TRUE)

sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 0,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = covarSettings)

summary(sccsEraData)
model <- fitSccsModel(sccsEraData)
exp(coef(model))

# Censor at event:
hois <- sccsData$eras[sccsData$eras$eraType == "hoi", ]
firstHois <- aggregate(startDay ~ observationPeriodId, data = hois, min)
cases <- merge(sccsData$cases, ff::as.ffdf(firstHois))
cases$observationDays <- cases$startDay
sccsData$cases <- cases
sccsEraData <- createSccsEraData(sccsData,
                                 naivePeriod = 0,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = covarSettings)
summary(sccsEraData)

model <- fitSccsModel(sccsEraData)
exp(coef(model))

