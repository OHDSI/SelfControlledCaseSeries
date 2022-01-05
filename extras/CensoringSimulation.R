# This script demonstrates why censoring at the event time is a REALLY BAD IDEA

library(SelfControlledCaseSeries)
options(andromedaTempFolder = "s:/andromedaTemp")
setwd("s:/temp")
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

sccsIntervalData <- createSccsIntervalData(sccsData,
                                           naivePeriod = 0,
                                           firstOutcomeOnly = FALSE,
                                           covariateSettings = covarSettings)

summary(sccsIntervalData)
model <- fitSccsModel(sccsIntervalData)
exp(coef(model))

# Censor at event:
hois <- sccsData$eras[sccsData$eras$eraType == "hoi", ]
firstHois <- aggregate(startDay ~ observationPeriodId, data = hois, min)
cases <- merge(sccsData$cases, ff::as.ffdf(firstHois))
cases$observationDays <- cases$startDay
sccsData$cases <- cases
sccsIntervalData <- createSccsIntervalData(sccsData,
                                           naivePeriod = 0,
                                           firstOutcomeOnly = FALSE,
                                           covariateSettings = covarSettings)
summary(sccsIntervalData)

model <- fitSccsModel(sccsIntervalData)
exp(coef(model))


# Testing weighting functions used for adjusting for censoring ------------------------
library(SelfControlledCaseSeries)
setwd("s:/temp")
options(andromedaTempFolder = "s:/andromedaTemp")
rw <- createSimulationRiskWindow(start = 0,
                                 end = 0,
                                 endAnchor = "era end",
                                 relativeRisks = 2)

settings <- createSccsSimulationSettings(eraIds = 1,
                                         simulationRiskWindows = list(rw),
                                         includeAgeEffect = FALSE,
                                         includeCalendarTimeEffect = FALSE,
                                         includeSeasonality = FALSE)
sccsData <- simulateSccsData(10000, settings)


studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = settings$outcomeId,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0)

covarSettings <- createEraCovariateSettings(label = "Exposures of interest",
                                            includeEraIds = 1,
                                            stratifyById = FALSE,
                                            start = 0,
                                            end = 0,
                                            endAnchor = "era end")

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           eventDependentObservation = TRUE)

model <- fitSccsModel(sccsIntervalData)
model
model$metaData$censorModel

max(studyPop$cases$endDay)
aStart <- median(studyPop$cases$ageInDays) / 365.25 + 10
t <- seq(aStart, aStart + 10, by = 1/12)

w <- sapply(t, function(x) 12*SelfControlledCaseSeries:::testEwid(p = model$metaData$censorModel$p,
                                                                  present = 0,
                                                                  astart = aStart,
                                                                  aend = aStart + 10,
                                                                  start = x,
                                                                  end = x + 1/12))

plot(t,w)




folder <- "s:/temp/testSccs"
diclofenac <- 1124300
sccsData <- loadSccsData(file.path(folder, "sccsData.zip"))
studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 1,
                                  firstOutcomeOnly = TRUE,
                                  naivePeriod = 180)
plotEventObservationDependence(studyPop)

covarDiclofenac <- createEraCovariateSettings(label = "Exposure of interest",
                                              includeEraIds = diclofenac,
                                              start = 0,
                                              end = 0,
                                              endAnchor = "era end",
                                              profileLikelihood = FALSE)

calendarTimeSettings <- createCalendarTimeCovariateSettings()
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarDiclofenac,
                                           calendarTimeCovariateSettings = calendarTimeSettings,
                                           eventDependentObservation = T)

model <- fitSccsModel(sccsIntervalData)
plotCalendarTimeEffect(model)
plotEventToCalendarTime(studyPop, model)
model$metaData$censorModel
aStart <- median(studyPop$cases$ageInDays) / 365.25
maxAge <- median(studyPop$cases$endDay) / 365.25
t <- seq(aStart, aStart + maxAge, by = 1/12)
w <- sapply(t, function(x) SelfControlledCaseSeries:::testEwad(p = model$metaData$censorModel$p,
                                                               present = 1,
                                                               astart = aStart,
                                                               aend = aStart + maxAge,
                                                               start = x,
                                                               end = x + 1/12))
plot(t,w)
hist((studyPop$cases$ageInDays + studyPop$cases$endDay) / 365.25)

mean(studyPop$cases$noninformativeEndCensor)


# Some simple simulations -----------------------
library(Cyclops)
library(survival)
library(gnm)

n <- 10000
irr <- 2
backgroundRate <- 0.01
x <- rep(c(1, 0), n)
s <- rep(seq(1:n), each = 2)
e <- runif(n * 2, 1, 100)
lambda <- backgroundRate * ifelse(x == 1, irr, 1) * e
y <- rpois(n*2, lambda)
# w <- rep(runif(n), each = 2)
w <- rep(runif(n), each = 2) + x*0.1

cyclopsData <- createCyclopsData(y ~ x + offset(log(e)) + strata(s), modelType = "cpr")
fit <- fitCyclopsModel(cyclopsData)
exp(coef(fit))

cyclopsData <- createCyclopsData(y ~ x + offset(log(e)) + strata(s), modelType = "cpr")
fit <- fitCyclopsModel(cyclopsData, weights = w)
exp(coef(fit))

cyclopsData <- createCyclopsData(y ~ x + offset(log(e) - log(w)) + strata(s), modelType = "cpr")
fit <- fitCyclopsModel(cyclopsData)
exp(coef(fit))

fit <- gnm(y ~ x + offset(log(e)), family = poisson, eliminate = as.factor(s))
exp(coef(fit))

fit <- gnm(y ~ x + offset(log(e)), family = poisson, eliminate = as.factor(s), weights = w)
exp(coef(fit))

fit <- gnm(y ~ x + offset(log(e) - log(w)), family = poisson, eliminate = as.factor(s))
exp(coef(fit))

# Adding censoring to simulations from package -----------------------------
library(SelfControlledCaseSeries)
options(andromedaTempFolder = "s:/andromedaTemp")
n <- 10000
simulationRiskWindow <- createSimulationRiskWindow(start = 0,
                                                   end = 0,
                                                   endAnchor = "era end",
                                                   relativeRisks = 2)

settings <- createSccsSimulationSettings(eraIds = 1,
                                         simulationRiskWindows = list(simulationRiskWindow),
                                         minBaselineRate = 0.0001,
                                         maxBaselineRate = 0.001,
                                         includeAgeEffect = F,
                                         includeCalendarTimeEffect = TRUE,
                                         includeSeasonality = TRUE)

sccsData <- simulateSccsData(n, settings)

# No censoring, no adjustment for censoring
studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = settings$outcomeId,
                                  firstOutcomeOnly = TRUE,
                                  naivePeriod = 0)
covarSettings <- createEraCovariateSettings(label = "Exposures of interest",
                                            includeEraIds = 1,
                                            stratifyById = FALSE,
                                            start = 0,
                                            end = 0,
                                            endAnchor = "era end")
seasonalitySettings <- createSeasonalityCovariateSettings()
calendarTimeSettings <- createCalendarTimeCovariateSettings()
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           seasonalityCovariateSettings = seasonalitySettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings,
                                           eventDependentObservation = F)
model1 <- fitSccsModel(sccsIntervalData)
model1
plotSeasonality(model1)
plotCalendarTimeEffect(model1)


# No censoring, adjusting for censoring
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           seasonalityCovariateSettings = seasonalitySettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings,
                                           eventDependentObservation = TRUE)
model2 <- fitSccsModel(sccsIntervalData)
model2
plotSeasonality(model2)
plotCalendarTimeEffect(model2)

# Simulate censoring using Weibull distribution
# saveSccsData(sccsData, "s:/temp/sccsData.zip")
# sccsData <- loadSccsData("s:/temp/sccsData.zip")
censoringDays <- rweibull(n, shape = 1, scale = 100)
censoringDays[runif(n) < 0.25] <- 9999999
hist(censoringDays)
eras <- collect(sccsData$eras)
eras <- eras %>%
  arrange(caseId, startDay) %>%
  filter(eraType == "rx" | !duplicated(paste(caseId, eraType)))
sccsData$eras <- eras
sccsData$cases <- sccsData$cases %>%
  inner_join(
    eras %>%
      filter(eraType == "hoi") %>%
      mutate(censorDay = round(startDay + censoringDays)) %>%
      select(caseId, censorDay),
    by = "caseId",
    copy = TRUE) %>%
  mutate(noninformativeEndCensor = ifelse(censorDay < observationDays, 0, 1)) %>%
  mutate(observationDays = ifelse(censorDay < observationDays, censorDay, observationDays)) %>%
  select(-censorDay)

mean(collect(sccsData$cases)$noninformativeEndCensor)

# Censoring, no adjustment for censoring
studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = settings$outcomeId,
                                  naivePeriod = 0)
plotEventObservationDependence(studyPop)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           seasonalityCovariateSettings = seasonalitySettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings,
                                           eventDependentObservation = F)

model3 <- fitSccsModel(sccsIntervalData)
model3
plotSeasonality(model3)
plotCalendarTimeEffect(model3)
plotEventToCalendarTime(studyPop, model3)

# Censoring, adjusting for censoring
sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           seasonalityCovariateSettings = seasonalitySettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings,
                                           eventDependentObservation = TRUE)

model4 <- fitSccsModel(sccsIntervalData)
model4
plotSeasonality(model4)
plotCalendarTimeEffect(model4)
model4$metaData$censorModel
plotEventToCalendarTime(studyPop, model4)

