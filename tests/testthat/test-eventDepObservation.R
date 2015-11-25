library("testthat")
#   options(fftempdir = "s:/fftemp")
# Simulation:
set.seed(123)
n <- 200000
observationDays <- 100
rr <- 2
minBaselineRate <- 0.00005
maxBaselineRate <- 0.0001

data <- data.frame(personId = 1:n)
data$observationStartDate = 1
data$observationEndDate = observationDays
data$exposureStartDate <- round(runif(n, 1, observationDays))
data$exposureEndDate <- round(runif(n, data$exposureStartDate, observationDays))
data$daysExposed <- data$exposureEndDate - data$exposureStartDate + 1
data$daysUnexposed <- observationDays - data$daysExposed
data$baselineRisk <- runif(n, minBaselineRate, maxBaselineRate)
data$eventsUnexposed <- rpois(n, data$baselineRisk * data$daysUnexposed)
data$eventsExposed <- rpois(n, rr * data$baselineRisk * data$daysExposed)
#data$ageInDays <- 1 # Age on the first day. Can be zero
data$ageInDays <- round(runif(n, 1,100))
data$eventDate <- observationDays + 1

peopleUnexpEvent <- data$eventsUnexposed > 0
# Date of event is random day when unexposed:
data$eventDate[peopleUnexpEvent] <- round(runif(sum(peopleUnexpEvent), 1, data$daysUnexposed[peopleUnexpEvent]))
# If day greater than exposure start day, at exposure time so it falls in period post exposure:
data$eventDate[peopleUnexpEvent & data$eventDate > data$exposureStartDate] <- data$eventDate[peopleUnexpEvent & data$eventDate > data$exposureStartDate]  + data$daysExposed[peopleUnexpEvent & data$eventDate > data$exposureStartDate]

# For people with event during exposure, and no event in the period prior exposure, randomly pick an event date during exposure:
peopleExpEvent <- data$eventsExposed > 0 & data$eventDate > data$exposureStartDate
data$eventDate[peopleExpEvent] <- round(runif(sum(peopleExpEvent), data$exposureStartDate[peopleExpEvent], data$exposureEndDate[peopleExpEvent]))

# Remove non-cases:
data <- data[data$eventDate <= observationDays,]

### Event-dependent observation period ###
preEventCensorRate <- 0.001
postEventCensorRate <- 0.05

# Pre event censoring
data$censorDate <- round(rexp(nrow(data), preEventCensorRate))

# Post event censoring
notCensored <- data$censorDate > data$eventDate
data$censorDate[notCensored] <- data$eventDate[notCensored] + round(rexp(sum(notCensored), postEventCensorRate))

# Remove patients where outcome or exposure falls after censor date:
data <- data[data$exposureStartDate <= data$censorDate,]
data <- data[data$eventDate <= data$censorDate,]

#Truncate exposure end date at censor date:
data$exposureEndDate[data$exposureEndDate > data$censorDate] <- data$censorDate[data$exposureEndDate > data$censorDate]

data$censorDate[data$censorDate > observationDays] <- observationDays
nrow(data)

### Use SCCS library. Note differences in convention:
# - end dates are NOT included to compute length of a period, so +1 for end dates
# - events are considered part of a period if period start data < event date <= period end date, so +1 for even dates


# Note: SCCS package currently not in CRAN.
# Also note: I had to modify SCCS, remove adding 0.5 days when event date is end date
# library(SCCS)
# x <- eventdepenobs(formula = event ~ exposure + strata(indivL) + offset(logw),
#                    adrug = data$ageInDays + data$exposureStartDate - data$observationStartDate,
#                    aedrug = data$ageInDays + data$exposureEndDate - data$observationStartDate + 1,
#                    data = data.frame(Indiv = data$personId,
#                                      aevent = data$ageInDays + data$eventDate - data$observationStartDate + 1,
#                                      astart = data$ageInDays,
#                                      aend = data$ageInDays + data$censorDate - data$observationStartDate + 1,
#                                      aestudy = data$ageInDays + observationDays))

x <- list(summary = list())
x$summary$coefficients <- c(0.762933)
x$modelfit <- matrix(c(-3122.776,-3122.776,-3122.776,-3122.776), nrow = 2)

test_that("Produces same results as SCCS package when using event-dependent observation periods", {
  cases <- data.frame(observationPeriodId = data$personId,
                      personId = data$personId,
                      observationDays = data$censorDate - data$observationStartDate + 1,
                      ageInDays = data$ageInDays,
                      observationStartYear = 2000,
                      observationStartMonth = 5,
                      observationStartDay = 1)
  heiEras <- data.frame(eraType = "hei",
                        observationPeriodId = data$personId,
                        conceptId = 1,
                        value = 1,
                        startDay = data$exposureStartDate - data$observationStartDate,
                        endDay = data$exposureEndDate - data$observationStartDate)
  hoiEras <- data.frame(eraType = "hoi",
                        observationPeriodId = data$personId,
                        conceptId = 2,
                        value = 1,
                        startDay = data$eventDate - data$observationStartDate,
                        endDay = data$eventDate - data$observationStartDate)
  eras <- rbind(heiEras, hoiEras)
  eras <- eras[order(eras$observationPeriodId),]
  sccsData <- list(cases = ff::as.ffdf(cases),
                   eras = ff::as.ffdf(eras),
                   metaData = list(outcomeIds = 2),
                   covariateRef = ff::as.ffdf(data.frame(covariateId = c(1), covariateName = c("")))
  )
  sccsEraData <- createSccsEraData(sccsData = sccsData,
                                   covariateSettings = createCovariateSettings(includeCovariateIds = 1,
                                                                               start = 0,
                                                                               end = 0,
                                                                               addExposedDaysToEnd = TRUE),
                                   naivePeriod = 0,
                                   eventDependentObservation = TRUE)

  expect_equal(sccsEraData$metaData$censorModel$aic, min(x$modelfit[2,]), tolerance = 1E-4)

  fit <- fitSccsModel(sccsEraData)

  expect_equal(x$summary$coefficients[1], as.vector(coef(fit)), tolerance = 1E-4)
})

# exp(x$summary$coefficients[1])
# exp(coef(fit))
#
# myData <- merge(ff::as.ram(sccsEraData$outcomes), ff::as.ram(sccsEraData$covariates), all.x = TRUE)
# myData$covariateValue[is.na(myData$covariateValue)] <- 0
# myData <- data.frame(stratumId = myData$stratumId, exposure = myData$covariateValue,
#                      w = myData$time, event = myData$y)
# clogit(event ~ exposure + strata(stratumId) + offset(log(w)), data = myData)
#
# chopdat <- readRDS("s:/temp/chopdat.rds")
# chopdat$w <- exp(chopdat$logw)*365.25
# chopdat$stratumId <- as.numeric(as.character(chopdat$Indiv))
# cd1 <- aggregate(w ~ stratumId + exposure, data = chopdat, sum)
# cd2 <- aggregate(event ~ stratumId + exposure, data = chopdat, sum)
# cd <- merge(cd1,cd2)
# cd <- cd[order(cd$stratumId),]
# rownames(cd) <- NULL
# clogit(event ~ exposure + strata(stratumId) + offset(log(w)), data = cd)
#
# head(cd)
# head(myData)
# all.equal(cd,myData)
# which(myData$w > 10000)
# myData[myData$w > 100000,]
# (cd$w[1] - myData$w[1])
# sccsData = data.frame(Indiv = as.factor(data$personId),
#                       aevent = data$ageInDays + data$eventDate - data$observationStartDate + 1,
#                       astart = data$ageInDays,
#                       aend = data$ageInDays + data$censorDate - data$observationStartDate + 1,
#                       aestudy = data$ageInDays + observationDays)
# sccsData$present <- ifelse(sccsData$aend>=sccsData$aestudy, 1, 0)
# chopdat <- formatdata(data$ageInDays + data$exposureStartDate - data$observationStartDate,
#                       data$ageInDays + data$exposureEndDate - data$observationStartDate + 1,
#                       NULL,
#                       0,
#                       0,
#                       TRUE,
#                       data = sccsData) # Expanded data based on the age and exposure groups
# exposedCases1 <- as.numeric(levels(sccsData$Indiv)[chopdat$indiv[chopdat$event == 1 & chopdat$exposure == 1]])
# exposedCases2 <- myData$stratumId[myData$y == 1 & myData$covariateValue == 1]
# all.equal(exposedCases1, exposedCases2)
#
#
#
# sccsData = data.frame(Indiv = c(1,2,3),
#                       aevent = c(2,3,4),
#                       astart = c(1,1,1),
#                       aend = c(10,10,10),
#                       aestudy = c(10,10,10))
# chopdat <- formatdata(adrug = c(2,2,2),
#                       aedrug = c(4,4,4),
#                       NULL,
#                       0,
#                       0,
#                       TRUE,
#                       data = sccsData)
# chopdat
#
# data[data$personId == 99141,]
# chopdat[chopdat$indivL == which(levels(sccsData$Indiv) == 99141),]
#
# sccsEraData <- createSccsEraData(sccsData = sccsData,
#                                  exposureId = 1,
#                                  exposureOfInterestSettings = createCovariateSettings(start = 0,
#                                                                                       end = 0,
#                                                                                       addExposedDaysToEnd = TRUE),
#                                  naivePeriod = 0,
#                                  firstOutcomeOnly = TRUE,
#                                  includeAgeEffect = FALSE,
#                                  includePreExposureOfInterest = FALSE,
#                                  eventDependentObservation = FALSE)
#
# fit <- fitSccsModel(sccsEraData, exposureId = 1, prior = createPrior("none"))
# exp(coef(fit))
# # })
