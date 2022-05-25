library(testthat)
library(SelfControlledCaseSeries)


set.seed(123)
sampleSize <- 1000
simulationRiskWindows <- list(
  createSimulationRiskWindow(relativeRisks = 1),
  createSimulationRiskWindow(relativeRisks = 1.5)
)
settings <- createSccsSimulationSettings(
  simulationRiskWindows = simulationRiskWindows,
  eraIds = c(1, 2),
  outcomeId = 10
)
sccsData <- simulateSccsData(sampleSize, settings)

test_that("Support functions and diagnostics", {
  s <- summary(sccsData)
  expect_equal(class(s), "summary.SccsData")
  expect_equal(s$caseCount, sampleSize)

  covar <- createEraCovariateSettings(includeEraIds = c(1, 2), endAnchor = "era end")
  ageSettings <- createAgeCovariateSettings(allowRegularization = TRUE)
  seasonSettings <- createSeasonalityCovariateSettings(allowRegularization = TRUE)
  calendarTimeSettings <- createCalendarTimeCovariateSettings(allowRegularization = TRUE)
  studyPop <- createStudyPopulation(
    sccsData = sccsData,
    outcomeId = 10
  )
  sccsIntervalData <- createSccsIntervalData(
    studyPopulation = studyPop,
    sccsData = sccsData,
    eraCovariateSettings = covar,
    ageCovariateSettings = ageSettings,
    seasonalityCovariateSettings = seasonSettings,
    calendarTimeCovariateSettings = calendarTimeSettings
  )

  s <- summary(sccsIntervalData)
  expect_equal(class(s), "summary.SccsIntervalData")

  model <- fitSccsModel(sccsIntervalData, prior = createPrior("laplace", 0.001))

  expect_equal(class(model), "SccsModel")

  p <- plotAgeEffect(model)
  expect_is(p, "ggplot")

  p <- plotSeasonality(model)
  expect_is(p, "ggplot")

  p <- plotCalendarTimeEffect(model)
  expect_is(p, "ggplot")

  p <- plotEventToCalendarTime(studyPop, model)
  expect_is(p, "ggplot")

  diagnostic <- computeTimeStability(studyPop, model)
  expect_is(diagnostic, "data.frame")

  mdrr <- computeMdrr(sccsIntervalData = sccsIntervalData, exposureCovariateId = 1000)
  expect_lt(mdrr$mdrr, Inf)

  mdrr <- computeMdrr(sccsIntervalData = sccsIntervalData, exposureCovariateId = 9999)
  expect_equal(mdrr$mdrr, Inf)
})

test_that("Parameter sweep", {
  coefs <- c()
  ageSettings <- createAgeCovariateSettings()
  seasonSettings <- createSeasonalityCovariateSettings()
  calendarTimeSettings <- createCalendarTimeCovariateSettings()
  for (stratifyById in c(TRUE, FALSE)) {
    for (naivePeriod in c(0, 180)) {
      for (firstOutcomeOnly in c(TRUE, FALSE)) {
        for (includeAgeSeasonAndCalendarTime in c(TRUE, FALSE)) {
          for (eventDependentObservation in c(FALSE)) {
            covar <- createEraCovariateSettings(
              includeEraIds = c(1, 2),
              stratifyById = stratifyById,
              endAnchor = "era end"
            )
            studyPop <- createStudyPopulation(
              sccsData = sccsData,
              outcomeId = 10,
              naivePeriod = naivePeriod,
              firstOutcomeOnly = firstOutcomeOnly
            )
            sccsIntervalData <- createSccsIntervalData(
              studyPopulation = studyPop,
              sccsData = sccsData,
              eraCovariateSettings = covar,
              ageCovariateSettings = if (includeAgeSeasonAndCalendarTime) ageSettings else NULL,
              seasonalityCovariateSettings = if (includeAgeSeasonAndCalendarTime) seasonSettings else NULL,
              calendarTimeCovariateSettings = if (includeAgeSeasonAndCalendarTime) calendarTimeSettings else NULL,
              eventDependentObservation = eventDependentObservation
            )
            expect_equivalent(class(sccsIntervalData), "SccsIntervalData")
            # Not enough data to fit age and season:
            if (!includeAgeSeasonAndCalendarTime) {
              model <- fitSccsModel(sccsIntervalData)
              coefs <- c(coefs, coef(model)[1])
            }
          }
        }
      }
    }
  }
  # Each analysis should produce a unique estimate:
  expect_equal(length(coefs), length(unique(coefs)))
})

test_that("Plots", {
  studyPop <- createStudyPopulation(
    sccsData = sccsData,
    outcomeId = 10,
    naivePeriod = 0,
    firstOutcomeOnly = TRUE
  )

  plot <- plotAgeSpans(studyPopulation = studyPop)
  expect_s3_class(plot, "ggplot")

  plot <- plotCalendarTimeSpans(studyPopulation = studyPop)
  expect_s3_class(plot, "ggplot")

  plot <- plotEventToCalendarTime(studyPopulation = studyPop)
  expect_s3_class(plot, "ggplot")

  plot <- plotEventObservationDependence(studyPopulation = studyPop)
  expect_s3_class(plot, "ggplot")

  plot <- plotExposureCentered(
    studyPopulation = studyPop,
    sccsData = sccsData,
    exposureEraId = 1
  )
  expect_s3_class(plot, "ggplot")

  expect_warning(plotExposureCentered(
    studyPopulation = studyPop,
    sccsData = sccsData,
    exposureEraId = 999
  ))
})
