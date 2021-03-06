library("testthat")
# options(fftempdir = 's:/fftemp')

set.seed(123)
sampleSize <- 1000
simulationRiskWindows <- list(createSimulationRiskWindow(relativeRisks = 1),
                              createSimulationRiskWindow(relativeRisks = 1.5))
settings <- createSccsSimulationSettings(simulationRiskWindows = simulationRiskWindows,
                                         eraIds = c(1, 2),
                                         outcomeId = 10)
sccsData <- simulateSccsData(sampleSize, settings)

test_that("Support functions", {
  s <- summary(sccsData)
  expect_equal(class(s), "summary.SccsData")
  expect_equal(s$caseCount, sampleSize)

  covar <- createEraCovariateSettings(includeEraIds = c(1, 2), endAnchor = "era end")
  ageSettings <- createAgeCovariateSettings(allowRegularization = TRUE)
  seasonSettings <- createSeasonalityCovariateSettings(allowRegularization = TRUE)
  studyPop <- createStudyPopulation(sccsData = sccsData,
                                    outcomeId = 10)
  sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                   sccsData = sccsData,
                                   eraCovariateSettings = covar,
                                   ageCovariateSettings = ageSettings,
                                   seasonalityCovariateSettings = seasonSettings)

  s <- summary(sccsIntervalData)
  expect_equal(class(s), "summary.SccsIntervalData")

  model <- fitSccsModel(sccsIntervalData, prior = createPrior("laplace", 0.001))

  expect_equal(class(model), "SccsModel")

  p <- plotAgeEffect(model)
  expect_is(p, "ggplot")

  p <- plotSeasonality(model)
  expect_is(p, "ggplot")
})

test_that("Parameter sweep", {
  coefs <- c()
  ageSettings <- createAgeCovariateSettings()
  seasonSettings <- createSeasonalityCovariateSettings()
  for (stratifyById in c(TRUE, FALSE)) {
    for (naivePeriod in c(0, 180)) {
      for (firstOutcomeOnly in c(TRUE, FALSE)) {
        for (includeAgeAndSeason in c(TRUE, FALSE)) {
          for (eventDependentObservation in c(FALSE)) {
            covar <- createEraCovariateSettings(includeEraIds = c(1, 2),
                                                stratifyById = stratifyById,
                                                endAnchor = "era end")
            studyPop <- createStudyPopulation(sccsData = sccsData,
                                              outcomeId = 10,
                                              naivePeriod = naivePeriod,
                                              firstOutcomeOnly = firstOutcomeOnly)
            sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                             sccsData = sccsData,
                                             eraCovariateSettings = covar,
                                             ageCovariateSettings = if (includeAgeAndSeason) ageSettings else NULL,
                                             seasonalityCovariateSettings = if (includeAgeAndSeason) seasonSettings else NULL,
                                             eventDependentObservation = eventDependentObservation)
            expect_equivalent(class(sccsIntervalData), "SccsIntervalData")
            # Not enough data to fit age and season:
            if (!includeAgeAndSeason) {
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
