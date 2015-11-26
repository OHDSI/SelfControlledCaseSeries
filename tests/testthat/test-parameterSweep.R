library("testthat")
# options(fftempdir = 's:/fftemp')

set.seed(123)
sampleSize <- 1000
simulationRiskWindows <- list(createSimulationRiskWindow(relativeRisks = 1),
                              createSimulationRiskWindow(relativeRisks = 1.5))
settings <- createSccsSimulationSettings(simulationRiskWindows = simulationRiskWindows,
                                         covariateIds = c(1, 2),
                                         outcomeId = 10)
sccsData <- simulateSccsData(sampleSize, settings)

test_that("Support functions", {
  s <- summary(sccsData)
  expect_equal(class(s), "summary.sccsData")
  expect_equal(s$caseCount, sampleSize)

  covar <- createCovariateSettings(includeCovariateIds = c(1, 2), addExposedDaysToEnd = TRUE)
  ageSettings <- createAgeSettings(includeAge = TRUE, allowRegularization = TRUE)
  seasonSettings <- createSeasonalitySettings(includeSeasonality = TRUE, allowRegularization = TRUE)
  sccsEraData <- createSccsEraData(sccsData = sccsData,
                                   outcomeId = 10,
                                   covariateSettings = covar,
                                   ageSettings = ageSettings,
                                   seasonalitySettings = seasonSettings)

  s <- summary(sccsEraData)
  expect_equal(class(s), "summary.sccsEraData")
  expect_equal(s$outcomeCounts$caseCount[1], sampleSize)

  model <- fitSccsModel(sccsEraData, prior = createPrior("laplace", 0.001))
  s <- summary(model)
  expect_equal(class(s), "summary.sccsModel")

  p <- plotAgeEffect(model)
  expect_is(p, "ggplot")

  p <- plotSeasonality(model)
  expect_is(p, "ggplot")
})

test_that("Parameter sweep", {
  coefs <- c()
  for (stratifyById in c(TRUE, FALSE)) {
    for (naivePeriod in c(0, 180)) {
      for (firstOutcomeOnly in c(TRUE, FALSE)) {
        for (includeAgeAndSeason in c(TRUE, FALSE)) {
          for (eventDependentObservation in c(FALSE)) {
          covar <- createCovariateSettings(includeCovariateIds = c(1, 2),
                                           stratifyById = stratifyById,
                                           addExposedDaysToEnd = TRUE)
          ageSettings <- createAgeSettings(includeAge = includeAgeAndSeason)
          seasonSettings <- createSeasonalitySettings(includeSeasonality = includeAgeAndSeason)
          sccsEraData <- createSccsEraData(sccsData = sccsData,
                                           outcomeId = 10,
                                           naivePeriod = naivePeriod,
                                           firstOutcomeOnly = firstOutcomeOnly,
                                           covariateSettings = covar,
                                           ageSettings = ageSettings,
                                           seasonalitySettings = seasonSettings,
                                           eventDependentObservation = eventDependentObservation)
          expect_equal(class(sccsEraData), "sccsEraData")
          # Not enough data to fit age and season:
          if (!includeAgeAndSeason) {
            model <- fitSccsModel(sccsEraData)
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
