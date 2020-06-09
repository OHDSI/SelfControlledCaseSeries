library(testthat)
library(dplyr)

convertToSccsDataWrapper <- function(cases,
                                     eras,
                                     exposureId = NULL,
                                     covariateSettings = NULL,
                                     ageSettings = NULL,
                                     seasonalitySettings = NULL,
                                     naivePeriod = 0,
                                     firstOutcomeOnly = FALSE,
                                     excludeeraIds = NULL,
                                     minAge = NULL,
                                     maxAge = NULL) {
  if (is.null(covariateSettings)) {
    covariateSettings <- createEraCovariateSettings(includeCovariateIds = exposureId,
                                                    start = 0,
                                                    end = 0,
                                                    addExposedDaysToEnd = TRUE)
  }

  covariateIds <- c()
  if (is.list(covariateSettings) && class(covariateSettings) != "EraCovariateSettings") {
    for (i in 1:length(covariateSettings)) {
      covariateIds <- c(covariateIds, covariateSettings[[i]]$includeCovariateIds)
    }
  } else {
    covariateIds <- covariateSettings$includeCovariateIds
  }
  eraRef <- eras %>%
    distinct(.data$eraId, .data$eraType) %>%
    mutate(eraName = "")

  data <- Andromeda::andromeda(cases = cases,
                               eras = eras,
                               eraRef = eraRef)
  attr(data, "metaData") <- list(outcomeIds = 10,
                                 attrition = tibble(outcomeId = 10))

  studyPop <- createStudyPopulation(sccsData = data,
                                    outcomeId = 10,
                                    firstOutcomeOnly = firstOutcomeOnly,
                                    naivePeriod = naivePeriod,
                                    minAge = minAge,
                                    maxAge = maxAge)

  result <- createSccsEraData(studyPopulation = studyPop,
                              sccsData = data,
                              ageCovariateSettings = ageSettings,
                              seasonalityCovariateSettings = seasonalitySettings,
                              eraCovariateSettings = covariateSettings)
  return(list(outcomes = collect(result$outcomes), covariates = collect(result$covariates)))
}

test_that("Simple era construction", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 75))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = 11)
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(49, 51))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Age restriction", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 10000,
                  ageInDays = 365,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(500, 525),
                 endDay = c(500, 575))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = 11, minAge = 2, maxAge = 3)
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(680, 51))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Outcome on boundary", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(25, 25),
                 endDay = c(25, 75))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(49, 51))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Outcome on boundary", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(24, 25),
                 endDay = c(24, 75))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(49, 51))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Outcome on boundary", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(75, 25),
                 endDay = c(75, 75))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(49, 51))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Outcome on boundary", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(76, 25),
                 endDay = c(76, 75))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(49, 51))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("One day era", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 25))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(99, 1))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Merging overlapping eras", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 11, 11),
                 value = c(1, 1, 1),
                 startDay = c(50, 25, 70),
                 endDay = c(50, 75, 80))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(44, 56))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Merging overlapping eras with same start date", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 11, 11),
                 value = c(1, 1, 1),
                 startDay = c(50, 25, 25),
                 endDay = c(50, 75, 50))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(49, 51))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})


test_that("Concomitant drug use", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 11, 12),
                 value = c(1, 1, 1),
                 startDay = c(50, 25, 60),
                 endDay = c(50, 75, 70))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11, 12))
  expect_equal(result$outcomes$rowId, c(0, 1, 2))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1))
  expect_equal(result$outcomes$time, c(49, 40, 11))
  expect_equal(result$outcomes$y, c(0, 1, 0))
  expect_equal(result$covariates$rowId, c(1, 2, 2))
  expect_equal(result$covariates$stratumId, c(1, 1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1000, 1001))
})

test_that("Concomitant drug use (3 drugs)", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hoi", "hei", "hei", "hei"),
                 observationPeriodId = c(1, 1, 1, 1, 1),
                 eraId = c(10, 10, 11, 12, 13),
                 value = c(1, 1, 1, 1, 1),
                 startDay = c(50, 85, 25, 70, 70),
                 endDay = c(NA, NA, 75, 80, 77))
  result <- convertToSccsDataWrapper(cases, eras, exposureId = c(11, 12, 13))
  expect_equal(result$outcomes$rowId, c(0, 1, 2, 3, 4))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1, 1, 1))
  expect_equal(result$outcomes$time, c(44, 45, 3, 2, 6))
  expect_equal(result$outcomes$y, c(1, 1, 0, 0, 0))
  expect_equal(result$covariates$rowId, c(1, 2, 3, 3, 4, 4, 4))
  expect_equal(result$covariates$stratumId, c(1, 1, 1, 1, 1, 1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1001, 1001, 1002, 1000, 1001, 1002))
})

test_that("Start risk window at day 1 not 0", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 50),
                 endDay = c(50, 75))
  result <- convertToSccsDataWrapper(cases,
                                     eras,
                                     covariateSettings = createEraCovariateSettings(includeCovariateIds = c(11, 12, 13),
                                                                                    start = 1,
                                                                                    end = 0,
                                                                                    addExposedDaysToEnd = TRUE))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(75, 25))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Two HOIs, keeping both", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hoi", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 10, 11),
                 value = c(1, 1, 1),
                 startDay = c(25, 50, 30),
                 endDay = c(25, 50, 60))
  result <- convertToSccsDataWrapper(cases, eras, firstOutcomeOnly = FALSE, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(69, 31))
  expect_equal(result$outcomes$y, c(1, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Two HOIs, keeping first", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hoi", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 10, 11),
                 value = c(1, 1, 1),
                 startDay = c(25, 50, 30),
                 endDay = c(25, 50, 60))
  result <- convertToSccsDataWrapper(cases, eras, firstOutcomeOnly = TRUE, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(69, 31))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Removal of risk windows where end before start", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 50),
                 endDay = c(50, 75))
  result <- convertToSccsDataWrapper(cases,
                                     eras,
                                     covariateSettings = createEraCovariateSettings(includeCovariateIds = c(11),
                                                                                    start = 0,
                                                                                    end = 7,
                                                                                    addExposedDaysToStart = TRUE))
  expect_equal(result$outcomes %>% count() %>% pull(), 0)
})

test_that("Aggregates on large set", {
  settings <- createSccsSimulationSettings(includeAgeEffect = FALSE, includeSeasonality = FALSE)
  sccsData <- simulateSccsData(1000, settings)
  sccsEraData <- createSccsEraData(sccsData,
                                   naivePeriod = 0,
                                   firstOutcomeOnly = FALSE,
                                   covariateSettings = createCovariateSettings(includeCovariateIds = c(1, 2),
                                                                               addExposedDaysToEnd = TRUE))
  x <- ff::as.ram(ffbase::subset.ffdf(sccsData$eras, eraId == 1))
  y <- ff::as.ram(ffbase::subset.ffdf(sccsData$eras, eraId == 10))
  z <- merge(x, y, by = c("observationPeriodId"))
  z <- subset(z, startDay.y >= startDay.x & startDay.y <= endDay.x)
  z <- unique(z$observationPeriodId)

  x <- ff::as.ram(ffbase::subset.ffdf(sccsEraData$covariates, covariateId == 1000))
  y <- ff::as.ram(ffbase::subset.ffdf(sccsEraData$outcomes, y != 0))
  z2 <- merge(x, y, by = c("rowId"))
  z2 <- unique(z2$stratumId.x)

  # eras <- ff::as.ram(sccsData$eras) eras[eras$observationPeriodId == 359,] cases <-
  # ff::as.ram(sccsData$cases) cases[cases$observationPeriodId == 359,] z[z$observationPeriodId ==
  # 359,] z2[z2$stratumId.x == 359,]

  # Same people have the event during exposure to 1:
  expect_equal(z, z2)

  x <- ff::as.ram(ffbase::subset.ffdf(sccsData$eras, eraId == 2))
  y <- ff::as.ram(ffbase::subset.ffdf(sccsData$eras, eraId == 10))
  z <- merge(x, y, by = c("observationPeriodId"))
  z <- subset(z, startDay.y >= startDay.x & startDay.y <= endDay.x)
  z <- unique(z$observationPeriodId)

  x <- ff::as.ram(ffbase::subset.ffdf(sccsEraData$covariates, covariateId == 1001))
  y <- ff::as.ram(ffbase::subset.ffdf(sccsEraData$outcomes, y != 0))
  z2 <- merge(x, y, by = c("rowId"))
  z2 <- unique(z2$stratumId.x)

  # Same people have the event during exposure to 2:
  expect_equal(z, z2)
})

test_that("Exposure splitting", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 75),
                 stringsAsFactors = TRUE)
  result <- convertToSccsDataWrapper(cases,
                                     eras,
                                     covariateSettings = createCovariateSettings(includeCovariateIds = 11,
                                                                                 start = 0,
                                                                                 end = 0,
                                                                                 addExposedDaysToEnd = TRUE,
                                                                                 splitPoints = c(7)))
  expect_equal(result$outcomes$rowId, c(0, 1, 2))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1))
  expect_equal(result$outcomes$time, c(49, 8, 43))
  expect_equal(result$outcomes$y, c(0, 0, 1))
  expect_equal(result$covariates$rowId, c(1, 2))
  expect_equal(result$covariates$stratumId, c(1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1001))
})

test_that("Exposure splitting twice", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 75),
                 stringsAsFactors = TRUE)
  result <- convertToSccsDataWrapper(cases,
                                     eras,
                                     covariateSettings = createCovariateSettings(includeCovariateIds = 11,
                                                                                 start = 0,
                                                                                 end = 0,
                                                                                 addExposedDaysToEnd = TRUE,
                                                                                 splitPoints = c(7,
                                                                                                 15)))
  expect_equal(result$outcomes$rowId, c(0, 1, 2, 3))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1, 1))
  expect_equal(result$outcomes$time, c(49, 8, 8, 35))
  expect_equal(result$outcomes$y, c(0, 0, 0, 1))
  expect_equal(result$covariates$rowId, c(1, 2, 3))
  expect_equal(result$covariates$stratumId, c(1, 1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1001, 1002))
})

test_that("Merging exposures (stratifyById=FALSE)", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0)
  eras <- tibble(eraType = c("hoi", "hei", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 11, 12),
                 value = c(1, 1, 1),
                 startDay = c(50, 25, 70),
                 endDay = c(50, 75, 100),
                 stringsAsFactors = TRUE)
  result <- convertToSccsDataWrapper(cases,
                                     eras,
                                     covariateSettings = createCovariateSettings(includeCovariateIds = c(11,12),
                                                                                 stratifyById = FALSE,
                                                                                 start = 0,
                                                                                 end = 0,
                                                                                 addExposedDaysToEnd = TRUE))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(25, 75))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})


test_that("Exposure splitting without stratifyById", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0)
  eras <- tibble(eraType = c("hoi", "hei", "hei"),
                 observationPeriodId = c(1, 1, 1),
                 eraId = c(10, 11, 12),
                 value = c(1, 1, 1),
                 startDay = c(50, 25, 70),
                 endDay = c(50, 75, 100),
                 stringsAsFactors = TRUE)
  result <- convertToSccsDataWrapper(cases,
                                     eras,
                                     covariateSettings = createCovariateSettings(includeCovariateIds = c(11,12),
                                                                                 stratifyById = FALSE,
                                                                                 start = 0,
                                                                                 end = 0,
                                                                                 addExposedDaysToEnd = TRUE,
                                                                                 splitPoints = c(50)))
  expect_equal(result$outcomes$rowId, c(0, 1, 2))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1))
  expect_equal(result$outcomes$time, c(25,51, 24))
  expect_equal(result$outcomes$y, c(0, 1, 0))
  expect_equal(result$covariates$rowId, c(1, 2))
  expect_equal(result$covariates$stratumId, c(1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1001))
})

test_that("Pre-exposure window", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 75),
                 stringsAsFactors = TRUE)
  result <- convertToSccsDataWrapper(cases, eras, covariateSettings = list(createCovariateSettings(includeCovariateIds = 11,
                                                                                                   start = 0,
                                                                                                   end = 0,
                                                                                                   addExposedDaysToEnd = TRUE),
                                                                           createCovariateSettings(includeCovariateIds = 11,
                                                                                                   start = -30,
                                                                                                   end = -1)))
  expect_equal(result$outcomes$rowId, c(0, 1, 2))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1))
  expect_equal(result$outcomes$time, c(24, 51, 25))
  expect_equal(result$outcomes$y, c(0, 1, 0))
  expect_equal(result$covariates$rowId, c(1, 2))
  expect_equal(result$covariates$stratumId, c(1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1001))
})

test_that("Simple era construction", {
  cases <- tibble(observationPeriodId = 1,
                  personId = 1,
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0)
  eras <- tibble(eraType = c("hoi", "hei"),
                 observationPeriodId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 75),
                 stringsAsFactors = TRUE)
  ffResult <- convertToSccsDataWrapper(cases, eras, exposureId = 11, placeInRam = FALSE)
  expect_equal(class(ffResult$outcomes), "ffdf")
  expect_equal(class(ffResult$covariates), "ffdf")
  ramResult <- forceSccsEraDataIntoRam(ffResult)
  expect_equal(class(ramResult$outcomes), "tibble")
  expect_equal(class(ramResult$covariates), "tibble")
})

# test_that('Age', { cases <- tibble(observationPeriodId = 1, personId = 1, observationDays =
# 100, ageInDays = 0, startYear = 2000, startMonth = 5, startDay =
# 1) eras <- tibble(eraType = c('hoi', 'hoi', 'hei'), observationPeriodId = c(1, 1, 1), eraId
# = c(10, 10, 11), value = c(1,1,1), startDay = c(25, 50,30), endDay = c(25, 50, 60)) result <-
# convertToSccsDataWrapper(cases, eras, exposureId = c(11), ageEffectSettings =
# createAgeEffectSettings(includeAge = TRUE)) expect_equal(nrow(result$covariates), 22) })

# f <- function() { writeLines(paste('expect_equal(result$outcomes$rowId, c(',
# paste(result$outcomes$rowId, collapse = ','), '))', sep = ''))
# writeLines(paste('expect_equal(result$outcomes$stratumId, c(', paste(result$outcomes$stratumId,
# collapse = ','), '))', sep = '')) writeLines(paste('expect_equal(result$outcomes$time, c(',
# paste(result$outcomes$time, collapse = ','), '))', sep = ''))
# writeLines(paste('expect_equal(result$outcomes$y, c(', paste(result$outcomes$y, collapse = ','),
# '))', sep = '')) writeLines(paste('expect_equal(result$covariates$rowId, c(',
# paste(result$covariates$rowId, collapse = ','), '))', sep = ''))
# writeLines(paste('expect_equal(result$covariates$stratumId, c(', paste(result$covariates$stratumId,
# collapse = ','), '))', sep = '')) writeLines(paste('expect_equal(result$covariates$covariateId,
# c(', paste(result$covariates$covariateId, collapse = ','), '))', sep = '')) }
