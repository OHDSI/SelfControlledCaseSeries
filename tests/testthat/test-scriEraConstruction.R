library(testthat)
library(dplyr)

convertToScriDataWrapper <- function(cases,
                                     eras,
                                     exposureId = NULL,
                                     covariateSettings = NULL,
                                     controlIntervalSettings = NULL,
                                     naivePeriod = 0,
                                     firstOutcomeOnly = FALSE,
                                     excludeeraIds = NULL,
                                     minAge = NULL,
                                     maxAge = NULL) {
  if (is.null(covariateSettings)) {
    covariateSettings <- createEraCovariateSettings(includeEraIds = exposureId,
                                                    start = 0,
                                                    end = 0,
                                                    endAnchor = "era end")
  }

  if (is.null(controlIntervalSettings)) {
    controlIntervalSettings <- createControlIntervalSettings(includeEraIds = exposureId,
                                                             start = -14,
                                                             end = -7,
                                                             endAnchor = "era start")
  }

  covariateIds <- c()
  if (is.list(covariateSettings) && class(covariateSettings) != "EraCovariateSettings") {
    for (i in 1:length(covariateSettings)) {
      covariateIds <- c(covariateIds, covariateSettings[[i]]$includeEraIds)
    }
  } else {
    covariateIds <- covariateSettings$includeEraIds
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

  result <- createScriIntervalData(studyPopulation = studyPop,
                                   sccsData = data,
                                   eraCovariateSettings = covariateSettings,
                                   controlIntervalSettings = controlIntervalSettings)
  return(list(outcomes = collect(result$outcomes), covariates = collect(result$covariates)))
}

test_that("Simple SCRI era construction", {
  cases <- tibble(observationPeriodId = "1000",
                  caseId = 1,
                  personId = "1",
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "rx"),
                 caseId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(50, 25),
                 endDay = c(50, 75))
  result <- convertToScriDataWrapper(cases, eras, exposureId = 11)
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(8, 51))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})


test_that("Outcome on boundary or control interval", {
  cases <- tibble(observationPeriodId = "1000",
                  caseId = 1,
                  personId = "1",
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "rx"),
                 caseId = c(1, 1),
                 eraId = c(10, 11),
                 value = c(1, 1),
                 startDay = c(25-7, 25),
                 endDay = c(25-7, 75))
  result <- convertToScriDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(8, 51))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Merging overlapping control intervals", {
  cases <- tibble(observationPeriodId = "1000",
                  caseId = 1,
                  personId = "1",
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "rx", "rx"),
                 caseId = c(1, 1, 1),
                 eraId = c(10, 11, 11),
                 value = c(1, 1, 1),
                 startDay = c(25-7, 25, 28),
                 endDay = c(25-7, 26, 29))
  result <- convertToScriDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(11, 4))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Control intervals overlapping with a risk window", {
  cases <- tibble(observationPeriodId = "1000",
                  caseId = 1,
                  personId = "1",
                  observationDays = 100,
                  ageInDays = 0,
                  startYear = 2000,
                  startMonth = 5,
                  startDay = 1,
                  censoredDays = 0,
                  noninformativeEndCensor = 0)
  eras <- tibble(eraType = c("hoi", "rx", "rx"),
                 caseId = c(1, 1, 1),
                 eraId = c(10, 11, 11),
                 value = c(1, 1, 1),
                 startDay = c(32, 30, 50),
                 endDay = c(32, 40, 60))
  result <- convertToScriDataWrapper(cases, eras, exposureId = c(11))
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(11, 22))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

