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
                                     maxAge = NULL,
                                     endOfObservationEraLength = 0) {
  if (is.null(covariateSettings)) {
    covariateSettings <- createEraCovariateSettings(
      includeEraIds = exposureId,
      stratifyById = TRUE,
      start = 0,
      end = 0,
      endAnchor = "era end"
    )
  }

  covariateIds <- c()
  if (is.list(covariateSettings) && class(covariateSettings) != "EraCovariateSettings") {
    for (i in 1:length(covariateSettings)) {
      covariateIds <- c(covariateIds, covariateSettings[[i]]$includeEraIds)
    }
  } else {
    covariateIds <- covariateSettings$includeEraIds
  }
  eraRef <- eras |>
    distinct(.data$eraId, .data$eraType) |>
    mutate(eraName = "")

  data <- Andromeda::andromeda(
    cases = cases |>
      mutate(observationPeriodStartDate = observationPeriodStartDate),
    eras = eras,
    eraRef = eraRef
  )
  attr(data, "metaData") <- list(
    outcomeIds = 10,
    attrition = tibble(outcomeId = 10),
    prevalences = tibble(outcomeId = 10)
  )
  class(data) <- "SccsData"
  attr(class(data), "package") <- "SelfControlledCaseSeries"

  studyPop <- createStudyPopulation(
    sccsData = data,
    outcomeId = 10,
    firstOutcomeOnly = firstOutcomeOnly,
    naivePeriod = naivePeriod,
    minAge = minAge,
    maxAge = maxAge
  )

  result <- createSccsIntervalData(
    studyPopulation = studyPop,
    sccsData = data,
    ageCovariateSettings = ageSettings,
    seasonalityCovariateSettings = seasonalitySettings,
    eraCovariateSettings = covariateSettings,
    endOfObservationEraLength = endOfObservationEraLength
  )
  return(list(outcomes = collect(result$outcomes), covariates = collect(result$covariates)))
}

test_that("Simple era construction", {
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(50, 25),
    eraEndDay = c(50, 75)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 365,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 0,
    endDay = 10000,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(500, 525),
    eraEndDay = c(500, 575)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(25, 25),
    eraEndDay = c(25, 75)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(24, 25),
    eraEndDay = c(24, 75)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(75, 25),
    eraEndDay = c(75, 75)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(76, 25),
    eraEndDay = c(76, 75)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(50, 25),
    eraEndDay = c(50, 25)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx", "rx"),
    caseId = c(1, 1, 1),
    eraId = c(10, 11, 11),
    eraValue = c(1, 1, 1),
    eraStartDay = c(50, 25, 70),
    eraEndDay = c(50, 75, 80)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx", "rx"),
    caseId = c(1, 1, 1),
    eraId = c(10, 11, 11),
    eraValue = c(1, 1, 1),
    eraStartDay = c(50, 25, 25),
    eraEndDay = c(50, 75, 50)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx", "rx"),
    caseId = c(1, 1, 1),
    eraId = c(10, 11, 12),
    eraValue = c(1, 1, 1),
    eraStartDay = c(50, 25, 60),
    eraEndDay = c(50, 75, 70)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "hoi", "rx", "rx", "rx"),
    caseId = c(1, 1, 1, 1, 1),
    eraId = c(10, 10, 11, 12, 13),
    eraValue = c(1, 1, 1, 1, 1),
    eraStartDay = c(50, 85, 25, 70, 70),
    eraEndDay = c(NA, NA, 75, 80, 77)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(50, 50),
    eraEndDay = c(50, 75)
  )
  result <- convertToSccsDataWrapper(cases,
    eras,
    covariateSettings = createEraCovariateSettings(
      includeEraIds = c(11, 12, 13),
      start = 1,
      end = 0,
      endAnchor = "era end"
    )
  )
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(75, 25))
  expect_equal(result$outcomes$y, c(1, 0))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Two HOIs, keeping both", {
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "hoi", "rx"),
    caseId = c(1, 1, 1),
    eraId = c(10, 10, 11),
    eraValue = c(1, 1, 1),
    eraStartDay = c(25, 50, 30),
    eraEndDay = c(25, 50, 60)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "hoi", "rx"),
    caseId = c(1, 1, 1),
    eraId = c(10, 10, 11),
    eraValue = c(1, 1, 1),
    eraStartDay = c(25, 50, 30),
    eraEndDay = c(25, 50, 60)
  )
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
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(50, 50),
    eraEndDay = c(50, 75)
  )
  expect_warning({
    result <- convertToSccsDataWrapper(cases,
      eras,
      covariateSettings = createEraCovariateSettings(
        includeEraIds = c(11),
        start = 0,
        end = 7,
        startAnchor = "era end",
        endAnchor = "era start"
      )
    )
  })
  expect_equal(result$outcomes |> count() |> pull(), 0)
})

test_that("Aggregates on large set", {
  settings <- createSccsSimulationSettings(includeAgeEffect = FALSE, includeSeasonality = FALSE)
  sccsData <- simulateSccsData(1000, settings)
  studyPop <- createStudyPopulation(
    sccsData = sccsData,
    naivePeriod = 0,
    firstOutcomeOnly = FALSE,
  )
  sccsIntervalData <- createSccsIntervalData(
    studyPopulation = studyPop,
    sccsData,
    eraCovariateSettings = createEraCovariateSettings(
      includeEraIds = c(1, 2),
      endAnchor = "era end",
      stratifyById = TRUE,
    )
  )

  x <- sccsData$eras |>
    filter(.data$eraId == 1) |>
    collect()
  y <- sccsData$eras |>
    filter(.data$eraId == 10) |>
    collect()
  z <- inner_join(x, y, by = join_by("caseId"), relationship = "many-to-many") |>
    filter(.data$eraStartDay.y >= .data$eraStartDay.x & .data$eraStartDay.y <= .data$eraEndDay.x) |>
    distinct(.data$caseId) |>
    pull()

  x <- sccsIntervalData$covariates |>
    filter(.data$covariateId == 1000) |>
    collect()
  y <- sccsIntervalData$outcomes |>
    filter(.data$y != 0) |>
    collect()
  z2 <- inner_join(x, y, by = join_by("rowId")) |>
    distinct(.data$stratumId.x) |>
    pull()

  # Same people have the event during exposure to 1:
  expect_equal(z, z2)

  x <- sccsData$eras |>
    filter(.data$eraId == 2) |>
    collect()
  y <- sccsData$eras |>
    filter(.data$eraId == 10) |>
    collect()
  z <- inner_join(x, y, by = join_by("caseId"), relationship = "many-to-many") |>
    filter(.data$eraStartDay.y >= .data$eraStartDay.x & .data$eraStartDay.y <= .data$eraEndDay.x) |>
    distinct(.data$caseId) |>
    pull()

  x <- sccsIntervalData$covariates |>
    filter(.data$covariateId == 1001) |>
    collect()
  y <- sccsIntervalData$outcomes |>
    filter(.data$y != 0) |>
    collect()
  z2 <- inner_join(x, y, by = join_by("rowId")) |>
    distinct(.data$stratumId.x) |>
    pull()

  # Same people have the event during exposure to 2:
  expect_equal(z, z2)


  outcomes <- sccsIntervalData$outcomes |>
    group_by(.data$stratumId) |>
    summarise(time = sum(.data$time, na.rm = TRUE)) |>
    collect()

  z3 <- sccsData$cases |>
    select(stratumId = "caseId", "endDay") |>
    inner_join(outcomes, by = join_by("stratumId"), copy = TRUE) |>
    collect()

  # Same amount of times before and after conversion to concomitant eras:
  expect_equal(z3$endDay + 1, z3$time)
})

test_that("Merging exposures (stratifyById=FALSE)", {
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 0,
    endDay = 99,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx", "rx"),
    caseId = c(1, 1, 1),
    eraId = c(10, 11, 12),
    eraValue = c(1, 1, 1),
    eraStartDay = c(50, 25, 70),
    eraEndDay = c(50, 75, 100)
  )
  result <- convertToSccsDataWrapper(cases,
    eras,
    covariateSettings = createEraCovariateSettings(
      includeEraIds = c(11, 12),
      stratifyById = FALSE,
      start = 0,
      end = 0,
      endAnchor = "era end"
    )
  )
  expect_equal(result$outcomes$rowId, c(0, 1))
  expect_equal(result$outcomes$stratumId, c(1, 1))
  expect_equal(result$outcomes$time, c(25, 75))
  expect_equal(result$outcomes$y, c(0, 1))
  expect_equal(result$covariates$rowId, c(1))
  expect_equal(result$covariates$stratumId, c(1))
  expect_equal(result$covariates$covariateId, c(1000))
})

test_that("Pre-exposure window", {
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 0,
    endDay = 99,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(50, 25),
    eraEndDay = c(50, 75)
  )
  result <- convertToSccsDataWrapper(cases, eras, covariateSettings = list(
    createEraCovariateSettings(
      includeEraIds = 11,
      start = 0,
      end = 0,
      endAnchor = "era end"
    ),
    createEraCovariateSettings(
      includeEraIds = 11,
      start = -30,
      end = -1,
      endAnchor = "era start"
    )
  ))
  expect_equal(result$outcomes$rowId, c(0, 1, 2))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1))
  expect_equal(result$outcomes$time, c(24, 51, 25))
  expect_equal(result$outcomes$y, c(0, 1, 0))
  expect_equal(result$covariates$rowId, c(1, 2))
  expect_equal(result$covariates$stratumId, c(1, 1))
  expect_equal(result$covariates$covariateId, c(1000, 1001))
})

test_that("End of observation era", {
  cases <- tibble(
    observationPeriodId = "1000",
    caseId = 1,
    personId = "1",
    ageAtObsStart = 0,
    observationPeriodStartDate = as.Date("2000-5-1"),
    startDay = 1,
    endDay = 100,
    noninformativeEndCensor = 0
  )
  eras <- tibble(
    eraType = c("hoi", "rx"),
    caseId = c(1, 1),
    eraId = c(10, 11),
    eraValue = c(1, 1),
    eraStartDay = c(50, 20),
    eraEndDay = c(50, 40)
  )
  result <- convertToSccsDataWrapper(cases, eras, exposureId = 11, endOfObservationEraLength = 30)
  expect_equal(result$outcomes$rowId, c(0, 1, 2))
  expect_equal(result$outcomes$stratumId, c(1, 1, 1))
  expect_equal(result$outcomes$time, c(49, 30, 21))
  expect_equal(result$outcomes$y, c(1, 0, 0))
  expect_equal(result$covariates$rowId, c(1, 2))
  expect_equal(result$covariates$stratumId, c(1, 1))
  expect_equal(result$covariates$covariateId, c(99, 1000))
})
