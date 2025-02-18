library(SelfControlledCaseSeries)
library(testthat)
library(dplyr)

connectionDetails <- Eunomia::getEunomiaConnectionDetails()

# Setup data
connection <- DatabaseConnector::connect(connectionDetails)
person <- data.frame(
  personId = c(1, 2, 3),
  genderConceptId = c(8507, 8507, 8507),
  yearOfBirth = c(1975, 1975, 1975),
  monthOfBirth = c(8, 8, 8),
  dayOfBirth = c(5, 5, 5)
)
observationPeriods <- data.frame(
  observationPeriodId = c(1, 2, 3 ,4),
  personId = c(1, 2, 3, 3),
  observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-05", "2000-01-09", "2002-01-01")),
  observationPeriodEndDate = as.Date(c("2000-12-31", "2000-12-31", "2000-12-31", "2002-12-31"))
)
nestingCohort <- data.frame(
  cohortDefinitionId = c(1, 1, 1, 1),
  subjectId = c(1, 1, 2, 3),
  cohortStartDate = as.Date(c("2000-07-01", "2000-11-01", "2000-07-01", "2002-07-01")),
  cohortEndDate = as.Date(c("2000-09-30", "2000-12-31", "2000-11-30", "2002-12-31"))
)
outcomeCohort <- data.frame(
  cohortDefinitionId = c(2, 2, 2, 2),
  subjectId = c(1, 1, 2, 3),
  cohortStartDate = as.Date(c("2000-08-01", "2000-12-01", "2000-08-01", "2002-08-01")),
  cohortEndDate = as.Date(c("2000-08-01", "2000-12-01", "2000-08-01", "2002-08-01"))
)
exposureCohort <- data.frame(
  cohortDefinitionId = c(3, 3, 3, 3),
  subjectId = c(1, 2, 3, 3),
  cohortStartDate = as.Date(c("2000-08-01", "2000-06-01", "2000-08-01", "2002-08-01")),
  cohortEndDate = as.Date(c("2000-08-15", "2000-08-01", "2000-08-15", "2002-08-15"))
)
cohort <- rbind(nestingCohort, outcomeCohort, exposureCohort)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "person",
  data = person,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE
)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "observation_period",
  data = observationPeriods,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE
)
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "cohort",
  data = cohort,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE
)
DatabaseConnector::disconnect(connection)

sccsData <- getDbSccsData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  outcomeDatabaseSchema = "main",
  outcomeTable = "cohort",
  outcomeIds = 2,
  exposureDatabaseSchema = "main",
  exposureTable = "cohort",
  exposureIds = 3,
  nestingCohortDatabaseSchema = "main",
  nestingCohortTable = "cohort",
  nestingCohortId = 1,
  useNestingCohort = TRUE
)

test_that("getDbSccsData correctly handles nesting", {
  cases <- sccsData$cases |>
    collect()
  expectedCases <- observationPeriods |>
    inner_join(nestingCohort |>
                 select("subjectId", nestingStartDate = "cohortStartDate", nestingEndDate = "cohortEndDate"),
               # by = join_by("subjectId"),
               by = join_by("personId" == "subjectId", "observationPeriodEndDate" >= "nestingStartDate", "observationPeriodStartDate" <= "nestingEndDate"),
               relationship = "many-to-many")
  expect_equal(as.numeric(cases$observationPeriodId), expectedCases$observationPeriodId)
  expect_equal(as.numeric(cases$personId), expectedCases$personId)
  expect_equal(cases$observationPeriodStartDate, expectedCases$observationPeriodStartDate)
  expect_equal(cases$startDay, as.numeric(expectedCases$nestingStartDate - expectedCases$observationPeriodStartDate))

  hois <- sccsData$eras |>
    filter(eraType == "hoi") |>
    arrange(caseId, eraStartDay) |>
    collect()
  expectedHois <- outcomeCohort |>
    select(personId = "subjectId",  outcomeDate = "cohortStartDate") |>
    inner_join(cases |>
                 mutate(personId = as.numeric(.data$personId)),
               by = join_by("personId"),
               relationship = "many-to-many") |>
    mutate(outcomeDay = as.numeric(.data$outcomeDate - .data$observationPeriodStartDate)) |>
    filter(.data$outcomeDay  >= 0, .data$outcomeDay <= endDay) |>
    arrange(.data$caseId, .data$startDay)
  expect_equal(hois$caseId, expectedHois$caseId)
  expect_equal(hois$eraStartDay , expectedHois$outcomeDay)
})

studyPop <- createStudyPopulation(
  sccsData = sccsData,
  outcomeId = 2,
  firstOutcomeOnly = FALSE
)
sccsIntervalData <- createSccsIntervalData(
  studyPopulation = studyPop,
  sccsData = sccsData,
  eraCovariateSettings = createEraCovariateSettings(includeEraIds = 3),
  endOfObservationEraLength = 0
)
test_that("sccsIntervalData correctly handles nesting", {
  outcomes <- sccsIntervalData$outcomes |>
    collect()
  # StratumId = caseId
  expect_equal(outcomes$stratumId, c(1, 1, 3, 3, 4, 4))
  expect_equal(outcomes$time, c(77,15, 121, 32, 169, 15))
  expect_equal(outcomes$y, c(0, 1, 0, 1, 0 ,1))
})

sccsData <- getDbSccsData(
  connectionDetails = connectionDetails,
  cdmDatabaseSchema = "main",
  outcomeDatabaseSchema = "main",
  outcomeTable = "cohort",
  outcomeIds = 2,
  exposureDatabaseSchema = "main",
  exposureTable = "cohort",
  exposureIds = 3
)

test_that("Nesting in the period when the drug was on the market", {
  studyPop <- createStudyPopulation(
    sccsData = sccsData,
    outcomeId = 2,
    firstOutcomeOnly = FALSE,
    restrictTimeToEraId = 3
  )
  expectedMinMax <- exposureCohort |>
    summarise(minDate = min(.data$cohortStartDate),
              maxDate = max(.data$cohortEndDate))
  expect_equal(studyPop$metaData$restrictedTimeToEra$minObservedDate, expectedMinMax$minDate)
  expect_equal(studyPop$metaData$restrictedTimeToEra$maxObservedDate, expectedMinMax$maxDate)
  observedMinMax <- studyPop$cases |>
    summarise(minDate = min(.data$observationPeriodStartDate + .data$startDay),
              maxDate = max(.data$observationPeriodStartDate + .data$endDay))
  expect_equal(observedMinMax$minDate, expectedMinMax$minDate)
  expect_equal(observedMinMax$maxDate, expectedMinMax$maxDate)
})
