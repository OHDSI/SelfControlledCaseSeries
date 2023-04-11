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
  observationPeriodStartDate = as.Date(c("2000-01-01", "2000-01-01", "2000-01-01", "2002-01-01")),
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
  cohortEndDate = as.Date(c("2000-08-01", "2000-128-01", "2000-08-01", "2002-08-01"))
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
  cases <- sccsData$cases %>%
    collect()
  expect_equal(cases$observationPeriodId, c("1.0", "1.0", "2.0", "4.0"))
  expect_equal(cases$personId, c("1.0", "1.0", "2.0", "3.0"))
  expect_equal(cases$caseId, c(1, 2, 3, 4))
  expect_equal(cases$startYear, c(2000, 2000, 2000, 2002))
  expect_equal(cases$startMonth , c(7, 11, 7, 7))
  expect_equal(cases$startDay , c(1, 1, 1, 1))

  hois <- sccsData$eras %>%
    filter(eraType == "hoi") %>%
    arrange(caseId, startDay) %>%
    collect()
  expect_equal(hois$caseId, c(1, 1, 2, 2, 3, 4))
  expect_equal(hois$startDay , c(31, 153, -92, 30, 31, 31))
})

studyPop <- createStudyPopulation(
  sccsData = sccsData,
  outcomeId = 2,
  firstOutcomeOnly = FALSE
)
sccsIntervalData <- createSccsIntervalData(
  studyPopulation = studyPop,
  sccsData = sccsData,
  eraCovariateSettings = createEraCovariateSettings(includeEraIds = 3)
)
test_that("sccsIntervalData correctly handles nesting", {
  outcomes <- sccsIntervalData$outcomes %>%
    collect()
  # StratumId = caseId
  expect_equal(outcomes$stratumId, c(1, 1, 3, 3, 4, 4))
  expect_equal(outcomes$time, c(77,15, 121, 32, 169, 15))
  expect_equal(outcomes$y, c(0, 1, 0, 1, 0 ,1))
})
