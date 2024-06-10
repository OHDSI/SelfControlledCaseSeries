# Code to upload the exported results of the multi-analysis vignette to a SQLite database

# Currently requires installing OhdsiSharing from https://github.com/anthonysena/OhdsiSharing/tree/issue-6-upload-results
library(OhdsiSharing)
library(DatabaseConnector)
library(SelfControlledCaseSeries)

outputFolder <- "d:/temp/sccsVignette2"

unlink(file.path(outputFolder, "Results.sqlite"))

connectionDetails <- createConnectionDetails(
  dbms = "sqlite",
  server = file.path(outputFolder, "Results.sqlite")
)

unlink(file.path(outputFolder, "export",  "resultsDataModelSpecification.csv"))
file.copy(
  from = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"),
  to = file.path(outputFolder, "export")
)

createResultsDataModel(
  connectionDetails = connectionDetails,
  databaseSchema = "main"
)

SelfControlledCaseSeries::uploadResults(
  connectionDetails = connectionDetails,
  schema = "main",
  zipFileName = file.path(outputFolder, "export", "Results_MDCD.zip"),
  purgeSiteDataBeforeUploading = FALSE
)

# Add cg_cohort_definition table:
cohortDefinition <- dplyr::tibble(
  cohortDefinitionId = 1,
  cohortName = "GI bleed",
  description = "Gastrointestinal bleeding",
  json = ""
)
connection <- connect(connectionDetails)
insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "cg_cohort_definition",
  data = cohortDefinition,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE)
disconnect(connection)

# Add database_meta_data table:
databaseMetaData <- dplyr::tibble(
  databaseId = "MDCD",
  cdmSourceAbbreviation = "MDCD"
)
connection <- connect(connectionDetails)
insertTable(
  connection = connection,
  databaseSchema = "main",
  tableName = "database_meta_data",
  data = databaseMetaData,
  dropTableIfExists = TRUE,
  createTable = TRUE,
  camelCaseToSnakeCase = TRUE)
disconnect(connection)



# csv <- readr::read_csv(file.path(outputFolder, "export", "sccs_attrition.csv"))
# csv$observed_days[is.na(csv$observed_days)] <- csv$days_observed[is.na(csv$observed_days)]
# csv$days_observed <- NULL
# readr::write_csv(csv, file.path(outputFolder, "export", "sccs_attrition.csv"))
