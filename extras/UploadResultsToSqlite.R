# Code to upload the exported results of the multi-analysis vignette to a SQLite database

# Currently requires installing OhdsiSharing from https://github.com/anthonysena/OhdsiSharing/tree/issue-6-upload-results
library(OhdsiSharing)
library(DatabaseConnector)

outputFolder <- "d:/temp/sccsVignette2"

connectionDetails <- createConnectionDetails(
  dbms = "sqlite",
  server = file.path(outputFolder, "Results.sqlite")
)

# file.copy(
#   from = system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"),
#   to = file.path(outputFolder, "export")
# )

csv <- readr::read_csv(system.file("csv", "resultsDataModelSpecification.csv", package = "SelfControlledCaseSeries"))
csv$empty_is_na <- FALSE
# csv$is_required[csv$table_name == "sccs_covariate" & csv$column_name == "covariate_analysis_id"] <- "No"
# csv$primary_key[csv$table_name == "sccs_age_spanning" & csv$column_name == "age_month"] <- "Yes"
# csv$primary_key[csv$table_name == "sccs_calendar_time_spanning" & csv$column_name == "year"] <- "Yes"
# csv$primary_key[csv$table_name == "sccs_calendar_time_spanning" & csv$column_name == "month"] <- "Yes"
readr::write_csv(csv, file.path(outputFolder, "export", "resultsDataModelSpecification.csv"))

createResultsDataModelTables(
  connectionDetails = connectionDetails,
  schema = "main",
  resultsFolder = file.path(outputFolder, "export")
)

uploadResults(
  connectionDetails = connectionDetails,
  schema = "main",
  resultsFolder = file.path(outputFolder, "export")
)



csv <- readr::read_csv(file.path(outputFolder, "export", "sccs_attrition.csv"))
csv$observed_days[is.na(csv$observed_days)] <- csv$days_observed[is.na(csv$observed_days)]
csv$days_observed <- NULL
readr::write_csv(csv, file.path(outputFolder, "export", "sccs_attrition.csv"))
