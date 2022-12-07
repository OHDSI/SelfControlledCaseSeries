library(DatabaseConnector)
library(dplyr)

source("dataPulls.R")
source("plotsAndTables.R")

connectionPool <- pool::dbPool(
    drv = DatabaseConnector::DatabaseConnectorDriver(),
    dbms = "sqlite",
    server = "d:/temp/sccsVignette2/Results.sqlite"
  )

resultsDatabaseSchema <- "main"

exposuresOutcomeSets <- getExposuresOutcomes(connectionPool, resultsDatabaseSchema)
exposuresOutcomeNames <- exposuresOutcomeSets %>%
  group_by(exposuresOutcomeSetId, outcomeName) %>%
  summarise(exposures = paste(exposureName, collapse = ", "), .groups = "drop") %>%
  mutate(name = sprintf("%s - %s", exposures, outcomeName))

sccsAnalyses <- getTable(connectionPool, resultsDatabaseSchema, "sccs_analysis")

databases <- getTable(connectionPool, resultsDatabaseSchema, "database_meta_data")
