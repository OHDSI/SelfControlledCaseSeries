#' @keywords internal
testcode <- function() {
  library(SelfControlledCaseSeries)
  setwd("s:/temp")
  options(fftempdir = "s:/temp")

  # Settings for SQL Server (at JnJ)
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  cdmDatabaseSchema <- "cdm4_sim.dbo"
  # cdmSchema <- 'CDM_Truven_MDCR'
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL

  # Settings for PostgreSQL
  dbms <- "postgresql"
  server <- "localhost/ohdsi"
  user <- "postgres"
  cdmDatabaseSchema <- "cdm4_sim"
  resultsDatabaseSchema <- "scratch"
  port <- NULL

  # Settings for OHDSI test environment
  dbms <- "postgresql"
  server <-  Sys.getenv("CDM5_POSTGRESQL_SERVER")
  user <- Sys.getenv("CDM5_POSTGRESQL_USER")
  pw <-  URLdecode(Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
  cdmDatabaseSchema <- Sys.getenv("CDM5_POSTGRESQL_SCHEMA")
  resultsDatabaseSchema <- "scratch"
  port <- NULL

  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  schema = cdmDatabaseSchema,
                                                                  port = port)

  # low back pain
  sccsData <- getDbSccsData(connectionDetails,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            outcomeConceptIds = 194133,
                            exposureConceptIds = c(),
                            excludeConceptIds = c(),
                            drugEraCovariates = FALSE,
                            conditionEraCovariates = FALSE,
                            procedureCovariates = FALSE,
                            visitCovariates = FALSE,
                            observationCovariates = FALSE,
                            deleteCovariatesSmallCount = 100)
  saveSccsData(sccsData, "sccsData")

  # You can start here if you already saved sccsData:
  sccsData <- loadSccsData("sccsData", readOnly = TRUE)

  system.time(sccsEraData <- createSccsEraData(sccsData,
                                               covariateStart = 0,
                                               covariatePersistencePeriod = 0,
                                               naivePeriod = 0,
                                               firstOutcomeOnly = FALSE,
                                               excludeConceptIds = NULL))
  # user system elapsed 36.05 2.15 49.48
  saveSccsEraData(sccsEraData, "sccsEraData")

  # You can start here if you already saved sccsEraData:
  sccsEraData <- loadSccsEraData("sccsEraData")

  fit <- fitModel(sccsEraData)

  model <- getModel(fit, sccsEraData)
  head(model)
}
