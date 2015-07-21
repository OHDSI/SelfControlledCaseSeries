#' @keywords internal
testcode <- function() {
  library(SelfControlledCaseSeries)
  setwd("s:/temp")
  options(fftempdir = "s:/temp")

  sccsData <- loadSccsData("s:/temp/sccsData")
  system.time(sccsEraData <- createSccsEraData(sccsData, covariateStart = 1, covariatePersistencePeriod = 30))

  #user  system elapsed
  #296.59  114.57 1718.76

  #user  system elapsed
  #323.13   80.14 1153.08
  saveSccsEraData(sccsEraData, "sccsEraData")

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
  cdmDatabaseSchema <- "cdm_truven_ccae_6k"
  resultsDatabaseSchema <- "scratch"
  port <- NULL
  cdmVersion <- "4"

  # Settings for PostgreSQL
  dbms <- "postgresql"
  server <- "localhost/ohdsi"
  user <- "postgres"
  cdmDatabaseSchema <- "vocabulary5"
  resultsDatabaseSchema <- "scratch"
  port <- NULL
  cdmVersion <- "5"


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
                            conditionEraCovariates = TRUE,
                            procedureCovariates = TRUE,
                            visitCovariates = TRUE,
                            observationCovariates = TRUE,
                            measurementCovariates = TRUE,
                            deleteCovariatesSmallCount = 100,
                            cdmVersion = cdmVersion)
  saveSccsData(sccsData, "sccsDataBackPain")

  sccsData

  summary(sccsData)

  # You can start here if you already saved sccsData:
  sccsData <- loadSccsData("sccsDataBackPain")

  sccsEraData <- createSccsEraData(sccsData,  covariateStart = 0,
                                               covariatePersistencePeriod = 0,
                                               naivePeriod = 0,
                                               firstOutcomeOnly = FALSE,
                                               excludeConceptIds = NULL)

  saveSccsEraData(sccsEraData, "sccsEraDataBackPain")

  # You can start here if you already saved sccsEraData:
  sccsEraData <- loadSccsEraData("sccsEraDataBackPain")

  sccsEraData

  summary(sccsEraData)

  fit <- fitSccsModel(sccsEraData)

  model <- getModel(fit, sccsEraData)
  head(model)
}
