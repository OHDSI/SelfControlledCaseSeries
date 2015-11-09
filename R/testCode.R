#' @keywords internal
testcode <- function() {
  library(SelfControlledCaseSeries)
  setwd("s:/temp")
  options(fftempdir = "s:/fftemp")
  sccsData <- loadSccsData("s:/temp/sccsDataGiBleed")
  naivePeriod = 180
  outcomeId = 3



  library(SelfControlledCaseSeries)
  setwd("s:/temp")
  options(fftempdir = "s:/temp")
  settings <- createSccsSimulationSettings()
  settings <- createSccsSimulationSettings(includeAgeEffect = FALSE, includeSeasonality = FALSE)

  sccsData <- simulateSccsData(1000, settings)

  sccsEraData <- createSccsEraData(sccsData,
                                   naivePeriod = 0,
                                   firstOutcomeOnly = FALSE,
                                   exposureId = c(1,2),
                                   includeAgeEffect = TRUE,
                                   includeSeasonality = TRUE,
                                   exposureOfInterestSettings = createCovariateSettings(stratifyByID = TRUE,
                                                                                        start = 0,
                                                                                        end = 0,
                                                                                        addExposedDaysToEnd = TRUE,
                                                                                        splitPoints = c(7)))

  model <- fitSccsModel(sccsEraData, exposureId = c(1,2), prior = createPrior("none"))
  exp(model$coefficients)
  exp(model$treatmentEstimate)
  sccsData$metaData$sccsSimulationSettings$simulationRiskWindows


  sccsEraData$covariates

  sccsEraData$outcomes
  sccsEraData$metaData



  #plotAgeEffect(model)
 # plotSeasonality(model)


  allCoefs <- coef(model)
  coefId <- as.numeric(names(allCoefs))
  splineCoefs <- allCoefs[coefId >= 100 & coefId < 120]
  splineCoefs <- c(0, splineCoefs)
  ageKnots <- sccsEraData$metaData$ageKnots
  age <- seq(min(ageKnots), max(ageKnots), length.out = 100)
  ageDesignMatrix <- splines::bs(age, knots = ageKnots[2:(length(ageKnots)-1)], Boundary.knots = ageKnots[c(1,length(ageKnots))])
  logRr <- apply(ageDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  targetLogRr <- sccsEraData$metaData$ageFun(age)
  targetLogRr <- targetLogRr - mean(targetLogRr)
  targetRr <- exp(targetLogRr)

  data <- data.frame(age = rep(age,2), rr = c(rr,targetRr), group = rep(c("Estimated","Simulated"),each = length(rr)))
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  ageLabels <- floor(min(ageKnots)/365.25) : floor(max(ageKnots)/365.25)
  if (length(ageLabels) > 10){
    ageLabels <- 10*(floor(min(ageKnots)/3652.5) : floor(max(ageKnots)/3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  ggplot2::ggplot(data, ggplot2::aes(x = age, y = rr, group = group, color = group)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1,lw = 0.2) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Age", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Relative risk", lim = c(0.1,10), trans = "log10", breaks = breaks, labels = breaks) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.text.y = themeRA,
                     axis.text.x = theme,
                     strip.text.x = theme,
                     strip.background = ggplot2::element_blank(),
                     legend.title = ggplot2::element_blank(),
                     legend.position = "top")



  allCoefs <- coef(model)
  coefId <- as.numeric(names(allCoefs))
  splineCoefs <- allCoefs[coefId >= 200 & coefId < 220]
  splineCoefs <- c(0, splineCoefs)
  seasonKnots <- sccsEraData$metaData$seasonKnots
  season <- seq(min(seasonKnots), max(seasonKnots), length.out = 100)
  seasonDesignMatrix <- cyclicSplineDesign(season, seasonKnots)
  logRr <- apply(seasonDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  days <- (season*30.5 - 15) %% 365
  targetLogRr <- sccsEraData$metaData$seasonFun(days)
  targetLogRr <- targetLogRr - mean(targetLogRr)
  targetRr <- exp(targetLogRr)
  targetRr <- targetRr - mean(targetRr) + 1

  data <- data.frame(season = rep(season,2), rr = c(rr,targetRr), group = rep(c("Estimated","Simulated"),each = length(rr)))
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  seasonBreaks <- 1:12
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  ggplot2::ggplot(data, ggplot2::aes(x = season, y = rr, group = group, color = group)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1,lw = 0.2) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous("Month", breaks = seasonBreaks, labels = seasonBreaks) +
    ggplot2::scale_y_continuous("Relative risk", lim = c(0.1,10), trans = "log10", breaks = breaks, labels = breaks) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   strip.text.x = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.title = ggplot2::element_blank(),
                   legend.position = "top")








  library(SelfControlledCaseSeries)
  library(SqlRender)
  setwd("s:/temp")
  options(fftempdir = "s:/fftemp")

  # Settings for SQL Server (at JnJ)
  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  #cdmDatabaseSchema <- "cdm4_sim.dbo"
  cdmDatabaseSchema <- 'CDM_Truven_MDCd.dbo'
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL
  cdmVersion <- "4"

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

  sql <- loadRenderTranslateSql("coxibVsNonselVsGiBleed.sql",
                                packageName = "CohortMethod",
                                dbms = dbms,
                                cdmDatabaseSchema = cdmDatabaseSchema,
                                resultsDatabaseSchema = resultsDatabaseSchema)

  connection <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::executeSql(connection, sql)


  sql <- "SELECT cohort_concept_id, COUNT(*) AS count FROM @resultsDatabaseSchema.coxibVsNonselVsGiBleed GROUP BY cohort_concept_id"
  sql <- SqlRender::renderSql(sql, resultsDatabaseSchema = resultsDatabaseSchema)$sql
  sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
  DatabaseConnector::querySql(connection, sql)
  dbDisconnect(connection)

  sccsData <- getDbSccsData(connectionDetails,
                            cdmDatabaseSchema = cdmDatabaseSchema,
                            exposureIds = c(1,2),
                            outcomeIds = 3,
                            excludeConceptIds = c(),
                            exposureDatabaseSchema = resultsDatabaseSchema,
                            exposureTable = "coxibVsNonselVsGiBleed",
                            outcomeDatabaseSchema = resultsDatabaseSchema,
                            outcomeTable = "coxibVsNonselVsGiBleed",
                            drugEraCovariates = FALSE,
                            conditionEraCovariates = FALSE,
                            procedureCovariates = FALSE,
                            visitCovariates = FALSE,
                            observationCovariates = FALSE,
                            measurementCovariates = FALSE,
                            deleteCovariatesSmallCount = 100,
                            cdmVersion = cdmVersion)
  saveSccsData(sccsData, "s:/temp/sccsDataGiBleed")

  sccsData

  summary(sccsData)

  # You can start here if you already saved sccsData:
  # sccsData <- loadSccsData("s:/temp/sccsDataGiBleed")

  sccsEraData <- createSccsEraData(sccsData = sccsData,
                                   naivePeriod = 180,
                                   exposureId = 1,
                                   outcomeId = 3,
                                   firstOutcomeOnly = FALSE,
                                   includeExposureOfInterest = TRUE,
                                   exposureOfInterestSettings = createCovariateSettings(stratifyByID = TRUE,
                                                                                        start = 0,
                                                                                        end = 0,
                                                                                        addExposedDaysToEnd = TRUE,
                                                                                        mergeErasBeforeSplit = TRUE,
                                                                                        splitPoints = c(7,15)),
                                   includePreExposureOfInterest = TRUE,
                                   preExposureOfInterestSetting = createCovariateSettings(stratifyByID = TRUE,
                                                                                          mergeErasBeforeSplit = FALSE,
                                                                                          start = -180,
                                                                                          end = -1,
                                                                                          addExposedDaysToEnd = FALSE,
                                                                                          splitPoints = c(-90,-60,-30)),
                                   includeAgeEffect = FALSE,
                                   includeSeasonality = FALSE)

  saveSccsEraData(sccsEraData, "s:/temp/sccsEraDataGiBleed")

  # You can start here if you already saved sccsEraData:
  # sccsEraData <- loadSccsEraData("sccsEraDataGiBleed")
  sccsEraData$covariateRef

outcomes <- ff::as.ram(sccsEraData$outcomes)
covariates <- ff::as.ram(sccsEraData$covariates)
eras <- ff::as.ram(sccsData$eras)

outcomes[outcomes$stratumId == 6700,]
covariates[covariates$covariateId == 304,]
eras[eras$observationPeriodId == 6700 & eras$conceptId %in% c(1,3), ]

sccsEraData

  summary(sccsEraData)

  model <- fitSccsModel(sccsEraData, exposureId = 1, prior = createPrior("none"))
  model$coefficients

  saveRDS(model, "s:/temp/sccsModel.rds")
  plotSeasonality(model)
  plotAgeEffect(model, rrLim =c(0.01, 100))

  model <- getModel(fit, sccsEraData)
  head(model)

}
