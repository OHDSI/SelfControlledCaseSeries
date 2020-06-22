library(SelfControlledCaseSeries)
options(andromedaTempFolder = "s:/andromedaTemp")

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "pdw",
                                                                server = Sys.getenv("PDW_SERVER"),
                                                                port = Sys.getenv("PDW_PORT"))

cdmDatabaseSchema <- "CDM_IBM_MDCD_V1153.dbo"

sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          outcomeIds = 192671,
                          exposureIds = 1124300)

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 192671,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 180)

covarDiclofenac = createEraCovariateSettings(label = "Exposure of interest",
                                             includeEraIds = 1124300,
                                             start = 0,
                                             end = 0,
                                             endAnchor = "era end")

sccsIntervalData <- createSccsIntervalData(studyPop,
                                           sccsData,
                                           eraCovariateSettings = covarDiclofenac)
model <- fitSccsModel(sccsIntervalData)
model
