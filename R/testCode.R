#' @keywords internal
testcode <- function(){
  library(SelfControlledCaseSeries)
  setwd("s:/temp")
  options("fftempdir" = "s:/temp")

  pw <- NULL
  dbms <- "sql server"
  user <- NULL
  server <- "RNDUSRDHIT07"
  cdmDatabaseSchema <- "cdm4_sim.dbo"
  #cdmSchema <- "CDM_Truven_MDCR"
  resultsDatabaseSchema <- "scratch.dbo"
  port <- NULL

  
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms=dbms, server=server, user=user, password=pw, schema=cdmDatabaseSchema,port=port)
  
  sccsData <- getDbSccsData(connectionDetails,
                                cdmDatabaseSchema=cdmDatabaseSchema,
                                resultsDatabaseSchema=resultsDatabaseSchema,
                                outcomeConceptIds = 194133)
  saveSccsData(sccsData, "sccsData")
  
  sccsData <- loadSccsData("sccsData", readOnly = TRUE)
  
  library(ffbase)
  cases <- as.ram(sccsData$cases[1:10,])
  eras <- as.ram(sccsData$eras)
  eras <- subset(eras, observationPeriodId %in% cases$observationPeriodId)
  x <- convertToSccs(cases, eras)
}
