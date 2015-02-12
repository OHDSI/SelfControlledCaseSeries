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
  cases <- as.ram(sccsData$cases)
  eras <- as.ram(sccsData$eras)
  eras <- subset(eras, observationPeriodId %in% cases$observationPeriodId)
  x <- convertToSccs(cases, eras)
  x
  #simple
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei"), 
                     observationPeriodId = c(1,1), 
                     conceptId = c(10,11),
                     startDay = c(50,25),
                     endDay = c(50,75))
  x <- convertToSccs(cases, eras)
  x
  
  #one-day era
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei"), 
                     observationPeriodId = c(1,1), 
                     conceptId = c(10,11),
                     startDay = c(50,25),
                     endDay = c(50,25))
  x <- convertToSccs(cases, eras)
  x
  
  #merging
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei","hei"), 
                     observationPeriodId = c(1,1,1), 
                     conceptId = c(10,11,11),
                     startDay = c(50,25,70),
                     endDay = c(50,75,80))
  x <- convertToSccs(cases, eras)
  x
  
  #concomitant
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hei","hei"), 
                     observationPeriodId = c(1,1,1), 
                     conceptId = c(10,11,12),
                     startDay = c(50,25,70),
                     endDay = c(50,75,70))
  x <- convertToSccs(cases, eras)
  x
  
  #concomitant 3
  cases <- data.frame(observationPeriodId = 1, personId = 1, observationDays = 100)
  eras <- data.frame(eraType = c("hoi","hoi","hei","hei","hei"), 
                     observationPeriodId = c(1,1,1,1,1), 
                     conceptId = c(10,9,11,12,13),
                     startDay = c(50,85,25,70,70),
                     endDay = c(NA,NA,75,80,77))
  x <- convertToSccs(cases, eras)
  x
  
  #concomitant 3, with length of 1
  cases <- data.frame(observationPeriodId = 119455083, personId = 999999, observationDays = 1500)
  eras <- data.frame(eraType = c("hoi","hoi","hei","hei","hei"), 
                     observationPeriodId = c(119455083,119455083,119455083,119455083,119455083), 
                     conceptId = c(10,9,11,12,43013616),
                     startDay = c(50,85,25,70,70),
                     endDay = c(NA,NA,75,80,80))
  x <- convertToSccs(cases, eras)
  x
}
