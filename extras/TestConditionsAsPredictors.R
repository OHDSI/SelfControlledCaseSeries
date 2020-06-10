library(SqlRender)
library(DatabaseConnector)
library(SelfControlledCaseSeries)
setwd("s:/temp")
options(fftempdir = "s:/fftemp")

pw <- NULL
dbms <- "pdw"
user <- NULL
server <- Sys.getenv("PDW_SERVER")
cdmDatabaseSchema <- "CDM_Truven_MDCD_V417.dbo"
cohortDatabaseSchema <- "scratch.dbo"
oracleTempSchema <- NULL
outcomeTable <- "mschuemi_sccs_vignette"
port <- Sys.getenv("PDW_PORT")
cdmVersion <- "5"

connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                server = server,
                                                                user = user,
                                                                password = pw,
                                                                port = port)

connection <- DatabaseConnector::connect(connectionDetails)

sql <- loadRenderTranslateSql("vignette.sql",
                              packageName = "SelfControlledCaseSeries",
                              dbms = dbms,
                              cdmDatabaseSchema = cdmDatabaseSchema,
                              cohortDatabaseSchema = cohortDatabaseSchema,
                              outcomeTable = outcomeTable)

DatabaseConnector::executeSql(connection, sql)

# Check number of subjects per cohort:
sql <- "SELECT cohort_definition_id, COUNT(*) AS count FROM @cohortDatabaseSchema.@outcomeTable GROUP BY cohort_definition_id"
sql <- SqlRender::renderSql(sql,
                            cohortDatabaseSchema = cohortDatabaseSchema,
                            outcomeTable = outcomeTable)$sql
sql <- SqlRender::translateSql(sql, targetDialect = connectionDetails$dbms)$sql
DatabaseConnector::querySql(connection, sql)

sccsData <- getDbSccsData(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema,
                          outcomeDatabaseSchema = cohortDatabaseSchema,
                          outcomeTable = outcomeTable,
                          outcomeIds = 1,
                          exposureDatabaseSchema = cdmDatabaseSchema,
                          exposureTable = "drug_era",
                          exposureIds = c(),
                          useCustomCovariates = TRUE,
                          customCovariateDatabaseSchema = cdmDatabaseSchema,
                          customCovariateTable = "condition_era",
                          customCovariateIds = c(),
                          cdmVersion = cdmVersion)
saveSccsData(sccsData, "s:/temp/sccsConditions/sccsData")
sccsData <- loadSccsData("s:/temp/sccsConditions/sccsData")

summary(sccsData)

counts <- Cyclops:::.bySum(sccsData$eras$value, sccsData$eras$conceptId)
counts <- counts[order(-counts$sums), ]
min(counts$sums)


drugs <- ff::as.ram(unique(sccsData$eras$conceptId[sccsData$eras$eraType == 'rx']))
conditions <- ff::as.ram(unique(sccsData$eras$conceptId[sccsData$eras$eraType == 'dx']))

diclofenac <- 1124300
dicyclomine <- 924724

covarDoi <- createCovariateSettings(label = "Exposure of interest",
                                             includeCovariateIds = dicyclomine,
                                             start = 0,
                                             end = 0,
                                             addExposedDaysToEnd = TRUE)

covarAllDrugs <- createCovariateSettings(label = "Other exposures",
                                         includeCovariateIds = drugs,
                                         excludeCovariateIds = dicyclomine,
                                         stratifyById = TRUE,
                                         start = 1,
                                         end = 0,
                                         addExposedDaysToEnd = TRUE,
                                         allowRegularization = TRUE)

covarAllConditions30d <- createCovariateSettings(label = "Conditions 30 days prior",
                                              includeCovariateIds = conditions,
                                              stratifyById = TRUE,
                                              start = 1,
                                              end = 30,
                                              addExposedDaysToEnd = TRUE,
                                              allowRegularization = TRUE)

# covarAllConditions180d <- createCovariateSettings(label = "Conditions 180 days prior",
#                                               includeCovariateIds = conditions,
#                                               stratifyById = TRUE,
#                                               start = 1,
#                                               end = 180,
#                                               addExposedDaysToEnd = TRUE,
#                                               allowRegularization = TRUE)

sccsIntervalData <- createSccsIntervalData(sccsData,
                                 naivePeriod = 180,
                                 firstOutcomeOnly = FALSE,
                                 covariateSettings = list(covarDoi,
                                                          covarAllDrugs,
                                                          covarAllConditions30d))

saveSccsIntervalData(sccsIntervalData, "s:/temp/sccsConditions/sccsIntervalData")
sccsIntervalData <- loadSccsIntervalData("s:/temp/sccsConditions/sccsIntervalData")
summary(sccsIntervalData)
sccsIntervalData$covariates
sccsIntervalData$outcomes
rownames(sccsIntervalData$covariates) <- NULL #Needs to be null or the ordering of ffdf will fail
temp <- sccsIntervalData$covariates[c("covariateId", "stratumId","rowId")]
idx <- ff::ffdforder(sccsIntervalData$covariates[c("covariateId", "stratumId","rowId")])
covariates <- sccsIntervalData$covariates[idx,]

x <- Cyclops:::.bySum(sccsIntervalData$covariates$covariateValue, sccsIntervalData$covariates$covariateId)
x <- x[order(-x$sums),]

m <- merge(x, sccsIntervalData$covariateRef, by.x = "bins", by.y = "covariateId")
m <- m[order(-m$sums),]

control <- createControl(cvType = "auto",
                         selectorType = "byPid",
                         startingVariance = 0.1,
                         noiseLevel = "quiet",
                         threads = 20)
# variance <- 0.00283335
debug(Cyclops:::convertToCyclopsData.ffdf)
#debug(fitSccsModel)
model <- fitSccsModel(sccsIntervalData, control = control)
saveRDS(model, "s:/temp/sccsConditions/model.rds")
summary(model)
estimates <- getModel(model)
estimates[estimates$originalCovariateId == dicyclomine, ]





as <- readRDS("S:/Temp/sccsVignette2/analysisSummary.rds")
as[as$exposureId == dicyclomine,]
