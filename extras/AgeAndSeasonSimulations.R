library(SelfControlledCaseSeries)
options(andromedaTempFolder = "d:/andromedaTemp")
settings <- createSccsSimulationSettings(includeAgeEffect = F,
                                         includeCalendarTimeEffect = T,
                                         includeSeasonality = F)
set.seed(123)
sccsData <- simulateSccsData(5000, settings)
# summary(sccsData)
ageSettings <- createAgeCovariateSettings(ageKnots = 5,
                                          allowRegularization = TRUE)
seasonalitySettings <- createSeasonalityCovariateSettings(seasonKnots = 5,
                                                          allowRegularization = F)
calendarTimeSettings <- createCalendarTimeCovariateSettings(calendarTimeKnots = 5,
                                                            allowRegularization = TRUE)
covarSettings <- createEraCovariateSettings(label = "Exposure of interest",
                                            includeEraIds = c(1, 2),
                                            stratifyById = TRUE,
                                            start = 0,
                                            end = 0,
                                            endAnchor = "era end")

studyPop <- createStudyPopulation(sccsData = sccsData,
                                  outcomeId = 10,
                                  firstOutcomeOnly = FALSE,
                                  naivePeriod = 0)

sccsIntervalData <- createSccsIntervalData(studyPopulation = studyPop,
                                           sccsData = sccsData,
                                           eraCovariateSettings = covarSettings,
                                           # ageCovariateSettings = ageSettings,
                                           # seasonalityCovariateSettings = seasonalitySettings,
                                           calendarTimeCovariateSettings = calendarTimeSettings,
                                           minCasesForTimeCovariates = 10000)

model <- fitSccsModel(sccsIntervalData, prior = createPrior("none"), control = createControl(threads = 4))

estimate1 <- model$estimates[model$estimates$originalEraId == 1, ]
estimate2 <- model$estimates[model$estimates$originalEraId == 2, ]
writeLines(sprintf("True RR: %0.2f, estimate: %0.2f (%0.2f-%0.2f)",
                   settings$simulationRiskWindows[[1]]$relativeRisks,
                   exp(estimate1$logRr),
                   exp(estimate1$logLb95),
                   exp(estimate1$logUb95)))
writeLines(sprintf("True RR: %0.2f, estimate: %0.2f (%0.2f-%0.2f)",
                   settings$simulationRiskWindows[[2]]$relativeRisks,
                   exp(estimate2$logRr),
                   exp(estimate2$logLb95),
                   exp(estimate2$logUb95)))



# model
plotSeasonality(model)
plotAgeEffect(model)
plotCalendarTimeEffect(model)
plotEventToCalendarTime(studyPop, model)
computeTimeStability(studyPop)
computeTimeStability(studyPop, model)

### Plot simulated seasonality ###
estimates <- model$estimates
splineCoefs <- estimates[estimates$covariateId >= 200 & estimates$covariateId < 300, "logRr"]
seasonKnots <- model$metaData$seasonality$seasonKnots
season <- seq(0,12, length.out = 100)
seasonDesignMatrix <- cyclicSplineDesign(season, seasonKnots)
logRr <- apply(seasonDesignMatrix %*% splineCoefs, 1, sum)
logRr <- logRr - mean(logRr)
rr <- exp(logRr)
data <- data.frame(x = season, y = rr, type = "estimated")

x <- 1:365
y <- attr(sccsData, "metaData")$seasonFun(x)
y <- y - mean(y)
y <- exp(y)
x <- x * 12/365
data <- rbind(data, data.frame(x = x, y = y, type = "simulated"))
breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
seasonBreaks <- 1:12
rrLim <- c(0.1, 10)
theme <- ggplot2::element_text(colour = "#000000", size = 12)
themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, group = type, color = type)) +
  ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
  ggplot2::geom_line(lwd = 1) +
  ggplot2::scale_x_continuous("Month", breaks = seasonBreaks, labels = seasonBreaks) +
  ggplot2::scale_y_continuous("Relative risk",
                              limits = rrLim,
                              trans = "log10",
                              breaks = breaks,
                              labels = breaks) +
  ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0),
                                         rgb(0, 0, 0.8))) +
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

print(plot)


### Plot simulated age effect ###
# estimates <- model$estimates
# estimates <- estimates[estimates$covariateId >= 100 & estimates$covariateId < 200, ]
# splineCoefs <- c(0, estimates$logRr)
# ageKnots <- model$metaData$age$ageKnots
# age <- seq(min(ageKnots), max(ageKnots), length.out = 100)
# ageDesignMatrix <- splines::bs(age,
#                                knots = ageKnots[2:(length(ageKnots) - 1)],
#                                Boundary.knots = ageKnots[c(1, length(ageKnots))])
# logRr <- apply(ageDesignMatrix %*% splineCoefs, 1, sum)
# logRr <- logRr - mean(logRr)
# rr <- exp(logRr)
# data <- data.frame(x = age, y = rr, type = "estimated")
#
# x <- age
# y <- attr(sccsData, "metaData")$ageFun(x)
# y <- y - mean(y)
# y <- exp(y)
# data <- rbind(data, data.frame(x = x, y = y, type = "simulated"))
# breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
# ageLabels <- unique(round(age/3655) * 10)
# ageBreaks <- ageLabels * 365.5
# rrLim <- c(0.1, 10)
# theme <- ggplot2::element_text(colour = "#000000", size = 12)
# themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
# plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, group = type, color = type)) +
#   ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
#   ggplot2::geom_line(lwd = 1) +
#   ggplot2::scale_x_continuous("Age", breaks = ageBreaks, labels = ageLabels) +
#   ggplot2::scale_y_continuous("Relative risk",
#                               limits = rrLim,
#                               trans = "log10",
#                               breaks = breaks,
#                               labels = breaks) +
#   ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0),
#                                          rgb(0, 0, 0.8))) +
#   ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
#                  panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
#                  panel.grid.major = ggplot2::element_blank(),
#                  axis.ticks = ggplot2::element_blank(),
#                  axis.text.y = themeRA,
#                  axis.text.x = theme,
#                  strip.text.x = theme,
#                  strip.background = ggplot2::element_blank(),
#                  legend.title = ggplot2::element_blank(),
#                  legend.position = "top")
# print(plot)
# ggplot2::ggsave("s:/temp/age.png", plot, width = 5, height = 4, dpi = 300)


### Plot simulated calendar time effect ###
estimates <- model$estimates
estimates <- estimates[estimates$covariateId >= 300 & estimates$covariateId < 400, ]
splineCoefs <- estimates$logRr

calendarTime <- seq(settings$minCalendarTime, settings$maxCalendarTime, length.out = 50)
calendarMonth <- as.numeric(format(calendarTime,'%Y')) * 12 + as.numeric(format(calendarTime,'%m')) - 1
calendarTimeKnots <- model$metaData$calendarTime$calendarTimeKnotsInPeriods[[1]]
calendarTimeDesignMatrix <- splines::bs(x = calendarMonth,
                                        knots = calendarTimeKnots[2:(length(calendarTimeKnots) - 1)],
                                        Boundary.knots = calendarTimeKnots[c(1, length(calendarTimeKnots))],
                                        degree = 2)
logRr <- apply(calendarTimeDesignMatrix %*% splineCoefs, 1, sum)
logRr <- logRr - mean(logRr)
rr <- exp(logRr)
data <- data.frame(x = calendarTime, y = rr, type = "estimated")

x <- calendarTime
y <- attr(sccsData, "metaData")$calendarTimeFun(x)
y <- y - mean(y)
y <- exp(y)
data <- rbind(data, data.frame(x = x, y = y, type = "simulated"))
breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
rrLim <- c(0.1, 10)
theme <- ggplot2::element_text(colour = "#000000", size = 12)
themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
plot <- ggplot2::ggplot(data, ggplot2::aes(x = x, y = y, group = type, color = type)) +
  ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, size = 0.2) +
  ggplot2::geom_line(lwd = 1) +
  ggplot2::geom_vline(xintercept = SelfControlledCaseSeries:::convertMonthToStartDate(calendarTimeKnots)) +
  ggplot2::scale_x_date("calendarTime") +
  ggplot2::scale_y_continuous("Relative risk",
                              limits = rrLim,
                              trans = "log10",
                              breaks = breaks,
                              labels = breaks) +
  ggplot2::scale_color_manual(values = c(rgb(0.8, 0, 0),
                                         rgb(0, 0, 0.8))) +
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
print(plot)
