prettyHr <- function(x) {
  result <- sprintf("%.2f", x)
  result[is.na(x) | x > 100] <- "NA"
  return(result)
}

convertToStartDate <- function(year, month) {
  return(as.Date(sprintf(
    "%s-%s-%s",
    year,
    month,
    1
  )))
}

convertToEndDate <- function(year, month) {
  year <- ifelse(month == 12, year + 1, year)
  month <- ifelse(month == 12, 1, month + 1)
  return(as.Date(sprintf(
    "%s-%s-%s",
    year,
    month,
    1
  )) - 1)
}

plotTimeTrend <- function(timeTrend) {

  timeTrend <- timeTrend %>%
    mutate(
      monthStartDate = convertToStartDate(calendarYear, calendarMonth),
      monthEndDate = convertToEndDate(calendarYear, calendarMonth),
      outcomeRate = pmax(0, outcomeRate),
      observedSubjects = pmax(0, observedSubjects),
      adjustedRate = pmax(0, adjustedRate))

  plotData <- bind_rows(
    select(timeTrend, "monthStartDate", "monthEndDate", value = "outcomeRate") %>%
      mutate(type = "Outcomes per person",
             stable = "Stable"),
    select(timeTrend, "monthStartDate", "monthEndDate", value = "observedSubjects") %>%
      mutate(type = "Observed persons",
             stable = "Stable"),
    select(timeTrend, "monthStartDate", "monthEndDate", value = "adjustedRate", "stable") %>%
      mutate(type = "Adj. outcomes per person",
             stable = ifelse(stable == 1, "Stable", "Unstable"))
  )

  levels <- c("Observed persons", "Outcomes per person", "Adj. outcomes per person")
  plotData$type <- factor(plotData$type, levels = rev(levels))

  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
  plot <- ggplot2::ggplot(plotData, ggplot2::aes(xmin = .data$monthStartDate, xmax = .data$monthEndDate + 1)) +
    ggplot2::geom_rect(ggplot2::aes(ymax = .data$value, fill = stable),
                       ymin = 0,
                       linewidth = 0) +
    ggplot2::scale_x_date("Calendar time") +
    ggplot2::scale_y_continuous("Count", limits = c(0, NA)) +
    ggplot2::scale_fill_manual(breaks = c("Stable", "Unstable") ,
                               values = c(rgb(0, 0, 0.8, alpha = 0.6), rgb(0.8, 0, 0, alpha = 0.6))) +
    ggplot2::facet_grid(.data$type ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.text = theme
    )
  return(plot)
}

plotTimeToEvent <- function(timeToEvent) {

  events <- timeToEvent %>%
    transmute(.data$week,
              type = "Events",
              count = .data$outcomes
    )

  observed <- timeToEvent %>%
    transmute(.data$week,
              type = "Subjects under observation",
              count = .data$observedSubjects
    )

  data <- bind_rows(events, observed) %>%
    mutate(count = pmax(0, .data$count),
           day = 3.5 + .data$week * 7)

  pLabel <- tibble(
    text = sprintf("P for pre-exposure gain = %0.2f", timeToEvent$p[1]),
    day = -178,
    count = max(events$count),
    type = "Events"
  )

  breaks <- seq(-180, 180, 30)
  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = day, y = count)) +
    ggplot2::geom_col(width = 7, fill = rgb(0, 0, 0.8), alpha = 0.6) +
    ggplot2::geom_vline(xintercept = -0.5, colour = "#000000", lty = 1, linewidth = 1) +
    ggplot2::geom_label(ggplot2::aes(label = .data$text), data = pLabel, hjust = 0, size = 4.5, alpha = 0.8) +
    ggplot2::scale_x_continuous("Days since first exposure start", breaks = breaks, labels = breaks) +
    ggplot2::scale_y_continuous("Count") +
    ggplot2::facet_grid(type ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#AAAAAA"),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  return(plot)
}

drawAttritionDiagram <- function(attrition) {
  formatNumber <- function(x) {
    return(formatC(x, big.mark = ",", format = "d"))
  }
  addStep <- function(data, attrition, row) {
    data$leftBoxText[length(data$leftBoxText) + 1] <- paste(attrition$description[row],
                                                            "\n",
                                                            "Cases: ",
                                                            formatNumber(attrition$outcomeSubjects[row]),
                                                            "\n",
                                                            "Outcomes: ",
                                                            formatNumber(attrition$outcomeEvents[row]),
                                                            sep = "")
    data$rightBoxText[length(data$rightBoxText) + 1] <- paste("Cases: ",
                                                              formatNumber(data$currentCases - attrition$outcomeSubjects[row]),
                                                              "\n",
                                                              "Outcomes: ",
                                                              formatNumber(data$currentOutcomes - attrition$outcomeEvents[row]),
                                                              sep = "")
    data$currentCases <- attrition$outcomeSubjects[row]
    data$currentOutcomes <- attrition$outcomeEvents[row]
    return(data)
  }
  data <- list(leftBoxText = c(paste("All outcomes occurrences:\n",
                                     "Cases: ",
                                     formatNumber(attrition$outcomeSubjects[1]),
                                     "\n",
                                     "Outcomes: ",
                                     formatNumber(attrition$outcomeEvents[1]),
                                     sep = "")),
               rightBoxText = c(""),
               currentCases = attrition$outcomeSubjects[1],
               currentOutcomes = attrition$outcomeEvents[1])
  for (i in 2:nrow(attrition)) {
    data <- addStep(data, attrition, i)
  }

  leftBoxText <- data$leftBoxText
  rightBoxText <- data$rightBoxText
  nSteps <- length(leftBoxText)

  boxHeight <- (1/nSteps) - 0.05
  boxWidth <- 0.45
  shadowOffset <- 0.01
  arrowLength <- 0.01
  x <- function(x) {
    return(0.25 + ((x - 1)/2))
  }
  y <- function(y) {
    return(1 - (y - 0.5) * (1/nSteps))
  }

  downArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!x1, y = !!y1, xend = !!x2, yend = !!y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!x2,
                                                y =!! y2,
                                                xend = !!x2 + arrowLength,
                                                yend = !!y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!x2,
                                                y = !!y2,
                                                xend = !!x2 - arrowLength,
                                                yend = !!y2 + arrowLength))
    return(p)
  }
  rightArrow <- function(p, x1, y1, x2, y2) {
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!x1, y = !!y1, xend = !!x2, yend = !!y2))
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!x2,
                                                y = !!y2,
                                                xend = !!x2 - arrowLength,
                                                yend = !!y2 + arrowLength))
    p <- p + ggplot2::geom_segment(ggplot2::aes(x = !!x2,
                                                y = !!y2,
                                                xend = !!x2 - arrowLength,
                                                yend = !!y2 - arrowLength))
    return(p)
  }
  box <- function(p, x, y) {
    p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = !!x - (boxWidth/2),
                                             ymin = !!y - (boxHeight/2),
                                             xmax = !!x + (boxWidth/2),
                                             ymax = !!y + (boxHeight/2)),
                                color = rgb(0, 0, 0.8, alpha = 1),
                                fill = rgb(0, 0, 0.8, alpha = 0.1))
    return(p)
  }
  label <- function(p, x, y, text, hjust = 0) {
    p <- p + ggplot2::geom_text(ggplot2::aes(x = !!x, y = !!y, label = !!text),
                                hjust = hjust,
                                size = 5)
    return(p)
  }

  p <- ggplot2::ggplot()
  for (i in 2:nSteps - 1) {
    p <- downArrow(p, x(1), y(i) - (boxHeight/2), x(1), y(i + 1) + (boxHeight/2))
  }
  for (i in 2:(nSteps)) {
    p <- rightArrow(p, x(1), y(i-0.5), x(2) - boxWidth/2, y(i-0.5))
  }
  for (i in 1:nSteps) {
    p <- box(p, x(1), y(i))
    p <- label(p, x(1) - boxWidth/2 + 0.02, y(i), text = leftBoxText[i])
  }
  for (i in 2:(nSteps )) {
    p <- box(p, x(2), y(i-0.5))
    p <- label(p, x(2) - boxWidth/2 + 0.02, y(i-0.5), text = rightBoxText[i])
  }
  p <- p + ggplot2::theme(legend.position = "none",
                          plot.background = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          axis.text = ggplot2::element_blank(),
                          axis.title = ggplot2::element_blank(),
                          axis.ticks = ggplot2::element_blank())
  p
  return(p)
}

plotEventDepObservation <- function(eventDepObservation, maxMonths = 12) {
  eventDepObservation <- eventDepObservation %>%
    filter(.data$monthsToEnd <= maxMonths) %>%
    mutate(
      outcomes = pmax(0, .data$outcomes),
      censoring = ifelse(.data$censored == 1, "Censored", "Uncensored")
    )
  timeBreaks <- 0:ceiling(max(eventDepObservation$monthsToEnd))
  timeLabels <- timeBreaks * 30.5

  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
  plot <- ggplot2::ggplot(eventDepObservation, ggplot2::aes(x = .data$monthsToEnd, y = .data$outcomes)) +
    # ggplot2::geom_vline(xintercept = timeBreaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_col(width = 1, fill = rgb(0, 0, 0.8), alpha = 0.6) +
    ggplot2::scale_x_continuous("Months from event", breaks = timeBreaks, labels = timeBreaks) +
    ggplot2::scale_y_continuous("Frequency") +
    ggplot2::facet_grid(censoring ~ ., scales = "free_y") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  return(plot)
}

plotSpanning <- function(spanning, type = "age") {
  if (type == "age") {
    spanning <- spanning  %>%
      mutate(x = .data$ageMonth)
    labels <- seq(ceiling(min(spanning$ageMonth) / 12), floor(max(spanning$ageMonth) / 12))
    breaks <- labels*12
    if (length(labels) > 10) {
      labels <- seq(ceiling(min(spanning$ageMonth) / 120), floor(max(spanning$ageMonth) / 120)) * 10
      breaks <- labels*12
    }
    xLabel <- "Age in years"
  } else {
    spanning <- spanning  %>%
      mutate(x = .data$calendarYear * 12 + .data$calendarMonth)
    labels <- seq(ceiling(min(spanning$x) / 12), floor(max(spanning$x) / 12))
    breaks <- labels*12
    xLabel <- "Calendar time"
  }
  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
  plot <- ggplot2::ggplot(spanning, ggplot2::aes(x = .data$x, y = .data$coverBeforeAfterSubjects)) +
    ggplot2::geom_vline(xintercept = breaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_col(width = 1, fill = rgb(0, 0, 0.8), alpha = 0.6) +
    ggplot2::scale_x_continuous(xLabel, breaks = breaks, labels = labels) +
    ggplot2::scale_y_continuous("Subjects") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  plot
  return(plot)
}

plotAgeSpline <- function(ageSpline, rrLim = c(0.1, 10)) {
  splineCoefs <- c(0, log(ageSpline$rr))
  ageKnots <- ageSpline$ageMonth
  age <- seq(min(ageKnots), max(ageKnots), length.out = 100)
  ageDesignMatrix <- splines::bs(age,
                                 knots = ageKnots[2:(length(ageKnots) - 1)],
                                 Boundary.knots = ageKnots[c(1, length(ageKnots))]
  )
  logRr <- apply(ageDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(age = age, rr = rr)
  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  ageLabels <- floor(min(ageKnots) / 365.25):floor(max(ageKnots) / 365.25)
  if (length(ageLabels) > 10) {
    ageLabels <- 10 * (floor(min(ageKnots) / 3652.5):floor(max(ageKnots) / 3652.5))
  }
  ageBreaks <- ageLabels * 365.25
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$age, y = .data$rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 1) +
    ggplot2::scale_x_continuous("Age", breaks = ageBreaks, labels = ageLabels) +
    ggplot2::scale_y_continuous("Relative risk",
                                limits = rrLim,
                                trans = "log10",
                                breaks = breaks,
                                labels = breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  return(plot)
}

plotSeasonSpline <- function(seasonSpline, rrLim = c(0.1, 10)) {
  season <- seq(1, 12, length.out = 100)
  seasonDesignMatrix <- cyclicSplineDesign(season, c(seasonSpline$knotMonth, 12))
  logRr <- apply(seasonDesignMatrix %*% log(seasonSpline$rr), 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(season = season, rr = rr)

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  seasonBreaks <- 1:12
  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = season, y = rr)) +
    ggplot2::geom_hline(yintercept = breaks, colour = "#AAAAAA", lty = 1, linewidth = 0.2) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 1) +
    ggplot2::scale_x_continuous("Month", breaks = seasonBreaks, labels = seasonBreaks) +
    ggplot2::scale_y_continuous("Relative risk",
                                limits = rrLim,
                                trans = "log10",
                                breaks = breaks,
                                labels = breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  return(plot)
}

plotCalendarTimeSpline <- function(calendarTimeSpline, rrLim = c(0.1, 10)) {
  splineCoefs <- c(0, log(calendarTimeSpline$rr))
  calendarTimeKnots <- calendarTimeSpline$knotMonth
  calendarTime <- seq(min(calendarTimeKnots), max(calendarTimeKnots), length.out = 100)
  calendarTimeDesignMatrix <- splines::bs(calendarTime,
                                          knots = calendarTimeKnots[2:(length(calendarTimeKnots) - 1)],
                                          Boundary.knots = calendarTimeKnots[c(1, length(calendarTimeKnots))]
  )
  logRr <- apply(calendarTimeDesignMatrix %*% splineCoefs, 1, sum)
  logRr <- logRr - mean(logRr)
  rr <- exp(logRr)
  data <- data.frame(calendarTime = calendarTime / 12, rr = rr)

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 12)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 12, hjust = 1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = calendarTime, y = rr)) +
    ggplot2::geom_line(color = rgb(0, 0, 0.8), alpha = 0.8, linewidth = 1) +
    ggplot2::scale_x_continuous("Calendar Time") +
    ggplot2::scale_y_continuous("Relative risk",
                                limits = rrLim,
                                trans = "log10",
                                breaks = breaks,
                                labels = breaks
    ) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.text.y = themeRA,
      axis.text.x = theme,
      axis.title = theme,
      strip.text.x = theme,
      strip.background = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(hjust = 0.5),
      legend.title = ggplot2::element_blank(),
      legend.position = "top"
    )
  return(plot)
}

cyclicSplineDesign <- function(x, knots, ord = 4) {
  nk <- length(knots)
  if (ord < 2) {
    stop("order too low")
  }
  if (nk < ord) {
    stop("too few knots")
  }
  knots <- sort(knots)
  k1 <- knots[1]
  if (min(x) < k1 || max(x) > knots[nk]) {
    stop("x out of range")
  }
  xc <- knots[nk - ord + 1]
  knots <- c(k1 - (knots[nk] - knots[(nk - ord + 1):(nk - 1)]), knots)
  ind <- x > xc
  X1 <- splines::splineDesign(knots, x, ord, outer.ok = TRUE)
  x[ind] <- x[ind] - max(knots) + k1
  if (sum(ind)) {
    X2 <- splines::splineDesign(knots, x[ind], ord, outer.ok = TRUE)
    X1[ind, ] <- X1[ind, ] + X2
  }
  X1
}

plotControlEstimates <- function(controlEstimates) {
  size <- 2
  labelY <- 0.7
  d <- rbind(data.frame(yGroup = "Uncalibrated",
                        logRr = controlEstimates$logRr,
                        seLogRr = controlEstimates$seLogRr,
                        ci95Lb = controlEstimates$ci95Lb,
                        ci95Ub = controlEstimates$ci95Ub,
                        trueRr = controlEstimates$trueEffectSize),
             data.frame(yGroup = "Calibrated",
                        logRr = controlEstimates$calibratedLogRr,
                        seLogRr = controlEstimates$calibratedSeLogRr,
                        ci95Lb = controlEstimates$calibratedCi95Lb,
                        ci95Ub = controlEstimates$calibratedCi95Ub,
                        trueRr = controlEstimates$trueEffectSize))
  d <- d[!is.na(d$logRr), ]
  d <- d[!is.na(d$ci95Lb), ]
  d <- d[!is.na(d$ci95Ub), ]
  if (nrow(d) == 0) {
    return(NULL)
  }
  d$Group <- as.factor(d$trueRr)
  d$Significant <- d$ci95Lb > d$trueRr | d$ci95Ub < d$trueRr
  temp1 <- aggregate(Significant ~ Group + yGroup, data = d, length)
  temp2 <- aggregate(Significant ~ Group + yGroup, data = d, mean)
  temp1$nLabel <- paste0(formatC(temp1$Significant, big.mark = ","), " estimates")
  temp1$Significant <- NULL

  temp2$meanLabel <- paste0(formatC(100 * (1 - temp2$Significant), digits = 1, format = "f"),
                            "% of CIs include ",
                            temp2$Group)
  temp2$Significant <- NULL
  dd <- merge(temp1, temp2)
  dd$tes <- as.numeric(as.character(dd$Group))

  breaks <- c(0.1, 0.25, 0.5, 1, 2, 4, 6, 8, 10)
  theme <- ggplot2::element_text(colour = "#000000", size = 14)
  themeRA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 1)
  themeLA <- ggplot2::element_text(colour = "#000000", size = 14, hjust = 0)

  d$Group <- paste("True hazard ratio =", d$Group)
  dd$Group <- paste("True hazard ratio =", dd$Group)
  alpha <- 1 - min(0.95 * (nrow(d)/nrow(dd)/50000)^0.1, 0.95)
  plot <- ggplot2::ggplot(d, ggplot2::aes(x = logRr, y = seLogRr), environment = environment()) +
    ggplot2::geom_vline(xintercept = log(breaks), colour = "#AAAAAA", lty = 1, size = 0.5) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.025), slope = 1/qnorm(0.025)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_abline(ggplot2::aes(intercept = (-log(tes))/qnorm(0.975), slope = 1/qnorm(0.975)),
                         colour = rgb(0.8, 0, 0),
                         linetype = "dashed",
                         size = 1,
                         alpha = 0.5,
                         data = dd) +
    ggplot2::geom_point(size = size,
                        color = rgb(0, 0, 0, alpha = 0.05),
                        alpha = alpha,
                        shape = 16) +
    ggplot2::geom_hline(yintercept = 0) +
    ggplot2::geom_label(x = log(0.15),
                        y = 0.9,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = nLabel),
                        size = 5,
                        data = dd) +
    ggplot2::geom_label(x = log(0.15),
                        y = labelY,
                        alpha = 1,
                        hjust = "left",
                        ggplot2::aes(label = meanLabel),
                        size = 5,
                        data = dd) +
    ggplot2::scale_x_continuous("Hazard ratio",
                                limits = log(c(0.1, 10)),
                                breaks = log(breaks),
                                labels = breaks) +
    ggplot2::scale_y_continuous("Standard Error", limits = c(0, 1)) +
    ggplot2::facet_grid(yGroup ~ Group) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.y = themeRA,
                   axis.text.x = theme,
                   axis.title = theme,
                   legend.key = ggplot2::element_blank(),
                   strip.text.x = theme,
                   strip.text.y = theme,
                   strip.background = ggplot2::element_blank(),
                   legend.position = "none")
  return(plot)
}

renderDiagnosticsSummary <- function(diagnosticsSummary) {
  tibble(
    Diagnostic = c("Minimum detectable relative risk (MDRR)", "Time trend P", "Pre-exposure gain P", "Expected absolute systematic error (EASE)"),
    Value = c(sprintf("%0.2f", diagnosticsSummary$mdrr), sprintf("%0.2f", diagnosticsSummary$timeTrendP), sprintf("%0.2f", diagnosticsSummary$preExposureP), sprintf("%0.2f", diagnosticsSummary$ease)),
    Status = c(diagnosticsSummary$mdrrDiagnostic, diagnosticsSummary$timeTrendDiagnostic, diagnosticsSummary$preExposureDiagnostic, diagnosticsSummary$easeDiagnostic)
  ) %>%
    return()
}
