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
