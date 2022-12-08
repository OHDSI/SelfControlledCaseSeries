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
      strip.text.y = theme,
      strip.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.text = theme
    )
  return(plot)
}
