# Plot time from event to observation end for censored and uncensored time.

Plot time from event to observation end for censored and uncensored
time.

## Usage

``` r
plotEventObservationDependence(studyPopulation, title = NULL, fileName = NULL)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- title:

  Optional: the main title for the plot

- fileName:

  Name of the file where the plot should be saved, for example
  'plot.png'. See the function
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for supported file formats.

## Value

A ggplot object. Use the
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
function to save to file in a different format.

## Details

This plot shows whether there is a difference in time between (first)
event and the observation period end for periods that are ' censored'
and those that are 'uncensored'. By 'censored' we mean periods that end
before we would normally expect. Here, we define periods to be
uncensored if they end at either the study end date (if specified),
database end date (i.e. the date after which no data is captured in the
database), or maximum age (if specified). All other periods are assumed
to be censored.

As proposed by Farrington et al., by comparing the two plots, we can
gain some insight into whether the censoring is dependent on the
occurrence of the event.

## References

Farrington P, Whitaker H, Ghebremichael Weldeselassie Y (2018),
Self-controlled case series studies: A modelling guide with R, Taylor &
Francis
