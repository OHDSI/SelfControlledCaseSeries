# Check if rare outcome assumption is violated

Check if rare outcome assumption is violated

## Usage

``` r
checkRareOutcomeAssumption(
  studyPopulation,
  firstOutcomeOnly = NULL,
  maxPrevalence = 0.1
)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- firstOutcomeOnly:

  Was the analysis restricted to the first outcome only? If left at
  NULL, will be determined by whether `firstOutcomeOnly` was set to
  `TRUE` when calling
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  or whether each person only had one outcome when pulling the data from
  the server.

- maxPrevalence:

  The maximum allowed prevalence (proportion of people with the outcome)
  allowed when restricting to first outcome only.

## Value

A tibble with one row and three columns: `outcomeProportion` indicates
the proportion of people having the outcome at least once.
`firstOutcomeOnly` indicated whether the analysis was restricted to the
first outcome only. `rare` is TRUE if the rare outcome assumption is
met, or the analysis was not restricted to the first outcome.

## Details

Most SCCS analyses restrict to the first outcome occurrence per person
to avoid violating the assumption that subsequent occurrences are
independent. This is fine, as long as the outcome is rare. According to
Farrington et al., the magnitude of the bias from violating this
assumption is 0.5p, where p is the prevalence. By default we set the
threshold for p at 0.1, corresponding to at most 5 percent bias.

The prevalence was computed in the
[`getDbSccsData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
function, within the population defined by the `observation_period`
table, and restricted to the study period(s) and nesting cohort if used.

## References

Farrington P, Whitaker H, Ghebremichael-Weldeselassie Y, Self-Controlled
Case Series Studies: A Modelling Guide with R, CRC Press, 2018
