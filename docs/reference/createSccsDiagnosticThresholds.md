# Create SCCS diagnostics thresholds

Threshold used when calling
[`runSccsAnalyses()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/runSccsAnalyses.md)
to determine if we pass or fail diagnostics.

## Usage

``` r
createSccsDiagnosticThresholds(
  mdrrThreshold = 10,
  easeThreshold = 0.25,
  timeTrendMaxRatio = 1.1,
  rareOutcomeMaxPrevalence = 0.1,
  eventObservationDependenceNullBounds = c(0.5, 2),
  eventExposureDependenceNullBounds = c(0.8, 1.25)
)
```

## Arguments

- mdrrThreshold:

  What is the maximum allowed minimum detectable relative risk (MDRR)?

- easeThreshold:

  What is the maximum allowed expected absolute systematic error (EASE).

- timeTrendMaxRatio:

  The maximum global ratio between the observed and expected count for
  the time stability diagnostic.

- rareOutcomeMaxPrevalence:

  The maximum allowed prevalence (proportion of people with the outcome)
  allowed when restricting to first outcome only.

- eventObservationDependenceNullBounds:

  The bounds for the null hypothesis for the incidence rate ratio of the
  end-of-observation probe window.

- eventExposureDependenceNullBounds:

  The bounds for the null hypothesis for the incidence rate of the
  pre-exposure window.

## Value

An object of type `SccsDiagnosticThresholds`.
