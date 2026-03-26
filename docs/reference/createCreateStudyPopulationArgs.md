# Create a parameter object for the [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md) function

Create a parameter object for the
[`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
function

## Usage

``` r
createCreateStudyPopulationArgs(
  firstOutcomeOnly = FALSE,
  naivePeriod = 0,
  minAge = NULL,
  maxAge = NULL,
  genderConceptIds = NULL,
  restrictTimeToEraId = NULL
)
```

## Arguments

- firstOutcomeOnly:

  Whether only the first occurrence of an outcome should be considered.

- naivePeriod:

  The number of days at the start of a patient's observation period that
  should not be included in the risk calculations. Note that the naive
  period can be used to determine current covariate status right after
  the naive period, and whether an outcome is the first one.

- minAge:

  Minimum age at which patient time will be included in the analysis.
  Note that information prior to the min age is still used to determine
  exposure status after the minimum age (e.g. when a prescription was
  started just prior to reaching the minimum age). Also, outcomes
  occurring before the minimum age is reached will be considered as
  prior outcomes when using first outcomes only. Age should be specified
  in years, but non-integer values are allowed. If not specified, no age
  restriction will be applied.

- maxAge:

  Maximum age at which patient time will be included in the analysis.
  Age should be specified in years, but non-integer values are allowed.
  If not specified, no age restriction will be applied.

- genderConceptIds:

  Set of gender concept IDs to restrict the population to. If not
  specified, no restriction on gender will be applied.

- restrictTimeToEraId:

  If provided, study time (for all patients) will be restricted to the
  calendar time when that era was observed in the data. For example, if
  the era ID refers to a drug, study time will be restricted to when the
  drug was on the market.

## Value

An object of type `CreateStudyPopulationArgs`.

## Details

Create an object defining the parameter values.
