# Create a parameter object for the [`createSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md) function

Create a parameter object for the
[`createSccsIntervalData()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSccsIntervalData.md)
function

## Usage

``` r
createCreateSccsIntervalDataArgs(
  eraCovariateSettings,
  ageCovariateSettings = NULL,
  seasonalityCovariateSettings = NULL,
  calendarTimeCovariateSettings = NULL,
  minCasesForTimeCovariates = 10000,
  endOfObservationEraLength = 30,
  eventDependentObservation = FALSE
)
```

## Arguments

- eraCovariateSettings:

  Either an object of type `EraCovariateSettings` as created using the
  [`createEraCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createEraCovariateSettings.md)
  function, or a list of such objects.

- ageCovariateSettings:

  An object of type `AgeCovariateSettings` as created using the
  [`createAgeCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createAgeCovariateSettings.md)
  function.

- seasonalityCovariateSettings:

  An object of type `SeasonalityCovariateSettings` as created using the
  [`createSeasonalityCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSeasonalityCovariateSettings.md)
  function.

- calendarTimeCovariateSettings:

  An object of type `CalendarTimeCovariateSettings` as created using the
  [`createCalendarTimeCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCalendarTimeCovariateSettings.md)
  function.

- minCasesForTimeCovariates:

  Minimum number of cases to use to fit age, season and calendar time
  splines. If needed (and available), cases that are not exposed will be
  included.

- endOfObservationEraLength:

  Length in days of the probe that is inserted at the end of a patient's
  observation time. This probe will be used to test whether there is
  event- dependent observation end. Set to 0 to not include the probe.

- eventDependentObservation:

  Should the extension proposed by Farrington et al. be used to adjust
  for event-dependent observation time?

## Value

An object of type `CreateSccsIntervalDataArgs`.

## Details

Create an object defining the parameter values.

## References

Farrington, C. P., Anaya-Izquierdo, A., Whitaker, H. J., Hocine, M.N.,
Douglas, I., and Smeeth, L. (2011). Self-Controlled case series analysis
with event-dependent observation periods. Journal of the American
Statistical Association 106 (494), 417-426
