# Create SCCS simulation settings

Create SCCS simulation settings

## Usage

``` r
createSccsSimulationSettings(
  meanPatientTime = 4 * 365,
  sdPatientTime = 2 * 365,
  minAge = 18 * 365,
  maxAge = 65 * 365,
  minBaselineRate = 0.001,
  maxBaselineRate = 0.01,
  minCalendarTime = as.Date("2000-01-01"),
  maxCalendarTime = as.Date("2010-01-01"),
  eraIds = c(1, 2),
  patientUsages = c(0.2, 0.1),
  usageRate = c(0.01, 0.01),
  usageRateSlope = c(0, 0),
  meanPrescriptionDurations = c(14, 30),
  sdPrescriptionDurations = c(7, 14),
  simulationRiskWindows = list(createSimulationRiskWindow(relativeRisks = 1),
    createSimulationRiskWindow(relativeRisks = 1.5)),
  includeAgeEffect = TRUE,
  ageKnots = 5,
  includeSeasonality = TRUE,
  seasonKnots = 5,
  includeCalendarTimeEffect = TRUE,
  calendarTimeKnots = 5,
  calendarTimeMonotonic = FALSE,
  outcomeId = 10
)
```

## Arguments

- meanPatientTime:

  Mean number of observation days per patient.

- sdPatientTime:

  Standard deviation of the observation days per patient.

- minAge:

  The minimum age in days.

- maxAge:

  The maximum age in days.

- minBaselineRate:

  The minimum baseline rate (per day).

- maxBaselineRate:

  The maximum baseline rate (per day).

- minCalendarTime:

  The minimum date patients are to be observed.

- maxCalendarTime:

  The maximum date patients are to be observed.

- eraIds:

  The IDs for the covariates to be generated.

- patientUsages:

  The fraction of patients that use the drugs.

- usageRate:

  The rate of prescriptions per person that uses the drug.

- usageRateSlope:

  The change in the usage rate from one day to the next. `usageRate` is
  the intercept at day 0

- meanPrescriptionDurations:

  The mean duration of a prescription, per drug.

- sdPrescriptionDurations:

  The standard deviation of the duration of a prescription, per drug.

- simulationRiskWindows:

  One or a list of objects of type `SimulationRiskWindow` as created
  using the
  [`createSimulationRiskWindow()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createSimulationRiskWindow.md)
  function. function.

- includeAgeEffect:

  Include an age effect for the outcome?

- ageKnots:

  Number of knots in the age spline.

- includeSeasonality:

  Include seasonality for the outcome?

- seasonKnots:

  Number of knots in the seasonality spline.

- includeCalendarTimeEffect:

  Include a calendar time effect for the outcome?

- calendarTimeKnots:

  Number of knots in the calendar time spline.

- calendarTimeMonotonic:

  Should the calender time effect be monotonic?

- outcomeId:

  The ID to be used for the outcome.

## Value

An object of type `SccsSimulationSettings`.

## Details

Create an object of settings for an SCCS simulation.
