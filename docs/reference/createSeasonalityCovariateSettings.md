# Create seasonality settings

Create seasonality settings

## Usage

``` r
createSeasonalityCovariateSettings(
  seasonKnots = 5,
  allowRegularization = FALSE,
  computeConfidenceIntervals = FALSE
)
```

## Arguments

- seasonKnots:

  If a single number is provided this is assumed to indicate the number
  of knots to use for the spline, and the knots are automatically
  equally spaced across the year. If more than one number is provided
  these are assumed to be the exact location of the knots in days
  relative to the start of the year.

- allowRegularization:

  When fitting the model, should the covariates defined here be allowed
  to be regularized?

- computeConfidenceIntervals:

  Should confidence intervals be computed for the covariates defined
  here? Setting this to FALSE might save computing time when fitting the
  model. Will be turned to FALSE automatically when
  `allowRegularization = TRUE`.

## Value

An object of type `SeasonalityCovariateSettings`.

## Details

Create an object specifying whether and how seasonality should be
included in the model. Seasonality can be included by splitting patient
time into calendar months. During a month, the relative risk attributed
to season is assumed to be constant, and the risk from month to month is
modeled using a cyclic quadratic spline.
