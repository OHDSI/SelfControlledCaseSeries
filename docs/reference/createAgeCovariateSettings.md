# Create age covariate settings

Create age covariate settings

## Usage

``` r
createAgeCovariateSettings(
  ageKnots = 5,
  allowRegularization = FALSE,
  computeConfidenceIntervals = FALSE
)
```

## Arguments

- ageKnots:

  If a single number is provided this is assumed to indicate the number
  of knots to use for the spline, and the knots are automatically spaced
  according to equal percentiles of the data. If more than one number is
  provided these are assumed to be the exact location of the knots in
  age-days

- allowRegularization:

  When fitting the model, should the covariates defined here be allowed
  to be regularized?

- computeConfidenceIntervals:

  Should confidence intervals be computed for the covariates defined
  here? Setting this to FALSE might save computing time when fitting the
  model. Will be turned to FALSE automatically when
  `allowRegularization = TRUE`.

## Value

An object of type `AgeCovariateSettings`.

## Details

Create an object specifying whether and how age should be included in
the model. Age can be included by splitting patient time into calendar
months. During a month, the relative risk attributed to age is assumed
to be constant, and the risk from month to month is modeled using a
quadratic spline.
