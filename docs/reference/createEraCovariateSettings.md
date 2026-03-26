# Create era covariate settings

Create era covariate settings

## Usage

``` r
createEraCovariateSettings(
  includeEraIds,
  excludeEraIds = NULL,
  label = "Covariates",
  stratifyById = FALSE,
  start = 0,
  startAnchor = "era start",
  end = 0,
  endAnchor = "era end",
  firstOccurrenceOnly = FALSE,
  allowRegularization = FALSE,
  profileLikelihood = FALSE,
  exposureOfInterest = FALSE,
  preExposure = start < 0
)
```

## Arguments

- includeEraIds:

  One or more IDs of variables in the
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  object that should be used to construct this covariate. If set to
  NULL, all variables will be used.

- excludeEraIds:

  One or more IDs of variables in the \[SccsData\] object that should
  not be used to construct this covariate.

- label:

  A label used to identify the covariates created using these settings.

- stratifyById:

  Should a single covariate be created for every ID in the
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  object, or should a single covariate be constructed? For example, if
  the IDs identify exposures to different drugs, should a covariate be
  constructed for every drug, or a single covariate for exposure to any
  of these drugs. Note that overlap will be considered a single
  exposure.

- start:

  The start of the risk window (in days) relative to the `startAnchor`.

- startAnchor:

  The anchor point for the start of the risk window. Can be
  `"era start"` or `"era end"`.

- end:

  The end of the risk window (in days) relative to the `endAnchor`.

- endAnchor:

  The anchor point for the end of the risk window. Can be `"era start"`
  or `"era end"`.

- firstOccurrenceOnly:

  Should only the first occurrence of the exposure be used?

- allowRegularization:

  When fitting the model, should the covariates defined here be allowed
  to be regularized?

- profileLikelihood:

  When fitting the model, should the likelihood profile be computed for
  the covariate defined here? The likelihood profile can be used to
  avoid making normal approximations on the likelihood and can be used
  in methods specifically designed to make use of the profile, but may
  take a while to compute.

- exposureOfInterest:

  If TRUE, the fitted coefficient for this variable will be reported
  when using
  [`runSccsAnalyses()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/runSccsAnalyses.md).
  Requires `includeEraIds` to be a exposure reference ID as defined in
  [`createExposure()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createExposure.md).

- preExposure:

  If TRUE, this variable will be used for the pre-exposure diagnostic.

## Value

An object of type `EraCovariateSettings`.

## Details

Create an object specifying how to create a (set of) era-based
covariates.
