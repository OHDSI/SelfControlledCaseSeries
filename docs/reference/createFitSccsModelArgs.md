# Create a parameter object for the function fitSccsModel

Create a parameter object for the function fitSccsModel

## Usage

``` r
createFitSccsModelArgs(
  prior = createPrior("laplace", useCrossValidation = TRUE),
  control = createControl(cvType = "auto", selectorType = "byPid", startingVariance =
    0.1, seed = 1, resetCoefficients = TRUE, noiseLevel = "quiet"),
  profileGrid = NULL,
  profileBounds = c(log(0.1), log(10))
)
```

## Arguments

- prior:

  The prior used to fit the model. See Cyclops::createPrior for details.

- control:

  The control object used to control the cross-validation used to
  determine the hyperparameters of the prior (if applicable). See
  Cyclops::createControl for details.

- profileGrid:

  A one-dimensional grid of points on the log(relative risk) scale where
  the likelihood for coefficient of variables is sampled. See details.

- profileBounds:

  The bounds (on the log relative risk scale) for the adaptive sampling
  of the likelihood function.

## Value

An object of type `FitSccsModelArgs`.

## Details

Create an object defining the parameter values.

Likelihood profiling is only done for variables for which
`profileLikelihood` is set to `TRUE` when calling
[`createEraCovariateSettings()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createEraCovariateSettings.md).
Either specify the `profileGrid` for a completely user- defined grid, or
`profileBounds` for an adaptive grid. Both should be defined on the log
IRR scale. When both `profileGrid` and `profileGrid` are `NULL`
likelihood profiling is disabled.

To make use of the more efficient Hermite interpolation in evidence
synthesis, set `profileGrid = seq(log(0.1), log(10), length.out = 8)`
and `profileBounds = NULL`.
