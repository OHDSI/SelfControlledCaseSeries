# Create control interval settings

Create control interval settings

## Usage

``` r
createControlIntervalSettings(
  includeEraIds = NULL,
  excludeEraIds = NULL,
  start = 0,
  startAnchor = "era start",
  end = 0,
  endAnchor = "era end",
  firstOccurrenceOnly = FALSE
)
```

## Arguments

- includeEraIds:

  One or more IDs of variables in the
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  object that should be used to construct this covariate. If no IDs are
  specified, all variables will be used.

- excludeEraIds:

  One or more IDs of variables in the \[SccsData\] object that should
  not be used to construct this covariate.

- start:

  The start of the control interval (in days) relative to the
  `startAnchor`.

- startAnchor:

  The anchor point for the start of the control interval. Can be
  `"era start"` or `"era end"`.

- end:

  The end of the control interval (in days) relative to the `endAnchor`.

- endAnchor:

  The anchor point for the end of the control interval. Can be
  `"era start"` or `"era end"`.

- firstOccurrenceOnly:

  Should only the first occurrence of the exposure be used?

## Value

An object of type `ControlIntervalSettings`.

## Details

Create an object specifying how to create a control interval for the
self-controlled risk interval (SCRI) design.
