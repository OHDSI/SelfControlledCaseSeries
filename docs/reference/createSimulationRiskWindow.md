# Create a risk window definition for simulation

Create a risk window definition for simulation

## Usage

``` r
createSimulationRiskWindow(
  start = 0,
  end = 0,
  endAnchor = "era end",
  splitPoints = c(),
  relativeRisks = c(0)
)
```

## Arguments

- start:

  Start of the risk window relative to exposure start.

- end:

  The end of the risk window (in days) relative to the `endAnchor`.

- endAnchor:

  The anchor point for the end of the risk window. Can be `"era start"`
  or `"era end"`.

- splitPoints:

  Subdivision of the risk window in to smaller sub-windows.

- relativeRisks:

  Either a single number representing the relative risk in the risk
  window, or when splitPoints have been defined a vector of relative
  risks, one for each sub-window.

## Value

An object of type `SimulationRiskWindow`.
