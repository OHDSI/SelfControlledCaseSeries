# Create Self-Controlled Risk Interval (SCRI) era data

Create Self-Controlled Risk Interval (SCRI) era data

## Usage

``` r
createScriIntervalData(studyPopulation, sccsData, createScriIntervalDataArgs)
```

## Arguments

- studyPopulation:

  An object created using the
  [`createStudyPopulation()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createStudyPopulation.md)
  function.

- sccsData:

  An object of type
  [SccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsData-class.md)
  as created using the
  [getDbSccsData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/getDbSccsData.md)
  function.

- createScriIntervalDataArgs:

  An object of type `CreateScriIntervalDataArgs` as created by the
  [`createCreateScriIntervalDataArgs()`](https://ohdsi.github.io/SelfControlledCaseSeries/reference/createCreateScriIntervalDataArgs.md)
  function.

## Value

An object of type
[SccsIntervalData](https://ohdsi.github.io/SelfControlledCaseSeries/reference/SccsIntervalData-class.md).

## Details

This function creates interval data according to the elf-Controlled Risk
Interval (SCRI) design. Unlike the generic SCCS design, where all
patient time is used to establish a background rate, in the SCRI design
a specific control interval (relative to the exposure) needs to be
defined. The final model will only include time that is either part of
the risk interval (defined using the `eraCovariateSettings` argument, or
the control interval (defined using `controlIntervalSettings`).

## References

Greene SK, Kulldorff M, Lewis EM, Li R, Yin R, Weintraub ES, Fireman BH,
Lieu TA, Nordin JD, Glanz JM, Baxter R, Jacobsen SJ, Broder KR, Lee GM.
Near real-time surveillance for influenza vaccine safety:
proof-of-concept in the Vaccine Safety Datalink Project. Am J Epidemiol.
2010 Jan 15;171(2):177-88. doi: 10.1093/aje/kwp345.
