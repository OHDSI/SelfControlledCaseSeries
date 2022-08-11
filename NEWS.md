SelfControlledCaseSeries 3.4.0
==============================

Changes

1. Setting the `deleteCovariatesSmallCount` argument to 0 in the `getDbSccsData()` and `createGetDbSccsDataArgs()` functions. This was originally set to 100 for SCCS analyses with many covariates, but makes no sense when considering only one or a few exposures.

2. Adding the `computePreExposureGainP()` function.

3. Setting deffault `maxRatio` in `computeTimeStability()` to 1.25 (instead of 1.1) to avoid false positives.


Bug fixes

1. Fixing "RIGHT and FULL OUTER JOINs are not currently supported" error on SQLite when using `drug_era` or `condition_era` table.

2. Fixing `computeTimeStability()` per-month p-value (was showing the minimum p-value for all months).


SelfControlledCaseSeries 3.3.0
==============================

Changes

1. Setting the default Cyclops control object to use `resetCoefficients = TRUE` to ensure we always get the exact same model, irrespective of the number of threads used.

2. Added `maxRatio` argument to `computeTimeStability()`.


Bug fixes

1. Fixing missing months in `plotEventToCalendarTime()` when there are no observation period starts and ends in those months.

2. Now limiting the number of unexposed cases when including only a calendar time spline (as set by `minCasesForTimeCovariates`).

3. Fix error when calling `computeMdrr()` on an `sccsIntervalData` object that does not contain the exposure of interest.

4. Fixed typo in `computeMdrr()`, renaming `propPopExposued ` to `propPopulationExposed`.


SelfControlledCaseSeries 3.2.1
==============================

BugFixes

1. Remove dependency on develop branch of `SqlRender`.


SelfControlledCaseSeries 3.2.0
==============================

Changes

1. Adding optional calendar time covariate.

2. Added the `analysesToExclude` argument to `runSccsAnalyses()`, allowing the users to specify exposure-outcome-analysis combinations to exclude from execution.

3. Fixing seed for regularization cross-validation to improve reproducibility.

4. Added the `computeTimeStability()` function.


SelfControlledCaseSeries 3.1.0
==============================

Changes

1. Adding likelihood profile to SCCS model objects if `profileLikelihood` argument is set to `TRUE` when calling `createEraCovariateSettings()`.

2. Deprecating `oracleTempSchema` argument in favor of `tempEmulationSchema` in accordance with new `SqlRender` convention.

3. Adding optional `title` argument to all plotting functions.

4. Adding `highlightExposedEvents` argument to `plotExposureCentered` function.

5. Switching power calculation default method to signed root likelihood ratio as recommended by Musonda et al. (2005).

BugFixes

1. Prevent error when excluding variable not in data from regularization.

2. Removing unexposed subjects when computing power to avoid overestimating statistical power.


SelfControlledCaseSeries 3.0.0
==============================

Changes

1. Adding the self-controlled risk interval design.

2. Downloading person and observation period IDs as strings to avoid issues with 64-bit integers. (These IDs are not used by SCCS, and are used for reference only).

3. Outputting log likelihood ratio as part of estimates.

4. Computing meta-data on covariates.

BugFixes

1. Fixed syntax error in SQL when using a nesting cohort.

2. Fixing error when sampled cohort is empty.

3. Fixing nesting.

4. Attrition table now also includes lines where remaining count is zero.

5. Fixing custom covariates download.

6. Fixing error on Oracle due to long temp table name.

7. Fixing computation of confidence intervals (CIs) when not all estimates for which CIs are computed have data.


SelfControlledCaseSeries 2.0.0
==============================

Changes

1. Switching from ff to Andromeda for storing large data objects.

2. Making creation of the study population a separate step, with the new `createStudyPopulation` function.

3. The data on cohorts, exposure, and outcome eras retrieved from the database is now consistently referred to as 'eras'. Data transformed to non-overlapping intervals is now referred to as 'sccsIntervalData'.

4. Adding tracking of attrition.

5. Automatically removing age spline if selected censoring model already adjusts for age.

BugFixes

1. Generating sequential case IDs instead of observation period IDs to avoid collisions due to loss of precision when converting BIGINT to R's numeric.

2. Added more heuristics to detect ill-behaving censoring functions when adjusting for event-dependent censoring.


SelfControlledCaseSeries 1.4.2
==============================

Bugfixes

1. Fixed errors introduced by R 4.0.0.

2. Gracefully handling when fitting the outcome model hits the max number of iterations.


SelfControlledCaseSeries 1.4.1
==============================

Bugfixes

1. Several workaround for issues with the ff package.

2. Fixed bug causing age to be read incorrectly when creating eras.

