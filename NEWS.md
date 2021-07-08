SelfControlledCaseSeries 3.1.0
==============================

Changes

1. Adding likelihood profile to SCCS model objects if `profileLikelihood` argument is set to `TRUE` when calling `createEraCovariateSettings()`.

2. Deprecating `oracleTempSchema` argument in favor of `tempEmulationSchema` in accordance with new `SqlRender` convention.

3. Adding optional `title` argument to all plotting functions.

4. Adding `highlightExposedEvents` argument to `plotExposureCentered` function.

BugFixes

1. Prevent error when excluding variable not in data from regularization.


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

