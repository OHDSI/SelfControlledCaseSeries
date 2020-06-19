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

