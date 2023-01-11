/**********************************************************************
@file CountOutcomes.sql

Copyright 2023 Observational Health Data Sciences and Informatics

This file is part of SelfControlledCaseSeries

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
***********************************************************************/
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @study_start_date = ''}
{DEFAULT @study_end_date = ''}

DROP TABLE IF EXISTS #counts;

CREATE TABLE #counts (
	outcome_id INT,
	description VARCHAR(255),
	outcome_subjects INT,
	outcome_events INT,
	outcome_obs_periods INT,
	observed_days BIGINT);

INSERT INTO #counts (outcome_id, description, outcome_subjects, outcome_events, outcome_obs_periods, observed_days)
SELECT outcome_ids.outcome_id,
	'All outcome occurrences',
	CASE WHEN outcome_subjects IS NULL THEN 0 ELSE outcome_subjects END AS outcome_subjects,
	CASE WHEN outcome_events IS NULL THEN 0 ELSE outcome_events END AS outcome_events,
	CASE WHEN outcome_obs_periods IS NULL THEN 0 ELSE outcome_obs_periods END AS outcome_obs_periods,
	CASE WHEN observed_days IS NULL THEN 0 ELSE observed_days END AS observed_days
FROM #outcome_ids outcome_ids
LEFT JOIN (
	SELECT outcome_id,
		COUNT(DISTINCT person_id) AS outcome_subjects,
		COUNT(*) AS outcome_events,
		COUNT(DISTINCT observation_period_id) AS outcome_obs_periods,
		SUM(observed_days) AS observed_days
	FROM #outcomes
	GROUP BY outcome_id
	) counts
ON outcome_ids.outcome_id = counts.outcome_id;

{@study_start_date != '' & @study_end_date != ''} ? {
INSERT INTO #counts (outcome_id, description, outcome_subjects, outcome_events, outcome_obs_periods, observed_days)
SELECT outcome_ids.outcome_id,
	'Outcomes in study period',
	CASE WHEN outcome_subjects IS NULL THEN 0 ELSE outcome_subjects END AS outcome_subjects,
	CASE WHEN outcome_events IS NULL THEN 0 ELSE outcome_events END AS outcome_events,
	CASE WHEN outcome_obs_periods IS NULL THEN 0 ELSE outcome_obs_periods END AS outcome_obs_periods,
	CASE WHEN observed_days IS NULL THEN 0 ELSE observed_days END AS observed_days
FROM #outcome_ids outcome_ids
LEFT JOIN (
	SELECT outcome_id,
		COUNT(DISTINCT person_id) AS outcome_subjects,
		COUNT(*) AS outcome_events,
		COUNT(DISTINCT observation_period_id) AS outcome_obs_periods,
		SUM(observed_days) AS observed_days
	FROM #outcomes_in_period
	GROUP BY outcome_id
	) counts
ON outcome_ids.outcome_id = counts.outcome_id;
}

{@use_nesting_cohort} ? {
INSERT INTO #counts (outcome_id, description, outcome_subjects, outcome_events, outcome_obs_periods, observed_days)
SELECT outcome_ids.outcome_id,
	'Outcomes in nesting cohort',
	CASE WHEN outcome_subjects IS NULL THEN 0 ELSE outcome_subjects END AS outcome_subjects,
	CASE WHEN outcome_events IS NULL THEN 0 ELSE outcome_events END AS outcome_events,
	CASE WHEN outcome_obs_periods IS NULL THEN 0 ELSE outcome_obs_periods END AS outcome_obs_periods,
	CASE WHEN observed_days IS NULL THEN 0 ELSE observed_days END AS observed_days
FROM #outcome_ids outcome_ids
LEFT JOIN (
	SELECT outcome_id,
		COUNT(DISTINCT person_id) AS outcome_subjects,
		COUNT(*) AS outcome_events,
		COUNT(DISTINCT observation_period_id) AS outcome_obs_periods,
		SUM(observed_days) AS observed_days
	FROM #outcomes_in_nesting
	GROUP BY outcome_id
	) counts
ON outcome_ids.outcome_id = counts.outcome_id;
}
