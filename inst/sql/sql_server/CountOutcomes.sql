/**********************************************************************
@file CountOutcomes.sql

Copyright 2020 Observational Health Data Sciences and Informatics

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

IF OBJECT_ID('tempdb..#counts', 'U') IS NOT NULL
	DROP TABLE #counts;
	
CREATE TABLE #counts (
	outcome_id INT,
	description VARCHAR(255),
	outcome_subjects INT,
	outcome_events INT,
	outcome_obs_periods INT);
	
INSERT INTO #counts (outcome_id, description, outcome_subjects, outcome_events, outcome_obs_periods)
SELECT outcome_id, 
	'Outcomes',
	COUNT(DISTINCT person_id),
	COUNT(*),
	COUNT(DISTINCT observation_period_id)
FROM #outcomes
GROUP BY outcome_id;

{@study_start_date != '' & @study_end_date != ''} ? {
INSERT INTO #counts (outcome_id, description, outcome_subjects, outcome_events, outcome_obs_periods)
SELECT outcome_id, 
	'Outcomes in study period',
	COUNT(DISTINCT person_id),
	COUNT(*),
	COUNT(DISTINCT observation_period_id)
FROM #outcomes_in_period
GROUP BY outcome_id;
}

{@use_nesting_cohort} ? {
INSERT INTO #counts (outcome_id, description, outcome_subjects, outcome_events, outcome_obs_periods)
SELECT outcome_id, 
	'Outcomes in nesting cohort',
	COUNT(DISTINCT person_id),
	COUNT(*),
	COUNT(DISTINCT observation_period_id)
FROM #outcomes_in_nesting
GROUP BY outcome_id;
}
