/**********************************************************************
@file SampleCases.sql

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

{DEFAULT @max_cases_per_outcome = 1000000}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }

DROP TABLE IF EXISTS #sampled_cases_per_o;

DROP TABLE IF EXISTS #sampled_cases;

SELECT observation_period_id,
	case_id,
	outcome_id
INTO #sampled_cases_per_o
FROM (
	SELECT outcomes.observation_period_id,
		outcome_id,
		case_id,
		ROW_NUMBER() OVER (PARTITION BY outcome_id ORDER BY random_id) AS rn 
	FROM (
		SELECT DISTINCT outcome_id,
			observation_period_id
{@use_nesting_cohort} ? {
		FROM #outcomes_in_nesting
} : { {@study_start_date != '' & @study_end_date != ''} ? {
		FROM #outcomes_in_period
} : {
		FROM #outcomes
}}			
	) outcomes
	INNER JOIN #cases cases
		ON outcomes.observation_period_id = cases.observation_period_id
) temp
WHERE rn <= @max_cases_per_outcome;
	
SELECT cases.observation_period_id,
	cases.case_id,
	cases.person_id,
	cases.observation_period_start_date,
	cases.start_date,
	cases.end_date,
	cases.age_in_days,
	cases.noninformative_end_censor,
	cases.gender_concept_id
INTO #sampled_cases
FROM (
	SELECT DISTINCT observation_period_id
	FROM #sampled_cases_per_o
	) sampled_ids
INNER JOIN #cases cases
	ON cases.observation_period_id = sampled_ids.observation_period_id;
