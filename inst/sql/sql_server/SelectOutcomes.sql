/**********************************************************************
@file SelectOutcomes.sql

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

{DEFAULT @cdm_database_schema = 'cdm_sim.dbo'}
{DEFAULT @outcome_database_schema = 'cdm_sim.dbo'}
{DEFAULT @outcome_table = 'condition_occurrence'}
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @nesting_cohort_database_schema = 'cdm_sim.dbo'}
{DEFAULT @nesting_cohort_table = 'cohort'}
{DEFAULT @nesting_cohort_id = ''}
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }

DROP TABLE IF EXISTS #outcomes;
	
SELECT outcome_id,
	observation_period_id,
	DATEDIFF(DAY, observation_period_start_date, observation_period_end_date) + 1 AS observed_days,
	outcome.person_id,
	outcome_date
INTO #outcomes
FROM @cdm_database_schema.observation_period
INNER JOIN (
{@outcome_table == 'condition_occurrence'} ? {
		SELECT person_id AS person_id,
			condition_concept_id AS outcome_id,
			condition_start_date AS outcome_date
		FROM @outcome_database_schema.condition_occurrence outcome
		WHERE condition_concept_id IN (@outcome_concept_ids)
} : {
	{@outcome_table == 'condition_era'} ? {
		SELECT person_id AS person_id,
			condition_concept_id AS outcome_id,
			condition_era_start_date AS outcome_date
		FROM @outcome_database_schema.condition_era outcome
		WHERE condition_concept_id IN (@outcome_concept_ids)
	} : { /* outcome table has same structure as cohort table */
		SELECT subject_id AS person_id,
			cohort_definition_id AS outcome_id,
			cohort_start_date AS outcome_date
		FROM @outcome_database_schema.@outcome_table outcome
		WHERE cohort_definition_id IN (@outcome_concept_ids)
	}
}
	) outcome
	ON 	outcome.person_id = observation_period.person_id
		AND outcome_date >= observation_period_start_date
		AND outcome_date <= observation_period_end_date;

{@study_start_date != '' & @study_end_date != ''} ? {
DROP TABLE IF EXISTS #outcomes_in_period;

SELECT outcome_id,
	observation_period_id,
	observed_days,
	person_id,
	outcome_date
INTO #outcomes_in_period
FROM #outcomes outcomes
WHERE
{@study_start_date != '' } ? {		outcome_date >= CAST('@study_start_date' AS DATE) }
{@study_start_date != '' | @study_end_date != ''} ? {		AND}
{@study_end_date != '' } ? {		outcome_date <= CAST('@study_end_date' AS DATE) }	
;	
}	
	
{@use_nesting_cohort} ? {
DROP TABLE IF EXISTS #outcomes_in_nesting;

SELECT outcome_id,
	observation_period_id,
	observed_days,
	person_id,
	outcome_date,
	cohort_start_date AS nesting_cohort_start_date
INTO #outcomes_in_nesting
	{@study_start_date != '' & @study_end_date != ''} ? {
FROM #outcomes_in_period outcomes
	} : {
FROM #outcomes outcomes
	}
INNER JOIN @nesting_cohort_database_schema.@nesting_cohort_table nesting
	ON person_id = subject_id
		AND outcome_date >= cohort_start_date
		AND outcome_date <= cohort_end_date
WHERE nesting.cohort_definition_id = @nesting_cohort_id;
}	
