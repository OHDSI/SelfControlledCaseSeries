/**********************************************************************
@file CreateCases.sql

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

{DEFAULT @cdm_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @outcome_database_schema = 'cdm4_sim'}
{DEFAULT @outcome_table = 'condition_occurrence'}
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @nesting_cohort_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @nesting_cohort_table = 'cohort'}
{DEFAULT @nesting_cohort_id = ''}
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }

IF OBJECT_ID('tempdb..#cases', 'U') IS NOT NULL
	DROP TABLE #cases;

SELECT observation_period_id,
	person.person_id,
	observation_period_start_date,
	start_date,
	end_date,
	DATEDIFF(DAY, DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 1), ISNULL(day_of_birth, 1)), start_date) AS age_in_days,
	NEWID() AS random_id
INTO #cases
FROM (
{@use_nesting_cohort} ? {
	SELECT person_id,
		observation_period_id,
		observation_period_start_date,
		CASE WHEN nesting.cohort_start_date > start_date THEN nesting.cohort_start_date	ELSE start_date END AS start_date,
		CASE WHEN nesting.cohort_end_date < end_date THEN nesting.cohort_end_date ELSE end_date END AS end_date
	FROM (
}
	SELECT person_id,
		observation_period_id,
		observation_period_start_date,
		observation_period_end_date,
{@study_start_date == '' } ? {
	observation_period_start_date AS start_date,
} : {
		CASE
			WHEN observation_period_start_date < CAST('@study_start_date' AS DATE)
				THEN CAST('@study_start_date' AS DATE)
			ELSE observation_period_start_date
			END AS start_date,
}
{@study_end_date == '' } ? {
	observation_period_end_date AS end_date
} : {
		CASE
			WHEN observation_period_end_date > CAST('@study_end_date' AS DATE)
				THEN CAST('@study_end_date' AS DATE)
			ELSE observation_period_end_date
			END AS end_date
}
	FROM @cdm_database_schema.observation_period
{@study_start_date != '' | @study_end_date != ''} ? {	WHERE}
{@study_start_date != '' } ? {		observation_period_end_date >= CAST('@study_start_date' AS DATE) }
{@study_start_date != '' | @study_end_date != ''} ? {		AND}
{@study_end_date != '' } ? {		observation_period_start_date < CAST('@study_end_date' AS DATE) }			
{@use_nesting_cohort} ? {
	) temp
	INNER JOIN @nesting_cohort_database_schema.@nesting_cohort_table nesting
	ON temp.person_id = nesting.subject_id
		AND temp.observation_period_start_date <= nesting.cohort_start_date
		AND temp.observation_period_end_date >= nesting.cohort_end_date
	WHERE nesting.cohort_definition_id = @nesting_cohort_id
}
) observation_period
INNER JOIN @cdm_database_schema.person
ON observation_period.person_id = person.person_id
WHERE EXISTS (
{@outcome_table == 'condition_occurrence'} ? {
		SELECT *
		FROM @outcome_database_schema.condition_occurrence outcome
		WHERE outcome.person_id = observation_period.person_id
			AND	condition_start_date <= end_date
			AND	condition_start_date >= observation_period_start_date
			AND	condition_concept_id IN (@outcome_concept_ids)
} : {
	{@outcome_table == 'condition_era'} ? {
		SELECT *
		FROM @outcome_database_schema.condition_era outcome
		WHERE outcome.person_id = observation_period.person_id
			AND	condition_era_start_date <= end_date
			AND	condition_era_start_date >= observation_period_start_date
			AND	condition_concept_id IN (@outcome_concept_ids)
	} : { /* outcome table has same structure as cohort table */
		SELECT *
		FROM @outcome_database_schema.@outcome_table outcome
		WHERE outcome.subject_id = observation_period.person_id
			AND cohort_start_date <= end_date
			AND	cohort_start_date >= observation_period_start_date
			AND	cohort_definition_id IN (@outcome_concept_ids)
	}
}
);
