/**********************************************************************
@file CreateCases.sql

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

{DEFAULT @cdm_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @nesting_cohort_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @nesting_cohort_table = 'cohort'}
{DEFAULT @nesting_cohort_id = ''}
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }

DROP TABLE IF EXISTS #cases;
	
SELECT observation_period_id,
	ROW_NUMBER() OVER (ORDER BY observation_period_id) AS case_id,
	person.person_id,
	observation_period_start_date,
	start_date,
	end_date,
	DATEDIFF(DAY, DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 1), ISNULL(day_of_birth, 1)), start_date) AS age_in_days,
	noninformative_end_censor,
	NEWID() AS random_id,
	person.gender_concept_id
INTO #cases
FROM (
{@use_nesting_cohort} ? {
	SELECT person_id,
		observation_period_id,
		observation_period_start_date,
		CASE WHEN nesting.cohort_start_date > start_date THEN nesting.cohort_start_date	ELSE start_date END AS start_date,
		CASE WHEN nesting.cohort_end_date < end_date THEN nesting.cohort_end_date ELSE end_date END AS end_date,
		CASE WHEN nesting.cohort_end_date < end_date THEN CAST(1 AS INT) ELSE noninformative_end_censor END AS noninformative_end_censor
	FROM (
}
	SELECT person_id,
		observation_period_id,
		observation_period_start_date,
		observation_period_end_date,
{@use_nesting_cohort} ? {
		nesting_cohort_start_date,
}
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
		observation_period_end_date AS end_date,
		noninformative_end_censor
} : {
		CASE
			WHEN CAST('@study_end_date' AS DATE) < observation_period_end_date
				THEN CAST('@study_end_date' AS DATE)
			ELSE observation_period_end_date
		END AS end_date,
		CASE WHEN CAST('@study_end_date' AS DATE) < observation_period_end_date 
			THEN CAST(1 AS INT) 
			ELSE noninformative_end_censor
		END AS noninformative_end_censor
}

	FROM (
		SELECT person_id,
			observation_period.observation_period_id,
			observation_period_start_date,
			observation_period_end_date,
{@use_nesting_cohort} ? {
			nesting_cohort_start_date,
}
			CASE WHEN observation_period_end_date = (SELECT MAX(observation_period_end_date) FROM @cdm_database_schema.observation_period)
				THEN CAST(1 AS INT)
				ELSE CAST(0 AS INT)
			END AS noninformative_end_censor
		FROM @cdm_database_schema.observation_period
		INNER JOIN (
			SELECT DISTINCT observation_period_id 
{@use_nesting_cohort} ? {
				, nesting_cohort_start_date
			FROM #outcomes_in_nesting
} : { {@study_start_date != '' & @study_end_date != ''} ? {
			FROM #outcomes_in_period
} : {
			FROM #outcomes
}}	
			) outcomes
		ON observation_period.observation_period_id = outcomes.observation_period_id
	) observation_period
{@use_nesting_cohort} ? {
	) temp
	INNER JOIN @nesting_cohort_database_schema.@nesting_cohort_table nesting
		ON temp.person_id = nesting.subject_id
			AND temp.nesting_cohort_start_date = nesting.cohort_start_date
			AND temp.observation_period_start_date <= nesting.cohort_start_date
			AND temp.observation_period_end_date >= nesting.cohort_start_date
	WHERE nesting.cohort_definition_id = @nesting_cohort_id
}
) observation_period
INNER JOIN @cdm_database_schema.person
	ON observation_period.person_id = person.person_id
;
	
