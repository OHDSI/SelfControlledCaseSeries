/**********************************************************************
Copyright 2024 Observational Health Data Sciences and Informatics

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
{DEFAULT @has_study_periods = FALSE}
{DEFAULT @use_nesting_cohort = FALSE}
{DEFAULT @nesting_cohort_database_schema = 'cdm_sim.dbo'}
{DEFAULT @nesting_cohort_table = 'cohort'}
{DEFAULT @nesting_cohort_id = ''}

DROP TABLE IF EXISTS #cases;
	
SELECT observation_period_id,
	ROW_NUMBER() OVER (ORDER BY observation_period_id) AS case_id,
	person.person_id,
	NEWID() AS random_id,
	CASE 
		WHEN observation_period_end_date = (SELECT MAX(observation_period_end_date) FROM @cdm_database_schema.observation_period)
				THEN CAST(1 AS INT)
		ELSE CAST(0 AS INT)
	END AS noninformative_end_censor,
	observation_period_start_date,
	observation_period_start_date AS start_date,
	observation_period_end_date AS end_date,
	DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 1), ISNULL(day_of_birth, 1)) AS date_of_birth,
	gender_concept_id
INTO #cases
FROM (
	SELECT DISTINCT observation_period_id,
		observation_period.person_id,
		observation_period_start_date,
		observation_period_end_date
	FROM @cdm_database_schema.observation_period
	INNER JOIN #outcomes outcome
		ON 	outcome.person_id = observation_period.person_id
			AND outcome_date >= observation_period_start_date
			AND outcome_date <= observation_period_end_date
	) observation_period
INNER JOIN @cdm_database_schema.person
	ON observation_period.person_id = person.person_id;
		
{@has_study_periods} ? {
DROP TABLE IF EXISTS #cases_in_periods;

SELECT observation_period_id,
	ROW_NUMBER() OVER (ORDER BY observation_period_id) AS case_id,
	person_id,
	random_id,
	CASE 
		WHEN study_end_date < end_date THEN CAST(1 AS INT)
		ELSE noninformative_end_censor
	END AS noninformative_end_censor,
	observation_period_start_date,
	CASE 
		WHEN study_start_date > start_date THEN study_start_date
		ELSE start_date
	END AS start_date,
	CASE 
		WHEN study_end_date < end_date THEN study_end_date 
		ELSE end_date
	END AS end_date,
	date_of_birth,
	gender_concept_id
INTO #cases_in_periods
FROM #cases cases
INNER JOIN #study_periods
	ON end_date >= study_start_date
	 AND start_date <= study_end_date;
}
	
{@use_nesting_cohort} ? {
DROP TABLE IF EXISTS #cases_in_nesting;

SELECT observation_period_id,
	ROW_NUMBER() OVER (ORDER BY observation_period_id) AS case_id,
	person_id,
	random_id,
	CASE 
		WHEN cohort_end_date < end_date THEN CAST(1 AS INT)
		ELSE noninformative_end_censor
	END AS noninformative_end_censor,
	observation_period_start_date,
	CASE 
		WHEN cohort_start_date > start_date THEN cohort_start_date
		ELSE start_date
	END AS start_date,
	CASE 
		WHEN cohort_end_date < end_date THEN cohort_end_date
		ELSE end_date
	END AS end_date,
	date_of_birth,
	gender_concept_id
INTO #cases_in_nesting
{@has_study_periods} ? {
FROM #cases_in_periods cases
} : {
FROM #cases cases
}
INNER JOIN @nesting_cohort_database_schema.@nesting_cohort_table nesting
	ON person_id = subject_id
		AND end_date >= cohort_start_date
		AND start_date <= cohort_end_date
WHERE nesting.cohort_definition_id = @nesting_cohort_id;
}
	
