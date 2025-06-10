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

SELECT COUNT(*) AS window_count,
  SUM(CAST(DATEDIFF(DAY, start_date, end_date) + 1 AS BIGINT)) AS days_count
FROM (
{@use_nesting_cohort} ? {
  SELECT person_id,
  	CASE
  		WHEN cohort_start_date > start_date THEN cohort_start_date
  		ELSE start_date
  	END AS start_date,
  	CASE
  		WHEN cohort_end_date < end_date THEN cohort_end_date
  		ELSE end_date
  	END AS end_date
  FROM (
}
{@has_study_periods} ? {
    SELECT person_id,
    	CASE
    		WHEN study_start_date > start_date THEN study_start_date
    		ELSE start_date
    	END AS start_date,
    	CASE
    		WHEN study_end_date < end_date THEN study_end_date
    		ELSE end_date
    	END AS end_date
    	FROM (
}
        SELECT observation_period_id,
        	person_id,
        	observation_period_start_date AS start_date,
        	observation_period_end_date AS end_date
        FROM @cdm_database_schema.observation_period
{@has_study_periods} ? {
      ) tmp
      INNER JOIN #study_periods
	      ON end_date >= study_start_date
	        AND start_date <= study_end_date
}
{@use_nesting_cohort} ? {
    ) tmp
    INNER JOIN @nesting_cohort_database_schema.@nesting_cohort_table nesting
    	ON person_id = subject_id
    		AND end_date >= cohort_start_date
    		AND start_date <= cohort_end_date
    WHERE nesting.cohort_definition_id = @nesting_cohort_id
}
) tmp;
