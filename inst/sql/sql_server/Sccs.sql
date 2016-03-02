/**********************************************************************
@file Sccs.sql

Copyright 2016 Observational Health Data Sciences and Informatics

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

{DEFAULT @cdm_database = 'cdm4_sim.dbo'}
{DEFAULT @outcome_database_schema = 'cdm4_sim'} 
{DEFAULT @outcome_table = 'condition_occurrence'} 
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @outcome_condition_type_concept_ids = ''}  
{DEFAULT @exposure_database_schema = 'cdm4_sim.dbo'} 
{DEFAULT @exposure_table = 'drug_era'} 
{DEFAULT @use_custom_covariates = FALSE}
{DEFAULT @custom_covariate_database_schema = 'cdm4_sim.dbo'} 
{DEFAULT @custom_covariate_table = 'drug_era'} 
{DEFAULT @has_exposure_ids = FALSE} 
{DEFAULT @has_custom_covariate_ids = FALSE} 
{DEFAULT @delete_covariates_small_count = 100}
{DEFAULT @cdm_version = '4'}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'} 

USE @cdm_database;

IF OBJECT_ID('tempdb..#cases', 'U') IS NOT NULL
	DROP TABLE #cases;
	
IF OBJECT_ID('tempdb..#eras', 'U') IS NOT NULL
	DROP TABLE #eras;
	
IF OBJECT_ID('tempdb..#covariate_ref', 'U') IS NOT NULL
	DROP TABLE #covariate_ref;
	
/**********************************************************************
							Select cases
***********************************************************************/
SELECT observation_period_id,
	person.person_id,
	observation_period_start_date,
	observation_period_end_date,
	DATEDIFF(dd, DATEFROMPARTS(year_of_birth, ISNULL(month_of_birth, 1), ISNULL(day_of_birth, 1)), observation_period_start_date) AS age_in_days
INTO #cases
FROM observation_period
INNER JOIN person
ON observation_period.person_id = person.person_id
WHERE EXISTS (
{@outcome_table == 'condition_occurrence'} ? {	
		SELECT *
		FROM @outcome_database_schema.condition_occurrence outcome
		WHERE outcome.person_id = observation_period.person_id
			AND	condition_start_date <= observation_period_end_date
			AND	condition_start_date >= observation_period_start_date	
			AND	condition_concept_id IN (@outcome_concept_ids)
	{@outcome_condition_type_concept_ids != ''} ? {
			AND condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	} 			
} : { 
	{@outcome_table == 'condition_era'} ? {
		SELECT *
		FROM @outcome_database_schema.condition_era outcome
		WHERE outcome.person_id = observation_period.person_id
			AND	condition_era_start_date <= observation_period_end_date
			AND	condition_era_start_date >= observation_period_start_date	
			AND	condition_concept_id IN (@outcome_concept_ids)
	} : { /* outcome table has same structure as cohort table */
		SELECT *
		FROM @outcome_database_schema.@outcome_table outcome
		WHERE outcome.subject_id = observation_period.person_id
			AND cohort_start_date <= observation_period_end_date
			AND	cohort_start_date >= observation_period_start_date	
			AND	@cohort_definition_id IN (@outcome_concept_ids)
	}
}
	);

/**********************************************************************
							Create eras
***********************************************************************/
CREATE TABLE #eras (
	era_type VARCHAR(3),
	observation_period_id INT,
	concept_id INT,
	era_value FLOAT,
	start_day INT,
	end_day INT
);

/* Create exposure eras */
{@exposure_table == 'drug_era'} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'hei',
	cases.observation_period_id,
	drug_concept_id,
	1, 
	DATEDIFF(dd, observation_period_start_date, drug_era_start_date),
	DATEDIFF(dd, observation_period_start_date, drug_era_end_date)
FROM @exposure_database_schema.drug_era
INNER JOIN #cases cases
ON drug_era.person_id = cases.person_id
	AND drug_era_start_date <= observation_period_end_date
	AND drug_era_end_date >= observation_period_start_date
{@has_exposure_ids} ? {
WHERE
	drug_concept_id IN (SELECT concept_id FROM #exposure_ids)
}
;  	
} : { /* exposure table has same structure as cohort table */
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'hei',
	cases.observation_period_id,
	@cohort_definition_id,
	1,
	DATEDIFF(dd, observation_period_start_date, cohort_start_date),
	DATEDIFF(dd, observation_period_start_date, cohort_end_date)
FROM @exposure_database_schema.@exposure_table exposure
INNER JOIN #cases cases
ON exposure.subject_id = cases.person_id
	AND cohort_start_date <= observation_period_end_date
	AND cohort_end_date >= observation_period_start_date
{@has_exposure_ids} ? {
WHERE
	@cohort_definition_id IN (SELECT concept_id FROM #exposure_ids)
}
;  		
}

/* Create outcome eras */
{@outcome_table == 'condition_occurrence'} ? {	
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'hoi',
	cases.observation_period_id,
	condition_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, condition_start_date),
	DATEDIFF(dd, observation_period_start_date, condition_end_date)
FROM @outcome_database_schema.condition_occurrence 
INNER JOIN #cases cases
ON condition_occurrence.person_id = cases.person_id
	AND condition_start_date >= observation_period_start_date
	AND condition_start_date <= observation_period_end_date
WHERE
	condition_concept_id IN (@outcome_concept_ids)
	{@outcome_condition_type_concept_ids != ''} ? {
	AND condition_type_concept_id IN (@outcome_condition_type_concept_ids)
	} 			
;  	
} : { 
	{@outcome_table == 'condition_era'} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'hoi',
	cases.observation_period_id,
	condition_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, condition_era_start_date),
	DATEDIFF(dd, observation_period_start_date, condition_era_end_date)
FROM @outcome_database_schema.condition_era
INNER JOIN #cases cases
ON condition_era.person_id = cases.person_id
	AND condition_era_start_date >= observation_period_start_date
	AND condition_era_start_date <= observation_period_end_date
WHERE
	condition_concept_id IN (@outcome_concept_ids)
;  
	} : { /* outcome table has same structure as cohort table */
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'hoi',
	cases.observation_period_id,
	@cohort_definition_id,
	1,
	DATEDIFF(dd, observation_period_start_date, cohort_start_date),
	DATEDIFF(dd, observation_period_start_date, cohort_end_date)
FROM @outcome_database_schema.@outcome_table outcomes
INNER JOIN #cases cases
ON outcomes.subject_id = cases.person_id
	AND cohort_start_date >= observation_period_start_date
	AND cohort_start_date <= observation_period_end_date
WHERE
	@cohort_definition_id IN (@outcome_concept_ids)
;  	
	}	
}
	
/* Create custom eras */
{@use_custom_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'custom',
	cases.observation_period_id,
	@cohort_definition_id,
	1,
	DATEDIFF(dd, observation_period_start_date, cohort_start_date),
	DATEDIFF(dd, observation_period_start_date, cohort_start_date)
FROM @custom_covariate_database_schema.@custom_covariate_table covars
INNER JOIN #cases cases
ON covars.subject_id = cases.person_id
WHERE cohort_start_date <= observation_period_end_date
	AND cohort_start_date >= observation_period_start_date
{@has_custom_covariate_ids} ? {
	AND @cohort_definition_id IN (SELECT concept_id FROM #custom_coviariate_ids)
}
;  		
}

/**********************************************************************
		Delete covariates with concept_id = 0 or small cell count
***********************************************************************/
DELETE FROM #eras 
WHERE concept_id = 0;

{@delete_covariates_small_count != 0} ? {	
DELETE FROM #eras 
WHERE concept_id IN (
	  SELECT concept_id
	  FROM #eras
		GROUP BY concept_id
	  HAVING COUNT(concept_id) < @delete_covariates_small_count
	);
}	

/**********************************************************************
					Create covariate ref table
***********************************************************************/
SELECT concept.concept_id AS covariate_id,
	concept_name AS covariate_name
INTO #covariate_ref
FROM concept
INNER JOIN (
	SELECT DISTINCT concept_id
	FROM #eras
	) eras
ON eras.concept_id = concept.concept_id
;

/**********************************************************************
					            Cleanup
***********************************************************************/
{@has_exposure_ids} ? {
TRUNCATE TABLE #exposure_ids;

DROP TABLE #exposure_ids;
}

{@has_custom_covariate_ids} ? {
TRUNCATE TABLE #custom_covariate_ids;

DROP TABLE #custom_covariate_ids;
}
