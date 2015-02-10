/**********************************************************************
@file Sccs.sql

Copyright 2014 Observational Health Data Sciences and Informatics

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
{DEFAULT @results_database_schema = 'scratch.dbo'} 
{DEFAULT @results_database = 'scratch'}
{DEFAULT @outcome_database_schema = 'cdm4_sim'} 
{DEFAULT @outcome_table = 'condition_occurrence'} 
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @outcome_condition_type_concept_ids = ''}  
{DEFAULT @exposure_database_schema = 'cdm4_sim.dbo'} 
{DEFAULT @exposure_table = 'drug_era'} 
{DEFAULT @exposure_concept_ids = ''}
{DEFAULT @drug_era_covariates = FALSE}
{DEFAULT @condition_era_covariates = FALSE}
{DEFAULT @procedure_covariates = FALSE}
{DEFAULT @visit_covariates = FALSE}
{DEFAULT @observation_covariates = FALSE}
{DEFAULT @delete_covariates_small_count = 100}

USE @results_database;

IF OBJECT_ID('#cases', 'U') IS NOT NULL 
	DROP TABLE #cases;
  
IF OBJECT_ID('#eras', 'U') IS NOT NULL 
	DROP TABLE #eras;
	
/**********************************************************************
							Select cases
***********************************************************************/
SELECT observation_period_id,
	person.person_id,
	observation_period_start_date,
	observation_period_end_date,
	year_of_birth,
	month_of_birth,
	day_of_birth
INTO #cases
FROM @cdm_database_schema.observation_period
INNER JOIN @cdm_database_schema.person
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
			AND condition_start_date <= observation_period_end_date
			AND	condition_start_date >= observation_period_start_date	
			AND	cohort_concept_id IN (@outcome_concept_ids)
	}
}
	) 
;

/**********************************************************************
							Create eras
***********************************************************************/
CREATE TABLE #eras (
	era_type VARCHAR(3),
	observation_period_id INT,
	concept_id INT,
	start_day INT,
	end_day INT
)

/* Create exposure eras */
{@exposure_table == 'drug_era'} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'hei',
	cases.observation_period_id,
	drug_concept_id,
	DATEDIFF(dd, observation_period_start_date, drug_era_start_date),
	DATEDIFF(dd, observation_period_start_date, drug_era_end_date)
FROM @exposure_database_schema.drug_era
INNER JOIN #cases cases
ON drug_era.person_id = cases.person_id
	AND drug_era_start_date <= observation_period_end_date
	AND drug_era_end_date >= observation_period_start_date
	{@exposure_concept_ids != ''} ? {
WHERE
	drug_concept_id IN (@exposure_concept_ids)
	}
;  	
} : { /* exposure table has same structure as cohort table */
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'hei',
	cases.observation_period_id,
	cohort_concept_id,
	DATEDIFF(dd, observation_period_start_date, cohort_start_date),
	DATEDIFF(dd, observation_period_start_date, cohort_end_date)
FROM @exposure_database_schema.@exposure_table exposure
INNER JOIN #cases cases
ON exposure.subject_id = cases.person_id
	AND cohort_start_date <= observation_period_end_date
	AND cohort_end_date >= observation_period_start_date
	{@exposure_concept_ids != ''} ? {
WHERE
	cohort_concept_id IN (@exposure_concept_ids)
	}
;  		
}

/* Create outcome eras */
{@outcome_table == 'condition_occurrence'} ? {	
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'hoi',
	cases.observation_period_id,
	condition_concept_id,
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
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'hoi',
	cases.observation_period_id,
	condition_concept_id,
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
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'hoi',
	cases.observation_period_id,
	cohort_concept_id,
	DATEDIFF(dd, observation_period_start_date, cohort_start_date),
	DATEDIFF(dd, observation_period_start_date, cohort_end_date)
FROM @outcome_database_schema.@outcome_table outcomes
INNER JOIN #cases cases
ON outcomes.subject_id = cases.person_id
	AND cohort_start_date >= observation_period_start_date
	AND cohort_era_start_date <= observation_period_end_date
WHERE
	cohort_concept_id IN (@outcome_concept_ids)
;  	
	}	
}
	
/* Create drug eras */
{@drug_era_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'rx',
	cases.observation_period_id,
	drug_concept_id,
	DATEDIFF(dd, observation_period_start_date, drug_era_start_date),
	DATEDIFF(dd, observation_period_start_date, drug_era_end_date)
FROM @cdm_database_schema.drug_era
INNER JOIN #cases cases
ON drug_era.person_id = cases.person_id
	AND drug_era_start_date <= observation_period_end_date
	AND drug_era_end_date >= observation_period_start_date
;  		
}

/* Create condition eras */
{@condition_era_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'dx',
	cases.observation_period_id,
	condition_concept_id,
	DATEDIFF(dd, observation_period_start_date, condition_era_start_date),
	DATEDIFF(dd, observation_period_start_date, condition_era_end_date)
FROM @cdm_database_schema.condition_era
INNER JOIN #cases cases
ON condition_era.person_id = cases.person_id
	AND condition_era_start_date <= observation_period_end_date
	AND condition_era_end_date >= observation_period_start_date
;  		
}

/* Create procedure eras */
{@procedure_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'px',
	cases.observation_period_id,
	procedure_concept_id,
	DATEDIFF(dd, observation_period_start_date, procedure_date),
	NULL
FROM @cdm_database_schema.procedure_occurrence
INNER JOIN #cases cases
ON procedure_occurrence.person_id = cases.person_id
	AND procedure_date <= observation_period_end_date
	AND procedure_date >= observation_period_start_date
;  		
}

/* Create visit eras */
{@visit_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'rx',
	cases.observation_period_id,
	visit_concept_id,
	DATEDIFF(dd, observation_period_start_date, visit_start_date),
	DATEDIFF(dd, observation_period_start_date, visit_end_date)
FROM @cdm_database_schema.visit_occurrence
INNER JOIN #cases cases
ON visit_occurrence.person_id = cases.person_id
	AND visit_start_date <= observation_period_end_date
	AND visit_end_date >= observation_period_start_date
;  		
}

/* Create observation eras */
{@observation_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, start_day, end_day)
SELECT 'rx',
	cases.observation_period_id,
	observation_concept_id,
	DATEDIFF(dd, observation_period_start_date, observation_date),
	NULL
FROM @cdm_database_schema.observation
INNER JOIN #cases cases
ON observation.person_id = cases.person_id
	AND observation_date <= observation_period_end_date
	AND observation_date >= observation_period_start_date
;  		
}

/**********************************************************************
		Delete covariates with concept_id = 0 or small cell count
***********************************************************************/
DELETE FROM #eras 
WHERE concept_id = 0;

{@delete_covariates_small_count != 0} ? {	
DELETE FROM #eras 
WHERE era_type != 'hei'
	AND era_type != 'hoi'
	AND concept_id IN (
	  SELECT concept_id
	  FROM #eras
		GROUP BY concept_id
	  HAVING COUNT(concept_id) < @delete_covariates_small_count
	);
}	

/**********************************************************************
					Create covariate ref table
***********************************************************************/
SELECT concept.concept_id,
	concept_name
INTO #covariate_ref
FROM @cdm_database_schema.concept
INNER JOIN (
	SELECT DISTINCT concept_id
	FROM #eras
	) eras
ON eras.concept_id = concept.concept_id
;
