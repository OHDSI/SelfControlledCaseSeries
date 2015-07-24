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

{DEFAULT @cdm_database = 'cdm4_sim.dbo'}
{DEFAULT @outcome_database_schema = 'cdm4_sim'} 
{DEFAULT @outcome_table = 'condition_occurrence'} 
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @outcome_condition_type_concept_ids = ''}  
{DEFAULT @exposure_database_schema = 'cdm4_sim.dbo'} 
{DEFAULT @exposure_table = 'drug_era'} 
{DEFAULT @exposure_concept_ids = ''}
{DEFAULT @exclude_concept_ids = ''}
{DEFAULT @drug_era_covariates = FALSE}
{DEFAULT @condition_era_covariates = FALSE}
{DEFAULT @procedure_covariates = FALSE}
{DEFAULT @visit_covariates = FALSE}
{DEFAULT @observation_covariates = FALSE}
{DEFAULT @measurement_covariates = FALSE}
{DEFAULT @delete_covariates_small_count = 100}
{DEFAULT @cdm_version == '4'}
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
	) 
;

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
	{@exposure_concept_ids != ''} ? {
WHERE
	drug_concept_id IN (@exposure_concept_ids)
		{@exclude_concept_ids != ''} ? {
	AND drug_concept_id NOT IN (@exclude_concept_ids)	
		}
	} : {
		{@exclude_concept_ids != ''} ? {
WHERE
	drug_concept_id NOT IN (@exclude_concept_ids)	
		}
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
	{@exposure_concept_ids != ''} ? {
WHERE
	@cohort_definition_id IN (@exposure_concept_ids)
		{@exclude_concept_ids != ''} ? {
	AND @cohort_definition_id NOT IN (@exclude_concept_ids)	
		}
	} : {
		{@exclude_concept_ids != ''} ? {
WHERE
	@cohort_definition_id NOT IN (@exclude_concept_ids)	
		}
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
	
/* Create drug eras */
{@drug_era_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'rx',
	cases.observation_period_id,
	drug_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, drug_era_start_date),
	DATEDIFF(dd, observation_period_start_date, drug_era_end_date)
FROM drug_era
INNER JOIN #cases cases
ON drug_era.person_id = cases.person_id
	AND drug_era_start_date <= observation_period_end_date
	AND drug_era_end_date >= observation_period_start_date
	{@exclude_concept_ids != ''} ? {
WHERE
	drug_concept_id NOT IN (@exclude_concept_ids)	
	}	
;  		
}

/* Create condition eras */
{@condition_era_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'dx',
	cases.observation_period_id,
	condition_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, condition_era_start_date),
	DATEDIFF(dd, observation_period_start_date, condition_era_end_date)
FROM condition_era
INNER JOIN #cases cases
ON condition_era.person_id = cases.person_id
	AND condition_era_start_date <= observation_period_end_date
	AND condition_era_end_date >= observation_period_start_date
	{@exclude_concept_ids != ''} ? {
WHERE
	condition_concept_id NOT IN (@exclude_concept_ids)	
	}	
;  		
}

/* Create procedure eras */
{@procedure_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'px',
	cases.observation_period_id,
	procedure_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, procedure_date),
	DATEDIFF(dd, observation_period_start_date, procedure_date)
FROM procedure_occurrence
INNER JOIN #cases cases
ON procedure_occurrence.person_id = cases.person_id
	AND procedure_date <= observation_period_end_date
	AND procedure_date >= observation_period_start_date
	{@exclude_concept_ids != ''} ? {
WHERE
	procedure_concept_id NOT IN (@exclude_concept_ids)	
	}		
;  		
}

/* Create visit eras */
{@visit_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'vx',
	cases.observation_period_id,
{@cdm_version == '4'} ? {
	place_of_service_concept_id,
} : {
    visit_concept_id,
}
    1,
	DATEDIFF(dd, observation_period_start_date, visit_start_date),
	DATEDIFF(dd, observation_period_start_date, visit_end_date)
FROM visit_occurrence
INNER JOIN #cases cases
ON visit_occurrence.person_id = cases.person_id
	AND visit_start_date <= observation_period_end_date
	AND visit_end_date >= observation_period_start_date
{@exclude_concept_ids != ''} ? {
WHERE
{@cdm_version == '4'} ? {
	place_of_service_concept_id NOT IN (@exclude_concept_ids)	
} : {
	visit_concept_id NOT IN (@exclude_concept_ids)	
}
}		
;  		
}

/* Create observation eras */
{@observation_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'rx',
	cases.observation_period_id,
	observation_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, observation_date),
	DATEDIFF(dd, observation_period_start_date, observation_date)
FROM observation
INNER JOIN #cases cases
ON observation.person_id = cases.person_id
	AND observation_date <= observation_period_end_date
	AND observation_date >= observation_period_start_date
	{@exclude_concept_ids != ''} ? {
WHERE
	observation_concept_id NOT IN (@exclude_concept_ids)	
	}		
;  		
}

/* Create measurement eras */
{@cdm_version != '4' & @measurement_covariates} ? {
INSERT INTO #eras (era_type, observation_period_id, concept_id, era_value, start_day, end_day)
SELECT 'rx',
	cases.observation_period_id,
	measurement_concept_id,
	1,
	DATEDIFF(dd, observation_period_start_date, measurement_date),
	DATEDIFF(dd, observation_period_start_date, measurement_date)
FROM measurement
INNER JOIN #cases cases
ON measurement.person_id = cases.person_id
	AND measurement_date <= observation_period_end_date
	AND measurement_date >= observation_period_start_date
	{@exclude_concept_ids != ''} ? {
WHERE
	measurement_concept_id NOT IN (@exclude_concept_ids)	
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
