/**********************************************************************
@file CreateEras.sql

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
{DEFAULT @outcome_database_schema = 'cdm4_sim'}
{DEFAULT @outcome_table = 'condition_occurrence'}
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @exposure_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @exposure_table = 'drug_era'}
{DEFAULT @use_custom_covariates = FALSE}
{DEFAULT @custom_covariate_database_schema = 'cdm4_sim.dbo'}
{DEFAULT @custom_covariate_table = 'drug_era'}
{DEFAULT @has_exposure_ids = FALSE}
{DEFAULT @has_custom_covariate_ids = FALSE}
{DEFAULT @delete_covariates_small_count = 100}
{DEFAULT @study_start_date = '' }
{DEFAULT @study_end_date = '' }
{DEFAULT @sampled_cases = FALSE}

DROP TABLE IF EXISTS #eras;

DROP TABLE IF EXISTS #era_ref;

CREATE TABLE #eras (
	era_type VARCHAR(3),
	case_id INT,
	era_id INT,
	era_value FLOAT,
	start_day INT,
	end_day INT
);

CREATE TABLE #era_ref (
	era_type VARCHAR(3),
	era_id INT,
	era_name VARCHAR(255)
);

/* Create exposure eras */
{@exposure_table == 'drug_era'} ? {
INSERT INTO #eras (era_type, case_id, era_id, era_value, start_day, end_day)
SELECT 'rx',
	cases.case_id,
	drug_concept_id,
	1,
	DATEDIFF(dd, start_date, drug_era_start_date),
	DATEDIFF(dd, start_date, drug_era_end_date)
FROM @exposure_database_schema.drug_era
{@sampled_cases} ? {
INNER JOIN #sampled_cases cases
} : {
INNER JOIN #cases cases
}
ON drug_era.person_id = cases.person_id
	AND drug_era_start_date <= end_date
	AND drug_era_end_date >= observation_period_start_date
{@has_exposure_ids} ? {
WHERE
	drug_concept_id IN (SELECT concept_id FROM #exposure_ids)
}
;

INSERT INTO #era_ref (era_type, era_id, era_name)
SELECT 'rx',
	concept_id,
	concept_name
FROM (
	SELECT DISTINCT era_id
	FROM #eras
	WHERE era_type = 'rx'
	) eras
LEFT JOIN @cdm_database_schema.concept
ON eras.era_id = concept.concept_id;

} : { /* exposure table has same structure as cohort table */
INSERT INTO #eras (era_type, case_id, era_id, era_value, start_day, end_day)
SELECT 'rx',
	cases.case_id,
	cohort_definition_id,
	1,
	DATEDIFF(dd, start_date, cohort_start_date),
	DATEDIFF(dd, start_date, cohort_end_date)
FROM @exposure_database_schema.@exposure_table exposure
{@sampled_cases} ? {
INNER JOIN #sampled_cases cases
} : {
INNER JOIN #cases cases
}
ON exposure.subject_id = cases.person_id
	AND cohort_start_date <= end_date
	AND cohort_end_date >= observation_period_start_date
{@has_exposure_ids} ? {
WHERE
	cohort_definition_id IN (SELECT concept_id FROM #exposure_ids)
}
;

INSERT INTO #era_ref (era_type, era_id, era_name)
SELECT 'rx',
	era_id,
	CONCAT('Exposure cohort ', era_id)
FROM (
	SELECT DISTINCT era_id
	FROM #eras
	WHERE era_type = 'rx'
	) eras;
}

/* Create outcome eras */
INSERT INTO #eras (era_type, case_id, era_id, era_value, start_day, end_day)
SELECT 'hoi',
	cases.case_id,
	outcome_id,
	1,
	DATEDIFF(dd, start_date, outcome_date),
	DATEDIFF(dd, start_date, outcome_date)
{@use_nesting_cohort} ? {
FROM #outcomes_in_nesting outcomes
} : { {@study_start_date != '' & @study_end_date != ''} ? {
FROM #outcomes_in_period outcomes
} : {
FROM #outcomes outcomes
}}	 
{@sampled_cases} ? {
INNER JOIN #sampled_cases cases
} : {
INNER JOIN #cases cases
}
ON outcomes.observation_period_id = cases.observation_period_id;

{@outcome_table == 'condition_occurrence' | @outcome_table == 'condition_era'} ? {
INSERT INTO #era_ref (era_type, era_id, era_name)
SELECT 'hoi',
	concept_id,
	concept_name
FROM @cdm_database_schema.concept
INNER JOIN (
	SELECT DISTINCT era_id
	FROM #eras
	WHERE era_type = 'hoi'
	) eras
ON eras.era_id = concept.concept_id;
} : {	
INSERT INTO #era_ref (era_type, era_id, era_name)
SELECT 'hoi',
	era_id,
	CONCAT('Outcome cohort ', era_id)
FROM (
	SELECT DISTINCT era_id
	FROM #eras
	WHERE era_type = 'hoi'
	) eras;		
}

/* Create custom eras */
{@use_custom_covariates} ? {
{@custom_covariate_table == 'condition_era'} ? {

INSERT INTO #eras (era_type, case_id, era_id, era_value, start_day, end_day)
SELECT 'dx',
	cases.case_id,
	condition_concept_id,
	1,
	DATEDIFF(dd, start_date, condition_era_start_date),
	DATEDIFF(dd, start_date, condition_era_end_date)
FROM @custom_covariate_database_schema.@custom_covariate_table covars
{@sampled_cases} ? {
INNER JOIN #sampled_cases cases
} : {
INNER JOIN #cases cases
}
ON covars.person_id = cases.person_id
WHERE condition_era_start_date <= end_date
	AND condition_era_start_date >= observation_period_start_date
{@has_custom_covariate_ids} ? {
	AND condition_concept_id IN (SELECT concept_id FROM #custom_cov_ids)
}
;

INSERT INTO #era_ref (era_type, era_id, era_name)
SELECT 'dx',
	concept_id,
	concept_name
FROM (
	SELECT DISTINCT era_id
	FROM #eras
	WHERE era_type = 'dx'
	) eras
LEFT JOIN @cdm_database_schema.concept
ON eras.era_id = concept.concept_id;

} : {

INSERT INTO #eras (era_type, case_id, era_id, era_value, start_day, end_day)
SELECT 'cst',
	cases.case_id,
	cohort_definition_id,
	1,
	DATEDIFF(dd, start_date, cohort_start_date),
	DATEDIFF(dd, start_date, cohort_end_date)
FROM @custom_covariate_database_schema.@custom_covariate_table covars
{@sampled_cases} ? {
INNER JOIN #sampled_cases cases
} : {
INNER JOIN #cases cases
}
ON covars.subject_id = cases.person_id
WHERE cohort_start_date <= end_date
	AND cohort_start_date >= observation_period_start_date
{@has_custom_covariate_ids} ? {
	AND cohort_definition_id IN (SELECT concept_id FROM #custom_cov_ids)
}
;

INSERT INTO #era_ref (era_type, era_id, era_name)
SELECT 'cst',
	era_id,
	CONCAT('Custom cohort ', era_id)
FROM (
	SELECT DISTINCT era_id
	FROM #eras
	WHERE era_type = 'cst'
	) eras;	

}
}

/**********************************************************************
		Delete covariates with era_id = 0 or small cell count
***********************************************************************/
DELETE FROM #eras
WHERE era_id = 0;

{@delete_covariates_small_count != 0} ? {
DELETE FROM #eras
WHERE era_id IN (
	  SELECT era_id
	  FROM #eras
		GROUP BY era_id
	  HAVING COUNT(era_id) < @delete_covariates_small_count
	);
}
