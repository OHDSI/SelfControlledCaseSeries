/**********************************************************************
Copyright 2024 Observational Health Data Sciences and Informatics
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

{DEFAULT @outcome_database_schema = 'cdm_sim.dbo'}
{DEFAULT @outcome_table = 'condition_occurrence'}

DROP TABLE IF EXISTS #outcomes;
	
{@outcome_table == 'condition_occurrence'} ? {
SELECT person_id AS person_id,
	condition_concept_id AS outcome_id,
	condition_start_date AS outcome_date
INTO #outcomes
FROM @outcome_database_schema.condition_occurrence outcome
INNER JOIN #outcome_ids
	ON condition_concept_id = outcome_id;
} : {{@outcome_table == 'condition_era'} ? {
SELECT person_id AS person_id,
	condition_concept_id AS outcome_id,
	condition_era_start_date AS outcome_date
INTO #outcomes
FROM @outcome_database_schema.condition_era outcome
INNER JOIN #outcome_ids
	ON condition_concept_id = outcome_id;
} : { /* outcome table has same structure as cohort table */
SELECT subject_id AS person_id,
	cohort_definition_id AS outcome_id,
	cohort_start_date AS outcome_date
INTO #outcomes
FROM @outcome_database_schema.@outcome_table outcome
INNER JOIN #outcome_ids
	ON cohort_definition_id = outcome_id;
}}
