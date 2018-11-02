/**********************************************************************
@file CasesPerOutcome.sql

Copyright 2018 Observational Health Data Sciences and Informatics

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

{DEFAULT @outcome_database_schema = 'cdm4_sim'}
{DEFAULT @outcome_table = 'condition_occurrence'}
{DEFAULT @outcome_concept_ids = ''}
{DEFAULT @cohort_definition_id = 'cohort_concept_id'}

IF OBJECT_ID('tempdb..#cases_per_outcome', 'U') IS NOT NULL
	DROP TABLE #cases_per_outcome;

{@outcome_table == 'condition_occurrence'} ? {
SELECT DISTINCT observation_period_id,
	random_id,
	condition_concept_id AS outcome_id
INTO #cases_per_outcome
FROM #cases cases
INNER JOIN @outcome_database_schema.condition_occurrence outcome
	ON outcome.person_id = cases.person_id
		AND	condition_start_date <= cases.end_date
		AND	condition_start_date >= cases.start_date
WHERE condition_concept_id IN (@outcome_concept_ids)
} : {
{@outcome_table == 'condition_era'} ? {
SELECT DISTINCT observation_period_id,
	random_id,
	condition_concept_id AS outcome_id
INTO #cases_per_outcome
FROM #cases cases
INNER JOIN @outcome_database_schema.condition_era outcome
	ON outcome.person_id = cases.person_id
		AND	condition_era_start_date <= cases.end_date
		AND	condition_era_start_date >= cases.start_date
WHERE condition_concept_id IN (@outcome_concept_ids)
} : { /* outcome table has same structure as cohort table */
SELECT DISTINCT observation_period_id,
	random_id,
	@cohort_definition_id AS outcome_id
INTO #cases_per_outcome
FROM #cases cases
INNER JOIN @outcome_database_schema.@outcome_table outcome
	ON outcome.subject_id = cases.person_id
		AND	cohort_start_date <= cases.end_date
		AND	cohort_start_date >= cases.start_date
WHERE @cohort_definition_id IN (@outcome_concept_ids)
}
}
;
