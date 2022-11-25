/***********************************
File vignette.sql 
***********************************/

DROP TABLE IF EXISTS @cohortDatabaseSchema.@outcomeTable;

SELECT 1 AS cohort_definition_id,
	condition_start_date AS cohort_start_date,
	condition_end_date AS cohort_end_date,
	condition_occurrence.person_id AS subject_id
INTO @cohortDatabaseSchema.@outcomeTable
FROM @cdmDatabaseSchema.condition_occurrence
INNER JOIN @cdmDatabaseSchema.visit_occurrence
	ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
WHERE condition_concept_id IN (
		SELECT descendant_concept_id
		FROM @cdmDatabaseSchema.concept_ancestor
		WHERE ancestor_concept_id = 192671 -- GI - Gastrointestinal haemorrhage
		)
	AND visit_occurrence.visit_concept_id IN (9201, 9203);
