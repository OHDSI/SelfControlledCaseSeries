IF OBJECT_ID('@cohortDatabaseSchema.@outcomeTable', 'U') IS NOT NULL
  DROP TABLE @cohortDatabaseSchema.@outcomeTable;

SELECT 1,
	condition_start_date,
	condition_end_date,
	condition_occurrence.person_id
INTO @cohortDatabaseSchema.@outcomeTable
FROM @cdmDatabaseSchema.condition_occurrence
INNER JOIN @cdmDatabaseSchema.visit_occurrence
	ON condition_occurrence.visit_occurrence_id = visit_occurrence.visit_occurrence_id
WHERE condition_concept_id IN (
		SELECT descendant_concept_id
		FROM @cdmDatabaseSchema.concept_ancestor
		WHERE ancestor_concept_id = 192671 -- GI - Gastrointestinal haemorrhage
		)
	AND visit_occurrence.place_of_service_concept_id IN (9201, 9203);