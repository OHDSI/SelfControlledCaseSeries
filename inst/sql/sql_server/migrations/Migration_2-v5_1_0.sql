-- Database migrations for version 5.1.0
-- This migration updates the schema:
-- 1. Adds the nesting_cohort_id to the sccs_exposures_outcome_set table.
-- 2. Adds the one_sided_p and calibrated_one_sided_p fields to the sccs_result table.
-- 3. Adds the unblind_for_evidence_synthesis field to the sccs_diagnostics_summary table.

ALTER TABLE @database_schema.@table_prefixsccs_exposures_outcome_set ADD nesting_cohort_id INT;
ALTER TABLE @database_schema.@table_prefixsccs_result ADD one_sided_p FLOAT, calibrated_one_sided_p FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD unblind_for_evidence_synthesis INT;
