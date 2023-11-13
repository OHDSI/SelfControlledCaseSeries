-- Database migrations for version 5.1.0
-- This migration updates the schema:
 -- 1. Adds the nesting_cohort_id to the sccs_exposures_outcome_set table

ALTER TABLE @database_schema.@table_prefixsccs_exposures_outcome_set ADD nesting_cohort_id FLOAT
