-- Database migrations for version 6.0.0
-- This migration updates the schema:
-- 1. Drops the sccs_event_dep_observation table.
-- 2. Adds pre_exposure and end_of_observation_period fields to sccs_covariate_analysis table.
-- 3. Drops the time_trend_p, time_trend_diagnostic, pre_exposure_p and pre_exposure_diagnostic from sccs_diagnostics_summary table.
-- 4. Adds the time_stability_p, time_stability_diagnostic, event_exposure_lb, event_exposure_ub,  event_exposure_diagnostic, event_observation_lb, event_observation_ub, event_observation_diagnostic, rare_outcome_prevalence, and rare_outcome_diagnostic fields to the sccs_diagnostics_summary table.

DROP TABLE @database_schema.@table_prefixsccs_event_dep_observation;

ALTER TABLE @database_schema.@table_prefixsccs_covariate_analysis ADD pre_exposure INT;
ALTER TABLE @database_schema.@table_prefixend_of_observation_period ADD pre_exposure INT;

ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary DROP COLUMN time_trend_p;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary DROP COLUMN time_trend_diagnostic;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary DROP COLUMN pre_exposure_p;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary DROP COLUMN pre_exposure_diagnostic;

ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD time_stability_p FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD time_stability_diagnostic VARCHAR(20);
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD event_exposure_lb FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD event_exposure_ub FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD event_exposure_diagnostic VARCHAR(20);
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD event_observation_lb FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD event_observation_ub FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD event_observation_diagnostic VARCHAR(20);
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD rare_outcome_prevalence FLOAT;
ALTER TABLE @database_schema.@table_prefixsccs_diagnostics_summary ADD rare_outcome_diagnostic VARCHAR(20);
