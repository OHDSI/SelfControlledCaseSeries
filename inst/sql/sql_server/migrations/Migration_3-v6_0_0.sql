-- Database migrations for version 6.0.0
-- This migration updates the schema:
-- 1. Deprecates the sccs_event_dep_observation table. (No changes)
-- 2. Adds pre_exposure and end_of_observation_period fields to sccs_covariate_analysis table.
-- 3. Deprecates the time_trend_p, time_trend_diagnostic, pre_exposure_p and pre_exposure_diagnostic from sccs_diagnostics_summary table. (No changes)
-- 4. Adds the time_stability_p, time_stability_diagnostic, event_exposure_lb, event_exposure_ub,  event_exposure_diagnostic, event_observation_lb, event_observation_ub, event_observation_diagnostic, rare_outcome_prevalence, and rare_outcome_diagnostic fields to the sccs_diagnostics_summary table.
-- 5. Adds the gradient field to the sccs_likelihood_profile table.


ALTER TABLE @database_schema.@table_prefixsccs_covariate_analysis ADD pre_exposure INT;
ALTER TABLE @database_schema.@table_prefixsccs_covariate_analysis ADD end_of_observation_period INT;

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

ALTER TABLE @database_schema.@table_prefixsccs_likelihood_profile ADD gradient FLOAT;
