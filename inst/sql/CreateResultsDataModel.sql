{DEFAULT @table_prefix = ''}
{DEFAULT @sccs_analysis = sccs_analysis}
{DEFAULT @sccs_covariate_analysis = sccs_covariate_analysis}
{DEFAULT @sccs_covariate = sccs_covariate}
{DEFAULT @sccs_era = sccs_era}
{DEFAULT @sccs_exposures_outcome_set = sccs_exposures_outcome_set}
{DEFAULT @sccs_exposure = sccs_exposure}
{DEFAULT @sccs_spline = sccs_spline}
{DEFAULT @sccs_censor_model = sccs_censor_model}
{DEFAULT @sccs_result = sccs_result}
{DEFAULT @sccs_covariate_result = sccs_covariate_result}
{DEFAULT @sccs_attrition = sccs_attrition}
{DEFAULT @sccs_likelihood_profile = sccs_likelihood_profile}
{DEFAULT @sccs_time_trend = sccs_time_trend}
{DEFAULT @sccs_time_to_event = sccs_time_to_event}
{DEFAULT @sccs_age_spanning = sccs_age_spanning}
{DEFAULT @sccs_calendar_time_spanning = sccs_calendar_time_spanning}
{DEFAULT @sccs_event_dep_observation = sccs_event_dep_observation}
{DEFAULT @sccs_diagnostics_summary = sccs_diagnostics_summary}
  
CREATE TABLE @database_schema.@table_prefix@sccs_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_covariate_analysis (
  	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 variable_of_interest INT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_covariate (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 covariate_name VARCHAR,
	 era_id INT,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_era (
  	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 era_type VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 era_name VARCHAR,
	 database_id VARCHAR NOT NULL
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_exposures_outcome_set (
  	 exposures_outcome_set_id INT NOT NULL,
	 outcome_id INT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_exposure (
  	 exposures_outcome_set_id INT NOT NULL,
	 era_id INT NOT NULL,
	 true_effect_size FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_spline (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 spline_type VARCHAR NOT NULL,
	 knot_month FLOAT NOT NULL,
	 rr FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_censor_model (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 parameter_id INT NOT NULL,
	 parameter_value FLOAT,
	 model_type VARCHAR
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	 p FLOAT,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 covariate_subjects INT,
	 covariate_days INT,
	 covariate_eras INT,
	 covariate_outcomes INT,
	 observed_days BIGINT,
	 log_rr FLOAT,
	 se_log_rr FLOAT,
	 llr FLOAT,
	 calibrated_rr FLOAT,
	 calibrated_ci_95_lb FLOAT,
	 calibrated_ci_95_ub FLOAT,
	 calibrated_p FLOAT,
	 calibrated_log_rr FLOAT,
	 calibrated_se_log_rr FLOAT,
	 database_id VARCHAR NOT NULL
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_covariate_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_attrition (
  	 sequence_number INT NOT NULL,
	 description VARCHAR,
	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 observed_days BIGINT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_likelihood_profile (
  	 log_rr FLOAT NOT NULL,
	 log_likelihood FLOAT,
	 covariate_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_time_trend (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 observed_subjects INT,
	 outcome_rate FLOAT,
	 adjusted_rate FLOAT,
	 stable INT,
	 p FLOAT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_time_to_event (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 week INT NOT NULL,
	 observed_subjects INT,
	 outcomes INT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_age_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 age_month INT NOT NULL,
	 cover_before_after_subjects INT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_calendar_time_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 cover_before_after_subjects INT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_event_dep_observation (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 months_to_end INT NOT NULL,
	 censored INT NOT NULL,
	 outcomes INT
);
 
CREATE TABLE @database_schema.@table_prefix@sccs_diagnostics_summary (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 mdrr FLOAT,
	 ease FLOAT,
	 time_trend_p FLOAT,
	 pre_exposure_p FLOAT,
	 mdrr_diagnostic VARCHAR(20),
	 ease_diagnostic VARCHAR(20),
	 time_trend_diagnostic VARCHAR(20),
	 pre_exposure_diagnostic VARCHAR(20),
	 unblind INT
);

