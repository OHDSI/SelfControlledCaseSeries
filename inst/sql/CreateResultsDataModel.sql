CREATE TABLE @database_schema.@table_prefixsccs_analysis (
  	 analysis_id INT NOT NULL,
	 description VARCHAR,
	 definition VARCHAR,
	PRIMARY KEY(analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_covariate_analysis (
  	 analysis_id INT NOT NULL,
	 covariate_analysis_id INT NOT NULL,
	 covariate_analysis_name VARCHAR,
	 variable_of_interest INT,
	PRIMARY KEY(analysis_id,covariate_analysis_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_covariate (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 covariate_name VARCHAR,
	 era_id INT,
	 covariate_analysis_id INT,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_era (
  	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 era_type VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 era_name VARCHAR,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(exposures_outcome_set_id,analysis_id,era_type,era_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_exposures_outcome_set (
  	 exposures_outcome_set_id INT NOT NULL,
	 outcome_id INT,
	PRIMARY KEY(exposures_outcome_set_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_exposure (
  	 exposures_outcome_set_id INT NOT NULL,
	 era_id INT NOT NULL,
	 true_effect_size FLOAT,
	PRIMARY KEY(exposures_outcome_set_id,era_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_spline (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 spline_type VARCHAR NOT NULL,
	 knot_month FLOAT NOT NULL,
	 rr FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,spline_type,knot_month)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_censor_model (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 parameter_id INT NOT NULL,
	 parameter_value FLOAT,
	 model_type VARCHAR,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,parameter_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_result (
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
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_covariate_result (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 covariate_id INT NOT NULL,
	 rr FLOAT,
	 ci_95_lb FLOAT,
	 ci_95_ub FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,covariate_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_attrition (
  	 sequence_number INT NOT NULL,
	 description VARCHAR,
	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 covariate_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 outcome_subjects INT,
	 outcome_events INT,
	 outcome_observation_periods INT,
	 observed_days BIGINT,
	PRIMARY KEY(sequence_number,analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_likelihood_profile (
  	 log_rr FLOAT NOT NULL,
	 log_likelihood FLOAT,
	 covariate_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 analysis_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	PRIMARY KEY(log_rr,covariate_id,exposures_outcome_set_id,analysis_id,database_id)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_time_trend (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 observed_subjects INT,
	 outcome_rate FLOAT,
	 adjusted_rate FLOAT,
	 stable INT,
	 p FLOAT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,calendar_year,calendar_month)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_time_to_event (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 era_id INT NOT NULL,
	 week INT NOT NULL,
	 observed_subjects INT,
	 outcomes INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,era_id,week)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_age_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 age_month INT NOT NULL,
	 cover_before_after_subjects INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,age_month)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_calendar_time_spanning (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 calendar_year INT NOT NULL,
	 calendar_month INT NOT NULL,
	 cover_before_after_subjects INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,calendar_year,calendar_month)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_event_dep_observation (
  	 analysis_id INT NOT NULL,
	 exposures_outcome_set_id INT NOT NULL,
	 database_id VARCHAR NOT NULL,
	 months_to_end INT NOT NULL,
	 censored INT NOT NULL,
	 outcomes INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,database_id,months_to_end,censored)
);
 
CREATE TABLE @database_schema.@table_prefixsccs_diagnostics_summary (
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
	 unblind INT,
	PRIMARY KEY(analysis_id,exposures_outcome_set_id,covariate_id,database_id)
);

