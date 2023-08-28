SELECT analysis_id,
	exposures_outcome_set_id,
	database_id,
	calendar_year
	calendar_month,
	observed_subjects,
	NULL AS ratio,
	NULL AS adjusted_ratio
INTO @new_time_trend_table
FROM @old_time_trend_table;