TRUNCATE TABLE #cases;
DROP TABLE #cases;

TRUNCATE TABLE #eras;
DROP TABLE #eras;

TRUNCATE TABLE #era_ref;
DROP TABLE #era_ref;

TRUNCATE TABLE #outcome_ids;
DROP TABLE #outcome_ids;

TRUNCATE TABLE #counts;
DROP TABLE #counts;

{@use_nesting_cohort} ? {
TRUNCATE TABLE #outcomes_in_nesting;
DROP TABLE #outcomes_in_nesting;
} 

{@study_start_date != '' & @study_end_date != ''} ? {
TRUNCATE TABLE #outcomes_in_period;
DROP TABLE #outcomes_in_period;
}

TRUNCATE TABLE #outcomes;
DROP TABLE #outcomes;

{@sampled_cases} ? {
TRUNCATE TABLE #sampled_cases_per_o;
DROP TABLE #sampled_cases_per_o;

TRUNCATE TABLE #sampled_cases;
DROP TABLE #sampled_cases;
}

{@has_exposure_ids} ? {
TRUNCATE TABLE #exposure_ids;

DROP TABLE #exposure_ids;
}

{@has_custom_covariate_ids} ? {
TRUNCATE TABLE #custom_cov_ids;

DROP TABLE #custom_cov_ids;
}
