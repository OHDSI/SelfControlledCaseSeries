TRUNCATE TABLE #cases;
DROP TABLE #cases;

TRUNCATE TABLE #eras;
DROP TABLE #eras;

TRUNCATE TABLE #era_ref;
DROP TABLE #era_ref;

TRUNCATE TABLE #outcome_ids;
DROP TABLE #outcome_ids;

{@use_nesting_cohort} ? {
TRUNCATE TABLE #cases_in_nesting;
DROP TABLE #cases_in_nesting;
} 

{@has_study_periods} ? {
TRUNCATE TABLE #study_periods;
DROP TABLE #study_periods;

TRUNCATE TABLE #cases_in_periods;
DROP TABLE #cases_in_periods;
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
