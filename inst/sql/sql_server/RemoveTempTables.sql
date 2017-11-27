TRUNCATE TABLE #cases;
DROP TABLE #cases;

TRUNCATE TABLE #eras;
DROP TABLE #eras;

TRUNCATE TABLE #covariate_ref;
DROP TABLE #covariate_ref;

{@cases_per_outcome} ? {
TRUNCATE TABLE #cases_per_outcome;
DROP TABLE #cases_per_outcome;
}

{@sampled_cases} ? {
TRUNCATE TABLE #sampled_cases_per_o;
DROP TABLE #sampled_cases_per_o;

TRUNCATE TABLE #sampled_cases;
DROP TABLE #sampled_cases;
}
