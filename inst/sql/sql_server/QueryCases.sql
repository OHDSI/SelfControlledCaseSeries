/**********************************************************************
@file QueryCases.sql

Copyright 2023 Observational Health Data Sciences and Informatics

This file is part of SelfControlledCaseSeries

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

   http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
***********************************************************************/

{DEFAULT @sampled_cases = FALSE}

SELECT
	CAST(observation_period_id AS VARCHAR(30)) AS observation_period_id,
	case_id,
	CAST(person_id AS VARCHAR(30)) AS person_id,
	DATEDIFF(DAY, start_date, end_date) + 1 AS observation_days,
	YEAR(start_date) AS start_year,
	MONTH(start_date) AS start_month,
	DAY(start_date) AS start_day,
	age_in_days,
	DATEDIFF(DAY, observation_period_start_date, start_date) AS censored_days,
	noninformative_end_censor,
	gender_concept_id
{@sampled_cases} ? {
FROM #sampled_cases
} : {
FROM #cases
}
ORDER BY case_id;
