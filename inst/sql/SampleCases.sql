/**********************************************************************
Copyright 2024 Observational Health Data Sciences and Informatics

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

{DEFAULT @max_cases_per_outcome = 1000000}
{DEFAULT @case_table = "#cases"}

DROP TABLE IF EXISTS #sampled_cases_per_o;

DROP TABLE IF EXISTS #sampled_cases;

SELECT case_id,
	start_date,
	end_date,
	outcome_id
INTO #sampled_cases_per_o
FROM (
	SELECT case_id,
		start_date,
		end_date,
		outcome_id,
		ROW_NUMBER() OVER (PARTITION BY outcome_id ORDER BY random_id) AS rn
	FROM (
		SELECT DISTINCT case_id,
			start_date,
			end_date,
			outcome_id,
			random_id
		FROM @case_table cases
		INNER JOIN #outcomes outcomes
			ON outcomes.person_id = cases.person_id
				AND outcome_date >= start_date
				AND outcome_date <= end_date
	) cases
) temp
WHERE rn <= @max_cases_per_outcome;

SELECT cases.*
INTO #sampled_cases
FROM (
	SELECT DISTINCT case_id,
		start_date,
		end_date
	FROM #sampled_cases_per_o
	) sampled_ids
INNER JOIN @case_table cases
	ON cases.case_id = sampled_ids.case_id
		AND cases.start_date = sampled_ids.start_date
		AND cases.end_date = sampled_ids.end_date;


