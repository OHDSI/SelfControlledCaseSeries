/**********************************************************************
@file SampleCases.sql

Copyright 2017 Observational Health Data Sciences and Informatics

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

IF OBJECT_ID('tempdb..#sampled_cases', 'U') IS NOT NULL
	DROP TABLE #sampled_cases;
	
SELECT cases.observation_period_id,
	cases.person_id,
	cases.observation_period_start_date,
	cases.start_date,
	cases.end_date,
	cases.age_in_days
INTO #sampled_cases
FROM (
	SELECT DISTINCT observation_period_id
	FROM (
		SELECT observation_period_id,
		ROW_NUMBER() OVER (PARTITION BY outcome_id ORDER BY random_id) AS rn 
		FROM #cases_per_outcome 
		) temp
	WHERE rn <= @max_cases_per_outcome
	) sampled_ids
INNER JOIN #cases cases
	ON cases.observation_period_id = sampled_ids.observation_period_id;