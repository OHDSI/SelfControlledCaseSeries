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
{DEFAULT @case_table = '#cases'}

SELECT outcome_ids.outcome_id,
	CASE WHEN outcome_subjects IS NULL THEN CAST(0 AS INT) ELSE outcome_subjects END AS outcome_subjects,
	CASE WHEN outcome_events IS NULL THEN CAST(0 AS INT) ELSE outcome_events END AS outcome_events,
	CASE WHEN outcome_obs_periods IS NULL THEN CAST(0 AS INT) ELSE outcome_obs_periods END AS outcome_obs_periods,
	CASE WHEN observed_days IS NULL THEN CAST(0 AS BIGINT) ELSE observed_days END AS observed_days
FROM #outcome_ids outcome_ids
LEFT JOIN (
	SELECT outcome_id,
		COUNT(DISTINCT outcomes.person_id) AS outcome_subjects,
		COUNT(*) AS outcome_events,
		COUNT(DISTINCT observation_period_id) AS outcome_obs_periods,
		SUM(CAST(DATEDIFF(DAY, start_date, end_date) AS BIGINT) + 1) AS observed_days
	FROM #outcomes outcomes
	INNER JOIN @case_table cases
		ON outcomes.person_id = cases.person_id
			AND outcome_date >= start_date
			AND outcome_date <= end_date
	GROUP BY outcome_id
	) counts
ON outcome_ids.outcome_id = counts.outcome_id;
