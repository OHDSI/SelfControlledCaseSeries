/**********************************************************************
@file QueryPersons.sql

Copyright 2014 Observational Health Data Sciences and Informatics

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

SELECT
	observation_period_id,
	person_id,
	DATEDIFF(dd, observation_period_start_date, observation_period_end_date) + 1 AS observation_days,
	YEAR(observation_period_start_date) AS observation_start_year,
	MONTH(observation_period_start_date) AS observation_start_month,
	DAY(observation_period_start_date) AS observation_start_day,
	age_in_days
FROM
	#cases
ORDER BY
	observation_period_id
;
