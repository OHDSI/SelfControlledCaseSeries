/*
 * @file SccsConverter.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2014 Observational Health Data Sciences and Informatics
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef SCCSCONVERTER_CPP_
#define SCCSCONVERTER_CPP_

#include <Rcpp.h>
#include "SccsConverter.h"
#include "PersonDataIterator.h"

using namespace Rcpp;

namespace ohdsi {
	namespace sccs {

		std::vector<Era> SccsConverter::extractOutcomes(std::vector<Era>& eras) {
			std::vector<Era> outcomes;
			std::vector<Era>::iterator iterator;
			for (iterator = eras.begin(); iterator != eras.end();) {
				if ((*iterator).isOutcome) {
					outcomes.push_back((*iterator));
					iterator = eras.erase(iterator);
				} else {
					++iterator;
				}
			}
			return outcomes;
		}

		std::vector<Era> SccsConverter::mergeOverlapping(std::vector<Era>& eras) {
			std::vector<Era> mergedEras;
			std::map<int64_t, Era> conceptIdToRunningEra;
			for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
				std::map<int64_t, Era>::iterator found = conceptIdToRunningEra.find(era->conceptId);
				if (found != conceptIdToRunningEra.end()) {
					Era* runningEra = &(*found).second;
					if (runningEra->end >= era->start) {
						if (runningEra->end < era->end) {
							runningEra->end = era->end;
						}
					} else {
						mergedEras.push_back(*runningEra);
						(*found).second = *era;
					}
				} else {
					conceptIdToRunningEra.insert(std::pair<int, Era>(era->conceptId, *era));
				}
			}
			for (std::map<int64_t, Era>::iterator iterator = conceptIdToRunningEra.begin(); iterator != conceptIdToRunningEra.end(); ++iterator) {
				mergedEras.push_back(iterator->second);
			}
			return mergedEras;
		}

		void SccsConverter::removeAllButFirstOutcome(std::vector<Era>& outcomes) {
			std::set<int64_t> seenOutcomes;
			std::vector<Era>::iterator iterator;
			for (iterator = outcomes.begin(); iterator != outcomes.end();) {
				if (!seenOutcomes.insert((*iterator).conceptId).second)
					iterator = outcomes.erase(iterator);
				else
					++iterator;
			}
		}

		void SccsConverter::clipEras(std::vector<Era>& eras, const int startDay, const int endDay) {
			std::vector<Era>::iterator iterator;
			for (iterator = eras.begin(); iterator != eras.end();) {
				if ((*iterator).end < startDay || (*iterator).start > endDay) {
					iterator = eras.erase(iterator);
				} else {
					if (iterator->start < startDay)
						iterator->start = startDay;
					if (iterator->end > endDay)
						iterator->end = endDay;
					++iterator;
				}
			}
		}

		std::vector<ConcomitantEra> SccsConverter::buildConcomitantEras(std::vector<Era>& eras, const int startDay, const int endDay) {
			// Find all unique cut points (where things change)
			std::set<int> cutPoints;
			cutPoints.insert(startDay);
			cutPoints.insert(endDay + 1);
			for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
				cutPoints.insert(era->start);
				cutPoints.insert(era->end + 1);
			}
			std::vector<int> sortedCutPoints(cutPoints.begin(), cutPoints.end());
			std::sort(sortedCutPoints.begin(), sortedCutPoints.end());

			// Build eras for every pair of consecutive cut points
			std::vector<ConcomitantEra> concomitantEras;
			concomitantEras.reserve(sortedCutPoints.size());
			for (unsigned int i = 1; i < sortedCutPoints.size(); i++) {
				ConcomitantEra concomitantEra;
				concomitantEra.start = sortedCutPoints[i - 1];
				concomitantEra.end = sortedCutPoints[i] - 1;
				concomitantEras.push_back(concomitantEra);
			}

			// Determine exposure status during each era
			std::sort(eras.begin(), eras.end()); // Sort by start date
			unsigned int startIndex = 0;
			for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
				int index = startIndex;
				while (concomitantEras[index].start < era->start) {
					index++;
				}
				startIndex = index;
				while (concomitantEras[index].end <= era->end) {
					concomitantEras[index].conceptIds.push_back(era->conceptId);
					index++;
				}
			}
			return concomitantEras;
		}

		void SccsConverter::addToResult(const std::vector<int64_t>& conceptIds, std::map<int64_t, int>& outcomeIdToCount, const int duration,
				const int64_t& observationPeriodId, ResultStruct& resultStruct, const std::set<int64_t>& outcomeIds) {
			// Add to outcome table:
			for (int64_t outcomeId : outcomeIds) {
				resultStruct.outcomeOutcomeId->push_back(outcomeId);
				resultStruct.outcomeY->push_back(outcomeIdToCount[outcomeId]);
				resultStruct.outcomeRowId->push_back(resultStruct.rowId);
				resultStruct.outcomeTime->push_back(duration);
				resultStruct.outcomeStratumId->push_back(observationPeriodId);
			}

			// Add to covariates table:
			for (int64_t conceptId : conceptIds) {
				resultStruct.eraRowId->push_back(resultStruct.rowId);
				resultStruct.eraStratumId->push_back(observationPeriodId);
				resultStruct.eraCovariateId->push_back(conceptId);
			}
			resultStruct.rowId++;
		}

		void SccsConverter::addToResult(std::vector<ConcomitantEra>& concomitantEras, std::vector<Era>& outcomes, const int64_t& observationPeriodId,
				ResultStruct& resultStruct) {
			std::set<int64_t> outcomeIds;
			for (Era era : outcomes)
				outcomeIds.insert(era.conceptId);

			// Sort eras based on covariate pattern:
			for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
				std::sort(era->conceptIds.begin(), era->conceptIds.end());
			}
			std::sort(concomitantEras.begin(), concomitantEras.end(), ConcomitantEraCovariateComparator());

			// Iterate over eras, merging those with similar patterns:
			std::vector<int64_t>* previousPattern = NULL;
			int duration = 0;
			std::map<int64_t, int> outcomeIdToCount;
			for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
				if (previousPattern == NULL || era->conceptIds.size() != previousPattern->size()
						|| !std::equal(era->conceptIds.begin(), era->conceptIds.end(), previousPattern->begin())) {
					if (previousPattern != NULL) {
						addToResult(*previousPattern, outcomeIdToCount, duration, observationPeriodId, resultStruct, outcomeIds);
					}
					previousPattern = &era->conceptIds;
					outcomeIdToCount.clear();
					duration = 0;
				}
				duration += era->end - era->start + 1;
				for (Era outcome : outcomes) {
					if (outcome.start >= era->start && outcome.start <= era->end) {
						std::map<int64_t, int>::iterator found = outcomeIdToCount.find(outcome.conceptId);
						if (found == outcomeIdToCount.end()) {
							outcomeIdToCount[outcome.conceptId] = 1;
						} else {
							(*found).second++;
						}
					}
				}
			}
			if (previousPattern != NULL) {
				addToResult(*previousPattern, outcomeIdToCount, duration, observationPeriodId, resultStruct, outcomeIds);
			}
		}

		void SccsConverter::processPerson(PersonData& personData, ResultStruct& resultStruct, const int covariatePersistencePeriod, const int naivePeriod,
				const bool firstOutcomeOnly) {
			std::cout << "ObsId: " << personData.observationPeriodId << std::endl;
			std::vector<Era> *eras = personData.eras;
			std::sort(eras->begin(), eras->end()); // Sort by start date
			std::vector<Era> outcomes = extractOutcomes(*eras);
			if (firstOutcomeOnly)
				removeAllButFirstOutcome(outcomes);
			int startDay = naivePeriod;
			int endDay = personData.daysOfObservation;

			if (startDay > endDay) // Days of observation is less than the naive period
				return;
			for (std::vector<Era>::iterator outcome = outcomes.begin(); outcome != outcomes.end(); ++outcome) {
				outcome->end = outcome->start;
			}
			for (std::vector<Era>::iterator era = eras->begin(); era != eras->end(); ++era) {
				era->end += covariatePersistencePeriod;
			}

			clipEras(outcomes, startDay, endDay);
			if (outcomes.size() == 0) // We lost all outcomes in the clipping
				return;
			clipEras(*eras, startDay, endDay);
			std::cout << "Check 1" << std::endl;
			std::vector<Era> mergedEras = mergeOverlapping(*eras);
			std::cout << "Check 2" << std::endl;
			std::vector<ConcomitantEra> concomitantEras = buildConcomitantEras(mergedEras, startDay, endDay);
			std::cout << "Check 3" << std::endl;
			addToResult(concomitantEras, outcomes, personData.observationPeriodId, resultStruct);
			std::cout << "Check 4" << std::endl;
		}

		List SccsConverter::convertToSccs(const DataFrame& cases, const DataFrame& eras, const int covariatePersistencePeriod, const int naivePeriod,
				const bool firstOutcomeOnly) {
			std::cout << "Check -2" << std::endl;
			PersonDataIterator iterator(cases, eras);
			std::cout << "Check -1" << std::endl;
			ResultStruct resultStruct;
			std::cout << "Check 0" << std::endl;
			while (iterator.hasNext()) {
				PersonData personData = iterator.next();
				processPerson(personData, resultStruct, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly);
			}

			return resultStruct.convertToRList();
		}

	}
}

#endif /* SCCSCONVERTER_CPP_ */
