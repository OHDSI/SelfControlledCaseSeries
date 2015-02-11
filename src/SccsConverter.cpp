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
			std::map<int, Era> conceptIdToRunningEra;
			for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
				std::map<int, Era>::iterator found = conceptIdToRunningEra.find(era->conceptId);
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
			for (std::map<int, Era>::iterator iterator = conceptIdToRunningEra.begin(); iterator != conceptIdToRunningEra.end(); ++iterator) {
				mergedEras.push_back(iterator->second);
			}
			return mergedEras;
		}

		void SccsConverter::removeAllButFirstOutcome(std::vector<Era>& outcomes) {
			std::set<int> seenOutcomes;
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

		void SccsConverter::addFinishedEras(const int day, std::vector<ConcomitantEra>& runningEras, std::vector<ConcomitantEra>& concomittantEras) {
			std::vector<ConcomitantEra>::iterator iterator;
			for (iterator = runningEras.begin(); iterator != runningEras.end();) {
				if (iterator->end < day) {
					concomittantEras.push_back(*iterator);
					iterator = runningEras.erase(iterator);
				} else {
					++iterator;
				}
			}
		}

		void SccsConverter::compareToRunningEras(ConcomitantEra& newEra, std::vector<ConcomitantEra>& runningEras) {
			std::vector<ConcomitantEra> partialEras;
			partialEras.push_back(newEra);
			int end = runningEras.size();
			for (int i = 0; i < end; i++) {
				ConcomitantEra* runningEra = &runningEras.at(i);
				std::vector<ConcomitantEra> newPartialEras;
				std::vector<ConcomitantEra>::iterator partialEra;
				for (partialEra = partialEras.begin(); partialEra != partialEras.end();) {
					if (partialEra->start <= runningEra->end && partialEra->end >= runningEra->start) {
						int startOld = runningEra->start;
						int startNew = partialEra->start;
						int endOld = runningEra->end;
						int endNew = partialEra->end;
						if (startNew < startOld) {
							ConcomitantEra extraEra(*partialEra);
							extraEra.end = startOld - 1;
							newPartialEras.push_back(extraEra);
						}
						if (startNew > startOld) {
							ConcomitantEra extraEra(*runningEra);
							extraEra.end = startNew - 1;
							newPartialEras.push_back(extraEra);
						}
						if (endNew > endOld) {
							ConcomitantEra extraEra(*partialEra);
							extraEra.end = endNew;
							extraEra.start = endOld + 1;
							newPartialEras.push_back(extraEra);
						}
						if (endNew < endOld) {
							ConcomitantEra extraEra(*runningEra);
							extraEra.end = endOld;
							extraEra.start = endNew - 1;
							newPartialEras.push_back(extraEra);
						}
						runningEra->start = std::max(startNew, startOld);
						runningEra->end = std::min(endNew, endOld);
						runningEra->conceptIds.insert(runningEra->conceptIds.end(), partialEra->conceptIds.begin(), partialEra->conceptIds.end());
						partialEra = partialEras.erase(partialEra);
					} else {
						partialEra++;
					}

				}
				partialEras.insert(partialEras.end(), newPartialEras.begin(), newPartialEras.end());
			}
			for (ConcomitantEra partialEra : partialEras) {
				if (partialEra.end - partialEra.start >= 0) {
					runningEras.push_back(partialEra);
				}
			}
		}

		std::vector<ConcomitantEra> SccsConverter::buildConcomitantEras(std::vector<Era>& eras) {
			std::vector<ConcomitantEra> concomitantEras;
			std::vector<ConcomitantEra> runningEras;
			for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
				ConcomitantEra concomitantEra(*era);
				compareToRunningEras(concomitantEra, runningEras);
				addFinishedEras(concomitantEra.start, runningEras, concomitantEras);
			}
			addFinishedEras(std::numeric_limits<int>::max(), runningEras, concomitantEras);
			return concomitantEras;
		}

		void SccsConverter::addNonExposure(std::vector<ConcomitantEra>& concomitantEras, const int _start, const int end) {
			int start = _start;
			std::vector<ConcomitantEra> emptyEras;
			std::sort(concomitantEras.begin(), concomitantEras.end()); // Sort by start date
			for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
				if (era->start <= end) {
					if (era->start > start) {
						ConcomitantEra emptyEra;
						emptyEra.start = start;
						emptyEra.end = era->start - 1;
						emptyEras.push_back(emptyEra);
					}
					if (era->end >= start)
						start = era->end + 1;
				}
			}
			if (start <= end) {
				ConcomitantEra emptyEra;
				emptyEra.start = start;
				emptyEra.end = end;
				emptyEras.push_back(emptyEra);
			}
			concomitantEras.insert(concomitantEras.end(), emptyEras.begin(), emptyEras.end());
		}

		std::set<int> SccsConverter::extractOutcomeIds(const DataFrame& eras) {
			std::set<int> outcomeIds;
			IntegerVector conceptId = eras["conceptId"];
			CharacterVector eraType = eras["eraType"];
			for (int i = 0; i < conceptId.size(); i++) {
				if (eraType[i] == "hoi")
					outcomeIds.insert(conceptId[i]);
			}
			return outcomeIds;
		}

		void SccsConverter::addToResult(const std::vector<int>& conceptIds, std::map<int, int>& outcomeIdToCount, const int duration,
				const int& observationPeriodId, ResultStruct& resultStruct, const std::set<int>& outcomeIds) {
			// Add to outcome table:
			for (int outcomeId : outcomeIds) {
				int count = outcomeIdToCount[outcomeId];
				//std::map<int, int>::iterator found = outcomeIdToCount.find(outcomeId);
				//if (found->second)
				//		count = found->first;
				//	else
				//	count = 0;

				resultStruct.outcomeOutcomeId->push_back(outcomeId);
				resultStruct.outcomeY->push_back(count);
				resultStruct.outcomeRowId->push_back(resultStruct.rowId);
				resultStruct.outcomeTime->push_back(duration);
				resultStruct.outcomeStratumId->push_back(observationPeriodId);
			}

			// Add to covariates table:
			for (int conceptId : conceptIds) {
				resultStruct.eraRowId->push_back(resultStruct.rowId);
				resultStruct.eraStratumId->push_back(observationPeriodId);
				resultStruct.eraCovariateId->push_back(conceptId);
			}
			resultStruct.rowId++;
		}

		void SccsConverter::addToResult(std::vector<ConcomitantEra>& concomitantEras, std::vector<Era>& outcomes, const int& observationPeriodId,
				ResultStruct& resultStruct, const std::set<int>& outcomeIds) {
			// Sort eras based on covariate pattern:
			for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
				std::sort(era->conceptIds.begin(), era->conceptIds.end());
			}
			std::sort(concomitantEras.begin(), concomitantEras.end(), ConcomitantEraCovariateComparator());

			// Iterate over eras, merging those with similar patterns:
			std::vector<int>* previousPattern = NULL;
			int duration = 0;
			std::map<int, int> outcomeIdToCount;
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
						std::map<int, int>::iterator found = outcomeIdToCount.find(outcome.conceptId);
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

		void SccsConverter::processPerson(PersonData& personData, ResultStruct& resultStruct, const std::set<int>& outcomeIds,
				const int covariatePersistencePeriod, const int naivePeriod, const bool firstOutcomeOnly) {
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
			std::cout << "check 1";
			std::vector<Era> mergedEras = mergeOverlapping(*eras);
			std::cout << "check 2";
			std::vector<ConcomitantEra> concomitantEras = buildConcomitantEras(mergedEras);
			std::cout << "check 3";
			addNonExposure(concomitantEras, startDay, endDay);
			std::cout << "check 4";
			addToResult(concomitantEras, outcomes, personData.observationPeriodId, resultStruct, outcomeIds);
			std::cout << "check 5";
		}

		List SccsConverter::convertToSccs(const DataFrame& cases, const DataFrame& eras, const int covariatePersistencePeriod, const int naivePeriod,
				const bool firstOutcomeOnly) {

			std::set<int> outcomeIds = extractOutcomeIds(eras);
			PersonDataIterator iterator(cases, eras);
			ResultStruct resultStruct;
			while (iterator.hasNext()) {
				PersonData personData = iterator.next();
				processPerson(personData, resultStruct, outcomeIds, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly);
			}

			return resultStruct.convertToRList();
		}

	}
}

#endif /* SCCSCONVERTER_CPP_ */
