/*
 * @file SccsConvert.cpp
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

		std::vector<Era> SccsConverter::mergeOverlapping(const std::vector<Era> &eras) {
			std::vector<Era> mergedEras;
			std::map<int, Era> conceptIdToRunningEra;
			for (Era era : eras) {
				std::map<int, Era>::iterator found = conceptIdToRunningEra.find(era.conceptId);
				if (found != conceptIdToRunningEra.end()) {
					Era runningEra = conceptIdToRunningEra.at(era.conceptId);
					if (runningEra.end >= era.start) {
						if (runningEra.end < era.end) {
							runningEra.end = era.end;
						}
					} else {
						mergedEras.push_back(runningEra);
						(*found).second = era;
					}
				} else {
					conceptIdToRunningEra.insert(std::pair<int, Era>(era.conceptId, era));
				}
			}
			for (std::map<int, Era>::iterator iterator = conceptIdToRunningEra.begin(); iterator != conceptIdToRunningEra.end(); ++iterator) {
				mergedEras.push_back((*iterator).second);
			}
			return mergedEras;
		}

		void SccsConverter::removeAllButFirstOutcome(std::vector<Era> &eras) {
			std::set<int> seenOutcomes;
			std::vector<Era>::iterator iterator;
			for (iterator = eras.begin(); iterator != eras.end();) {
				if ((*iterator).isOutcome && !seenOutcomes.insert((*iterator).conceptId).second)
					iterator = eras.erase(iterator);
				else
					++iterator;
			}
		}

		void SccsConverter::clipEras(std::vector<Era> &eras, const int startDay, const int endDay) {
			std::vector<Era>::iterator iterator;
			for (iterator = eras.begin(); iterator != eras.end();) {
				if ((*iterator).end < startDay || (*iterator).start > endDay)
					iterator = eras.erase(iterator);
				else {
					if ((*iterator).start < startDay)
						(*iterator).start = startDay;
					if ((*iterator).end > endDay)
						(*iterator).end = endDay;
					++iterator;
				}
			}
		}

		void SccsConverter::processPerson(PersonData &personData, std::vector<Era> &processedEras, const int covariatePersistencePeriod, const int naivePeriod,
				const bool firstOutcomeOnly) {
			std::vector<Era> eras = personData.eras;
			std::sort(eras.begin(), eras.end());
			if (firstOutcomeOnly)
				removeAllButFirstOutcome(eras);
			int startDay = naivePeriod;
			int endDay = personData.daysOfObservation;
			for (Era era : eras) {
				if (era.isOutcome) {
					era.end = era.start;
				} else {
					era.end += covariatePersistencePeriod;
				}
			}
			clipEras(eras, startDay, endDay);
			eras = mergeOverlapping(eras);
			// TODO: combine concomitant eras, add to processedEras
		}

		List SccsConverter::convertToSccs(DataFrame& cases, DataFrame& eras, const int covariatePersistencePeriod, const int naivePeriod,
				const bool firstOutcomeOnly) {
			PersonDataIterator iterator(cases, eras);
			unsigned int i = 0;
			std::vector<Era> processedEras;
			while (iterator.hasNext()) {
				PersonData personData = iterator.next();
				processPerson(personData, processedEras, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly);

				i++;
			}

			IntegerVector a(i);
			IntegerVector b(i);
			IntegerVector c(i);
			DataFrame ndf = DataFrame::create(Named("a") = a, Named("b") = b, Named("c") = c);

			return List::create(Named("outcomes") = ndf);
		}
	}
}

#endif /* SCCSCONVERTER_CPP_ */
