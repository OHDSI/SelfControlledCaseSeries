/*
 * @file SccsConvert.h
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

#ifndef SCCSCONVERTER_H_
#define SCCSCONVERTER_H_

#include <Rcpp.h>
#include "PersonDataIterator.h"
using namespace Rcpp;

namespace ohdsi {
	namespace sccs {

		struct SccsConverter {
		public:
			static List convertToSccs(DataFrame& cases, DataFrame& eras, const int covariatePersistencePeriod, const int naivePeriod, bool firstOutcomeOnly);
		private:
			static void processPerson(PersonData &personData, std::vector<Era> &processedEras, const int covariatePersistencePeriod, const int naivePeriod,
					bool firstOutcomeOnly);
			static void clipEras(std::vector<Era> &eras, const int startDay, const int endDay);
			static void removeAllButFirstOutcome(std::vector<Era> &eras);
			static std::vector<Era> mergeOverlapping(const std::vector<Era> &eras);
		};
	}
}

#endif /* SCCSCONVERTER_H_ */
