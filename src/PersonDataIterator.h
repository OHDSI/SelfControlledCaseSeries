/*
 * @file PersonDataIterator.h
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

#ifndef PERSONDATAITERATOR_H_
#define PERSONDATAITERATOR_H_

#include <Rcpp.h>

using namespace Rcpp;

namespace ohdsi {
	namespace sccs {
		struct Era {
			Era(int _start, int _end, int _conceptId, bool _isOutcome) :
					start(_start), end(_end), conceptId(_conceptId), isOutcome(_isOutcome) {
			}

			bool operator <(const Era& era) const {
				return (start < era.start);
			}

			int start;
			int end;
			int conceptId;
			bool isOutcome;
		};

		struct PersonData {
			PersonData(std::vector<Era> _eras, int _personId, int _observationPeriodId, int _daysOfObservation) :
					eras(_eras), personId(_personId), observationPeriodId(_observationPeriodId), daysOfObservation(_daysOfObservation) {
			}

			std::vector<Era> eras;
			int personId;
			int observationPeriodId;
			int daysOfObservation;
		};

		class PersonDataIterator {
		public:
			PersonDataIterator(DataFrame& _cases, DataFrame& _eras);
			bool hasNext();
			PersonData next();
		private:
			IntegerVector casesObservationPeriodId;
			IntegerVector casesPersonId;
			IntegerVector casesObservationDays;
			IntegerVector erasObservationPeriodId;
			IntegerVector erasStartDay;
			IntegerVector erasEndDay;
			IntegerVector erasConceptId;
			CharacterVector erasEraType;
			int casesCursor;
			int erasCursor;
		};
	}
}

#endif /* PERSONDATAITERATOR_H_ */
