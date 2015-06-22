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
			int64_t conceptId;
			bool isOutcome;
		};

		struct PersonData {
			PersonData(int64_t _personId, int64_t _observationPeriodId, int _daysOfObservation) :
					personId(_personId), observationPeriodId(_observationPeriodId), daysOfObservation(_daysOfObservation) {
				eras = new std::vector<Era>;
			}

			~PersonData() {
				delete eras;
			}

			std::vector<Era> *eras;
			int64_t personId;
			int64_t observationPeriodId;
			int daysOfObservation;
		};

		class PersonDataIterator {
		public:
			PersonDataIterator(const List& _cases, const List& _eras);
			bool hasNext();
			PersonData next();
		private:
			NumericVector casesPersonId;
			NumericVector casesObservationPeriodId;
			NumericVector casesObservationDays;
			NumericVector erasObservationPeriodId;
			NumericVector erasStartDay;
			NumericVector erasEndDay;
			NumericVector erasConceptId;
			CharacterVector erasEraType;
			int casesCursor;
			int erasCursor;
		};
	}
}

#endif /* PERSONDATAITERATOR_H_ */
