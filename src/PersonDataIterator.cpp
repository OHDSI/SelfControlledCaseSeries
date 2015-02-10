/*
 * @file PersonDataIterator.cpp
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

#ifndef PERSONDATAITERATOR_CPP_
#define PERSONDATAITERATOR_CPP_

#include <Rcpp.h>
#include "PersonDataIterator.h"

using namespace Rcpp;

namespace ohdsi {
	namespace sccs {

		PersonDataIterator::PersonDataIterator(DataFrame &_cases, DataFrame &_eras) : casesCursor(1), erasCursor(1) {
			casesPersonId = _cases["personId"];
			casesObservationPeriodId = _cases["observationPeriodId"];
			casesObservationDays = _cases["observationDays"];
			erasObservationPeriodId = _eras["observationPeriodId"];
			erasStartDay = _eras["startDay"];
			erasEndDay = _eras["endDay"];
			erasConceptId = _eras["conceptId"];
			erasEraType = _eras["eraType"];
		}

		bool PersonDataIterator::hasNext() {
			return (casesCursor <= casesObservationPeriodId.length());
		}

		PersonData PersonDataIterator::next() {
			int observationPeriodId = casesObservationPeriodId[casesCursor];
			std::vector<Era> eras;
			while (erasObservationPeriodId[erasCursor] == observationPeriodId){
				Era era(erasStartDay[erasCursor], erasEndDay[erasCursor], erasConceptId[erasCursor], erasEraType[erasCursor] == "hoi");
				eras.push_back(era);
				erasCursor++;
			}
			PersonData nextPerson(eras, casesPersonId[casesCursor], observationPeriodId, casesObservationDays[casesCursor]);
			casesCursor++;
			return nextPerson;
		}
	}
}
#endif /* PERSONDATAITERATOR_CPP_ */
