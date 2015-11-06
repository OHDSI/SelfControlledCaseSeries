/*
 * @file PersonDataIterator.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2015 Observational Health Data Sciences and Informatics
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
#include "FfdfIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {


PersonDataIterator::PersonDataIterator(const List& _cases, const List& _eras) :
 casesIterator(_cases, false), erasIterator(_eras, true), casesCursor(0), erasCursor(0) {
  loadNextCases();
  loadNextEras();
}

void PersonDataIterator::loadNextCases() {
  List cases = casesIterator.next();
  casesPersonId = cases["personId"];
  casesObservationPeriodId = cases["observationPeriodId"];
  casesObservationDays = cases["observationDays"];
  casesAgeInDays = cases["ageInDays"];
  casesObservationStartYear = cases["observationStartYear"];
  casesObservationStartMonth = cases["observationStartMonth"];
  casesObservationStartDay = cases["observationStartDay"];
  casesUncensored = cases["uncensored"];
}

void PersonDataIterator::loadNextEras() {
  List eras = erasIterator.next();
  erasObservationPeriodId = eras["observationPeriodId"];
  erasStartDay = eras["startDay"];
  erasEndDay = eras["endDay"];
  erasConceptId = eras["conceptId"];
  erasValue = eras["value"];
  erasEraType = eras["eraType"];
}

bool PersonDataIterator::hasNext() {
  return (casesCursor < casesObservationPeriodId.length());
}

PersonData PersonDataIterator::next() {
  int64_t observationPeriodId = casesObservationPeriodId[casesCursor];
  PersonData nextPerson(casesPersonId[casesCursor], observationPeriodId, casesObservationDays[casesCursor], casesAgeInDays[casesCursor],
                        casesObservationStartYear[casesCursor], casesObservationStartMonth[casesCursor], casesObservationStartDay[casesCursor],
                        casesUncensored[casesCursor]);
  casesCursor++;
  if (casesCursor == casesObservationPeriodId.length() && casesIterator.hasNext()){
    loadNextCases();
    casesCursor = 0;
  }
  while (erasObservationPeriodId[erasCursor] == observationPeriodId) {
    Era era(erasStartDay[erasCursor], erasEndDay[erasCursor], erasConceptId[erasCursor], erasValue[erasCursor], erasEraType[erasCursor] == "hoi");
    nextPerson.eras->push_back(era);
    erasCursor++;
    if (erasCursor == erasObservationPeriodId.length()){
      if (erasIterator.hasNext()){
        loadNextEras();
        erasCursor = 0;
      } else {
        break;
      }
    }
  }
  return nextPerson;
}
}
}
#endif /* PERSONDATAITERATOR_CPP_ */
