/*
 * @file PersonDataIterator.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2020 Observational Health Data Sciences and Informatics
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
#include "AndromedaTableIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {


PersonDataIterator::PersonDataIterator(const DataFrame& _cases, const DataFrame& _outcomes, const List& _eras) :
 erasIterator(_eras, true), casesStartDate(0), casesCursor(0), outcomesCursor(0), erasCursor(0) {

  outcomesObservationPeriodId = _outcomes["observationPeriodId"];
  outcomesOutcomeDay = _outcomes["outcomeDay"];

  casesPersonId = _cases["personId"];
  casesObservationPeriodId = _cases["observationPeriodId"];
  casesEndDay = _cases["endDay"];
  casesAgeInDays = _cases["ageInDays"];
  casesOffset = _cases["offset"];
  casesNoninformativeEndCensor = _cases["noninformativeEndCensor"];
  casesStartDate = _cases["startDate"];

  loadNextEras();
}


void PersonDataIterator::loadNextEras() {
  List eras = erasIterator.next();
  erasObservationPeriodId = eras["observationPeriodId"];
  erasStartDay = eras["startDay"];
  erasEndDay = eras["endDay"];
  erasEraId = eras["eraId"];
  erasValue = eras["value"];
  erasEraType = eras["eraType"];
 }

bool PersonDataIterator::hasNext() {
  return (casesCursor < casesObservationPeriodId.length());
}

PersonData PersonDataIterator::next() {
  int64_t observationPeriodId = casesObservationPeriodId[casesCursor];
  int offset = casesOffset[casesCursor];
  PersonData nextPerson(casesPersonId[casesCursor],
                        observationPeriodId,
                        0,
                        casesAgeInDays[casesCursor],
                        casesEndDay[casesCursor],
                        offset,
                        casesNoninformativeEndCensor[casesCursor]);
  casesCursor++;

  // Rcpp::Rcout << outcomesObservationPeriodId[erasCursor] << " (outcomesObservationPeriodId[erasCursor])\n";
  // Rcpp::Rcout << observationPeriodId << " (observationPeriodId)\n";
  while (outcomesCursor < outcomesObservationPeriodId.length() && outcomesObservationPeriodId[outcomesCursor] == observationPeriodId) {
    Era outcome(outcomesOutcomeDay[outcomesCursor], outcomesOutcomeDay[outcomesCursor], 0, 1.0);
    nextPerson.outcomes -> push_back(outcome);
    outcomesCursor++;
  }
  while (erasCursor < erasObservationPeriodId.length() && erasObservationPeriodId[erasCursor] < observationPeriodId) {
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
  while (erasCursor < erasObservationPeriodId.length() && erasObservationPeriodId[erasCursor] == observationPeriodId) {
    if (erasEraType[erasCursor] != "hoi") {
      Era era(erasStartDay[erasCursor] - offset, erasEndDay[erasCursor] - offset, erasEraId[erasCursor], erasValue[erasCursor]);
      nextPerson.eras -> push_back(era);
    }
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
