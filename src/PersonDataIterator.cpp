/*
 * @file PersonDataIterator.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2025 Observational Health Data Sciences and Informatics
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
 erasIterator(_eras, true, "caseId"), casesObservationPeriodStartDate(0), casesCursor(0), outcomesCursor(0), erasCursor(0) {

  outcomesCaseId = _outcomes["caseId"];
  outcomesOutcomeDay = _outcomes["outcomeDay"];

  casesObservationPeriodId = _cases["observationPeriodId"];
  casesPersonId = _cases["personId"];
  casesCaseId = _cases["caseId"];
  casesObservationPeriodStartDate = _cases["observationPeriodStartDate"];
  casesAgeAtObsStart = _cases["ageAtObsStart"];
  casesStartDay = _cases["startDay"];
  casesEndDay = _cases["endDay"];
  casesNoninformativeEndCensor = _cases["noninformativeEndCensor"];

  loadNextEras();
}


void PersonDataIterator::loadNextEras() {
  List eras = erasIterator.next();
  erasCaseId = eras["caseId"];
  erasStartDay = eras["eraStartDay"];
  erasEndDay = eras["eraEndDay"];
  erasEraId = eras["eraId"];
  erasValue = eras["eraValue"];
  erasEraType = eras["eraType"];
 }

bool PersonDataIterator::hasNext() {
  return (casesCursor < casesCaseId.length());
}

PersonData PersonDataIterator::next() {
  int caseId = casesCaseId[casesCursor];
  PersonData nextPerson(casesPersonId[casesCursor],
                        casesObservationPeriodId[casesCursor],
                        caseId,
                        casesObservationPeriodStartDate[casesCursor],
                        casesAgeAtObsStart[casesCursor],
                        casesStartDay[casesCursor],
                        casesEndDay[casesCursor],
                        casesNoninformativeEndCensor[casesCursor]);
  casesCursor++;

  // Rcpp::Rcout << outcomesCaseId[erasCursor] << " (outcomesCaseId[erasCursor])\n";
  // Rcpp::Rcout << observationPeriodId << " (observationPeriodId)\n";
  while (outcomesCursor < outcomesCaseId.length() && outcomesCaseId[outcomesCursor] == caseId) {
    Era outcome(outcomesOutcomeDay[outcomesCursor],
                outcomesOutcomeDay[outcomesCursor],
                0,
                1.0);
    nextPerson.outcomes -> push_back(outcome);
    outcomesCursor++;
  }
  while (erasCursor < erasCaseId.length() && erasCaseId[erasCursor] < caseId) {
    erasCursor++;
    if (erasCursor == erasCaseId.length()){
      if (erasIterator.hasNext()){
        loadNextEras();
        erasCursor = 0;
      } else {
        break;
      }
    }
  }
  while (erasCursor < erasCaseId.length() && erasCaseId[erasCursor] == caseId) {
    if (erasEraType[erasCursor] != "hoi") {
      Era era(erasStartDay[erasCursor],
              erasEndDay[erasCursor],
              erasEraId[erasCursor],
                       erasValue[erasCursor]);
      nextPerson.eras -> push_back(era);
    }
    erasCursor++;
    if (erasCursor == erasCaseId.length()){
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
