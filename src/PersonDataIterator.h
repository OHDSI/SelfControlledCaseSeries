/*
 * @file PersonDataIterator.h
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2024 Observational Health Data Sciences and Informatics
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
#include "AndromedaTableIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {
struct Era {
  Era(int _start,
      int _end,
      int _eraId,
      double _value) :
  start(_start),
  end(_end),
  eraId(_eraId),
  value(_value) {
  }

  bool operator <(const Era& era) const {
    return (start < era.start);
  }

  int start;
  int end;
  int64_t eraId;
  double value;
};

struct PersonData {
  // Important: PersonData object uses start date of final period to include as
  // reference point, whereas up to now the reference point was the observation
  // period start date (in the database)
  PersonData(String _personId,
             String _observationPeriodId,
             int _caseId,
             Date _obsStartDate,
             int _ageInDays,
             int _startDay,
             int _endDay,
             bool _noninformativeEndCensor) :
  personId(_personId),
  observationPeriodId(_observationPeriodId),
  caseId(_caseId),
  startDay(_startDay),
  endDay(_endDay),
  ageInDays(_ageInDays),
  obsStartYear(_obsStartDate.getYear()),
  obsStartMonth(_obsStartDate.getMonth()),
  obsStartDay(_obsStartDate.getDay()),
  noninformativeEndCensor(_noninformativeEndCensor),
  eras(0),
  outcomes(0) {
    eras = new std::vector<Era>;
    outcomes = new std::vector<Era>;
  }

  ~PersonData() {
    delete eras;
    delete outcomes;
  }

  std::string personId;
  std::string observationPeriodId;
  int caseId;
  int startDay;
  int endDay;
  int ageInDays;
  int obsStartYear;
  int obsStartMonth;
  int obsStartDay;
  bool noninformativeEndCensor;
  std::vector<Era>* eras;
  std::vector<Era>* outcomes;
};

class PersonDataIterator {
public:
  PersonDataIterator(const DataFrame& _cases, const DataFrame& _outcomes, const List& _eras);
  bool hasNext();
  PersonData next();
private:
  AndromedaTableIterator erasIterator;
  CharacterVector casesPersonId;
  CharacterVector casesObservationPeriodId;
  NumericVector casesCaseId;
  DateVector casesObservationPeriodStartDate;
  NumericVector casesAgeAtObsStart;
  NumericVector casesStartDay;
  NumericVector casesEndDay;
  LogicalVector casesNoninformativeEndCensor;

  NumericVector outcomesCaseId;
  NumericVector outcomesOutcomeDay;

  NumericVector erasCaseId;
  NumericVector erasStartDay;
  NumericVector erasEndDay;
  NumericVector erasEraId;
  NumericVector erasValue;
  CharacterVector erasEraType;

  int casesCursor;
  int outcomesCursor;
  int erasCursor;
  void loadNextEras();
};
}
}

#endif /* PERSONDATAITERATOR_H_ */
