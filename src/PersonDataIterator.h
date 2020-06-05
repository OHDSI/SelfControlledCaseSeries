/*
 * @file PersonDataIterator.h
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
  PersonData(int64_t _personId,
             int64_t _observationPeriodId,
             Date _startDate,
             int _ageInDays,
             int _endDay,
             int _offset,
             bool _noninformativeEndCensor) :
  personId(_personId),
  observationPeriodId(_observationPeriodId),
  endDay(_endDay),
  ageInDays(_ageInDays),
  startYear(_startDate.getYear()),
  startMonth(_startDate.getMonth()),
  startDay(_startDate.getDay()),
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

  int64_t personId;
  int64_t observationPeriodId;
  int endDay;
  int ageInDays;
  int startYear;
  int startMonth;
  int startDay;
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
  NumericVector casesPersonId;
  NumericVector casesObservationPeriodId;
  DateVector casesStartDate;
  NumericVector casesEndDay;
  NumericVector casesAgeInDays;
  NumericVector casesOffset;
  LogicalVector casesNoninformativeEndCensor;

  NumericVector outcomesObservationPeriodId;
  NumericVector outcomesOutcomeDay;

  NumericVector erasObservationPeriodId;
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
