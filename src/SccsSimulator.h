/*
 * @file SccsSimulator.h
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

#ifndef SCCSSIMULTATOR_H_
#define SCCSSIMULTATOR_H_

#include <random>

#include <Rcpp.h>
using namespace Rcpp;

namespace ohdsi {
namespace sccs {

class SccsSimulator {
public:
  SccsSimulator(const DataFrame& _cases, const DataFrame& _eras, const std::vector<double> _baselineRates, const DataFrame& _eraRrs, const bool _includeAge,
                const int _ageOffset, const std::vector<double> _ageRrs, const bool _includeSeasonality, const std::vector<double> _seasonRrs,
                const bool _includeCalendarTimeEffect, const Date& _minCalendarTime, const std::vector<double> _calendarTimeRrs);
  List simulateOutcomes();
private:
  void processPerson(const int caseIndex, const int eraStartIndex, const int eraEndIndex);
  NumericVector casesCaseId;
  NumericVector casesEndDay;
  NumericVector casesAgeAtObsStart;
  DateVector casesObservationPeriodStartDate;
  NumericVector erasCaseId;
  NumericVector erasEraId;
  NumericVector erasStartDay;
  NumericVector erasEndDay;
  std::vector<int> outcomeCaseIds;
  std::vector<int> outcomeStartDays;
  std::vector<double> baselineRates;
  std::map<int, double> eraIdToRr;
  bool includeAge;
  int ageOffset;
  std::vector<double> ageRrs;
  bool includeSeasonality;
  std::vector<double> seasonRrs;
  bool includeCalendarTimeEffect;
  int minCalendarTime;
  std::vector<double> calendarTimeRrs;
  std::default_random_engine generator;
};
}
}

#endif /* SIMULTATOR_H_ */
