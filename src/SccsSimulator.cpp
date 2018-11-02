/*
 * @file SccsSimulator.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2018 Observational Health Data Sciences and Informatics
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

#ifndef SCCSSIMULTATOR_CPP_
#define SCCSSIMULTATOR_CPP_

#include <Rcpp.h>
#include <random>
#include <ctime>
#include "SccsSimulator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

SccsSimulator::SccsSimulator(const List& _cases, const List& _eras,  const std::vector<double>& _baselineRates, const List& _eraRrs, const bool _includeAge, const int _ageOffset, const std::vector<double> _ageRrs, const bool _includeSeasonality, const std::vector<double> _seasonRrs) :
baselineRates(_baselineRates), includeAge(_includeAge), ageOffset(_ageOffset), ageRrs(_ageRrs), includeSeasonality(_includeSeasonality), seasonRrs(_seasonRrs) {
  casesObservationPeriodId = _cases["observationPeriodId"];
  casesObservationDays = _cases["observationDays"];
  casesAgeInDays = _cases["ageInDays"];
  casesStartYear = _cases["startYear"];
  casesStartMonth = _cases["startMonth"];
  casesStartDay = _cases["startDay"];
  erasObservationPeriodId = _eras["observationPeriodId"];
  erasConceptId = _eras["conceptId"];
  erasStartDay = _eras["startDay"];
  erasEndDay = _eras["endDay"];
  NumericVector conceptId = _eraRrs["conceptId"];
  NumericVector rr = _eraRrs["rr"];
  for (int i = 0; i < conceptId.size(); i++){
    conceptIdToRr[conceptId[i]] = rr[i];
  }
}

void SccsSimulator::processPerson(const int caseIndex, const int eraStartIndex, const int eraEndIndex){
  double baselineRate = baselineRates[caseIndex];
  int ageAtStart = casesAgeInDays[caseIndex];
  struct tm startDate = {0, 0, 12};
  startDate.tm_year = casesStartYear[caseIndex] - 1900;
  startDate.tm_mon = casesStartMonth[caseIndex] - 1;
  startDate.tm_mday = casesStartDay[caseIndex];
  struct tm startOfYear(startDate);
  startOfYear.tm_mon = 0;
  startOfYear.tm_mday = 1;
  std::time_t time1 = std::mktime(&startDate);
  std::time_t time2 = std::mktime(&startOfYear);
  int startDayOfYear = std::difftime(time1, time2) / (60 * 60 * 24);

  for (int day = 0; day < casesObservationDays[caseIndex]; day++){
    // Start with baseline rate:
    double dailyRate = baselineRate;

    // Multiply by era RRs:
    int previousId = -1;
    for (int eraIndex = eraStartIndex; eraIndex < eraEndIndex; eraIndex++){
      if (erasConceptId[eraIndex] != previousId){
        if (day >= erasStartDay[eraIndex] && day <= erasEndDay[eraIndex]){
          previousId = erasConceptId[eraIndex];
          dailyRate *= conceptIdToRr[previousId];
        }
      }
    }

    // Multiply by age RR:
    if (includeAge){
      int age = ageAtStart + day;
      dailyRate *= ageRrs[age - ageOffset];
    }

    // Multiply by season RR:
    if (includeSeasonality){
      int dayOfYear = (startDayOfYear + day) % 365;
      dailyRate *= seasonRrs[dayOfYear];
    }

    // Sample outcomes:
    std::poisson_distribution<int> distribution(dailyRate);
    int count = distribution(generator);
    for (int i = 0; i < count; i++){
      outcomeObservationPeriodIds.push_back(casesObservationPeriodId[caseIndex]);
      outcomeStartDays.push_back(day);
    }
  }
}

List SccsSimulator::simulateOutcomes(){
  int eraStartIndex = 0;
  int eraEndIndex = 0;
  for (int caseIndex = 0; caseIndex < casesObservationPeriodId.size(); caseIndex++){
    int observationPeriodId = casesObservationPeriodId[caseIndex];
    while (erasObservationPeriodId[eraEndIndex] == observationPeriodId){
      eraEndIndex++;
    }
    processPerson(caseIndex, eraStartIndex, eraEndIndex);
    eraStartIndex = eraEndIndex;
  }
  return List::create(Named("observationPeriodId") = outcomeObservationPeriodIds, Named("startDay") = outcomeStartDays);
}

}
}

#endif /* SIMULTATOR_CPP_ */
