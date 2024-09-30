/*
 * @file SccsSimulator.cpp
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

#ifndef SCCSSIMULTATOR_CPP_
#define SCCSSIMULTATOR_CPP_

#include <Rcpp.h>
#include <random>
#include <ctime>
#include "SccsSimulator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

SccsSimulator::SccsSimulator(const DataFrame& _cases, const DataFrame& _eras,  const std::vector<double> _baselineRates, const DataFrame& _eraRrs,
                             const bool _includeAge, const int _ageOffset, const std::vector<double> _ageRrs, const bool _includeSeasonality,
                             const std::vector<double> _seasonRrs, const bool _includeCalendarTimeEffect, const Date& _minCalendarTime,
                             const std::vector<double> _calendarTimeRrs) : casesObservationPeriodStartDate(0), baselineRates(_baselineRates), includeAge(_includeAge),
                             ageOffset(_ageOffset), ageRrs(_ageRrs), includeSeasonality(_includeSeasonality), seasonRrs(_seasonRrs),
                             includeCalendarTimeEffect(_includeCalendarTimeEffect), minCalendarTime(_minCalendarTime),
                             calendarTimeRrs(_calendarTimeRrs) {
  casesCaseId = _cases["caseId"];
  casesEndDay = _cases["endDay"];
  casesAgeAtObsStart = _cases["ageAtObsStart"];
  casesObservationPeriodStartDate = _cases["observationPeriodStartDate"];
  erasCaseId = _eras["caseId"];
  erasEraId = _eras["eraId"];
  erasStartDay = _eras["eraStartDay"];
  erasEndDay = _eras["eraEndDay"];
  NumericVector eraId = _eraRrs["eraId"];
  NumericVector rr = _eraRrs["rr"];
  for (int i = 0; i < eraId.size(); i++){
    eraIdToRr[eraId[i]] = rr[i];
  }
}

void SccsSimulator::processPerson(const int caseIndex, const int eraStartIndex, const int eraEndIndex){
  double baselineRate = baselineRates[caseIndex];
  int ageAtObsStart = casesAgeAtObsStart[caseIndex];
  Date startDate = casesObservationPeriodStartDate[caseIndex];
  int startDayOfYear = startDate.getYearday() - 1;
  int calendarIndexAtStart = startDate - (Date)minCalendarTime;

  for (int day = 0; day < casesEndDay[caseIndex]; day++){
    // Start with baseline rate:
    double dailyRate = baselineRate;

    // Multiply by era RRs:
    int previousId = -1;
    for (int eraIndex = eraStartIndex; eraIndex < eraEndIndex; eraIndex++){
      if (erasEraId[eraIndex] != previousId){
        if (day >= erasStartDay[eraIndex] && day <= erasEndDay[eraIndex]){
          previousId = erasEraId[eraIndex];
          dailyRate *= eraIdToRr[previousId];
        }
      }
    }

    // Multiply by age RR:
    if (includeAge){
      int age = ageAtObsStart + day;
      dailyRate *= ageRrs[age - ageOffset];
    }

    // Multiply by season RR:
    if (includeSeasonality){
      int dayOfYear = (startDayOfYear + day) % 365;
      // Rcout << dayOfYear << " " << seasonRrs[dayOfYear] << "\n";
      dailyRate *= seasonRrs[dayOfYear];
    }

    // Multiply by calendar time RR:
    if (includeCalendarTimeEffect){
      dailyRate *= calendarTimeRrs[calendarIndexAtStart + day];
    }

    // Sample outcomes:
    std::poisson_distribution<int> distribution(dailyRate);
    int count = distribution(generator);
    // Rcout << dailyRate << " " << count << "\n";
    for (int i = 0; i < count; i++){
      // Rcout << caseIndex << " " << casesCaseId[caseIndex] << " " << day << "\n";
      outcomeCaseIds.push_back(casesCaseId[caseIndex]);
      outcomeStartDays.push_back(day);
    }
  }
}

List SccsSimulator::simulateOutcomes(){
  int eraStartIndex = 0;
  int eraEndIndex = 0;
  for (int caseIndex = 0; caseIndex < casesCaseId.size(); caseIndex++){
    int caseId = casesCaseId[caseIndex];
    while (eraEndIndex < erasCaseId.size() && erasCaseId[eraEndIndex] < caseId){
      eraEndIndex++;
    }
    eraStartIndex = eraEndIndex;
    while (eraEndIndex < erasCaseId.size() && erasCaseId[eraEndIndex] == caseId){
      eraEndIndex++;
    }
    processPerson(caseIndex, eraStartIndex, eraEndIndex);
  }
  return List::create(Named("caseId") = outcomeCaseIds, Named("startDay") = outcomeStartDays);
}

}
}

#endif /* SIMULTATOR_CPP_ */
