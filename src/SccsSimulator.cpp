/*
 * @file SccsSimulator.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2021 Observational Health Data Sciences and Informatics
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

SccsSimulator::SccsSimulator(const List& _cases, const List& _eras,  const std::vector<double> _baselineRates, const List& _eraRrs,
                             const bool _includeAge, const int _ageOffset, const std::vector<double> _ageRrs, const bool _includeSeasonality,
                             const std::vector<double> _seasonRrs, const bool _includeCalendarTimeEffect, const int _calendarTimeOffset,
                             const std::vector<double> _calendarTimeRrs) : baselineRates(_baselineRates), includeAge(_includeAge), ageOffset(_ageOffset), ageRrs(_ageRrs), includeSeasonality(_includeSeasonality), seasonRrs(_seasonRrs), includeCalendarTimeEffect(_includeCalendarTimeEffect, calendarTimeOffset(_calendarTimeOffset, calendarTimeRrs(_calendarTimeRrs) {
  casesCaseId = _cases["caseId"];
  casesObservationDays = _cases["observationDays"];
  casesAgeInDays = _cases["ageInDays"];
  casesStartYear = _cases["startYear"];
  casesStartMonth = _cases["startMonth"];
  casesStartDay = _cases["startDay"];
  casesStartDate = _cases["startDate"];
  erasCaseId = _eras["caseId"];
  erasEraId = _eras["eraId"];
  erasStartDay = _eras["startDay"];
  erasEndDay = _eras["endDay"];
  NumericVector eraId = _eraRrs["eraId"];
  NumericVector rr = _eraRrs["rr"];
  for (int i = 0; i < eraId.size(); i++){
    eraIdToRr[eraId[i]] = rr[i];
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
  int calendarTimeAtStart = casesStartDate[caseIndex];

  for (int day = 0; day < casesObservationDays[caseIndex]; day++){
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
      int age = ageAtStart + day;
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
      int calendarTime = calendarTimeAtStart + day;
      dailyRate *= calendarTimeRrs[calendarTime - calendarTimeOffset];
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
    while (erasCaseId[eraEndIndex] == caseId){
      eraEndIndex++;
    }
    processPerson(caseIndex, eraStartIndex, eraEndIndex);
    eraStartIndex = eraEndIndex;
  }
  return List::create(Named("caseId") = outcomeCaseIds, Named("startDay") = outcomeStartDays);
}

}
}

#endif /* SIMULTATOR_CPP_ */
