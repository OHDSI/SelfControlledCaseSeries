/*
 * @file SccsConverter.cpp
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

#ifndef SCCSCONVERTER_CPP_
#define SCCSCONVERTER_CPP_

#include <ctime>
#include <Rcpp.h>
#include "SccsConverter.h"
#include "PersonDataIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

std::vector<Era> SccsConverter::extractOutcomes(std::vector<Era>& eras) {
  std::vector<Era> outcomes;
  std::vector<Era>::iterator iterator;
  for (iterator = eras.begin(); iterator != eras.end();) {
    if ((*iterator).isOutcome) {
      outcomes.push_back((*iterator));
      iterator = eras.erase(iterator);
    } else {
      ++iterator;
    }
  }
  return outcomes;
}

std::vector<Era> SccsConverter::mergeOverlapping(std::vector<Era>& eras) {
  std::vector<Era> mergedEras;
  std::map<int64_t, Era> conceptIdToRunningEra;
  for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
    std::map<int64_t, Era>::iterator found = conceptIdToRunningEra.find(era->conceptId);
    if (found != conceptIdToRunningEra.end()) {
      Era* runningEra = &(*found).second;
      if (runningEra->end >= era->start) {
        if (runningEra->end < era->end) {
          runningEra->end = era->end;
        }
      } else {
        mergedEras.push_back(*runningEra);
        (*found).second = *era;
      }
    } else {
      conceptIdToRunningEra.insert(std::pair<int, Era>(era->conceptId, *era));
    }
  }
  for (std::map<int64_t, Era>::iterator iterator = conceptIdToRunningEra.begin(); iterator != conceptIdToRunningEra.end(); ++iterator) {
    mergedEras.push_back(iterator->second);
  }
  return mergedEras;
}

void SccsConverter::removeAllButFirstOutcome(std::vector<Era>& outcomes) {
  std::set<int64_t> seenOutcomes;
  std::vector<Era>::iterator iterator;
  for (iterator = outcomes.begin(); iterator != outcomes.end();) {
    if (!seenOutcomes.insert((*iterator).conceptId).second)
      iterator = outcomes.erase(iterator);
    else
      ++iterator;
  }
}

void SccsConverter::clipEras(std::vector<Era>& eras, const int startDay, const int endDay) {
  std::vector<Era>::iterator iterator;
  for (iterator = eras.begin(); iterator != eras.end();) {
    if ((*iterator).end < startDay || (*iterator).start > endDay) {
      iterator = eras.erase(iterator);
    } else {
      if (iterator->start < startDay)
        iterator->start = startDay;
      if (iterator->end > endDay)
        iterator->end = endDay;
      ++iterator;
    }
  }
}

std::vector<ConcomitantEra> SccsConverter::buildConcomitantEras(std::vector<Era>& eras, const int startDay, const int endDay) {
  // Find all unique cut points (where things change)
  std::set<int> cutPoints;
  cutPoints.insert(startDay);
  cutPoints.insert(endDay + 1);
  for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
    cutPoints.insert(era->start);
    cutPoints.insert(era->end + 1);
  }
  std::vector<int> sortedCutPoints(cutPoints.begin(), cutPoints.end());
  std::sort(sortedCutPoints.begin(), sortedCutPoints.end());

  // Build eras for every pair of consecutive cut points
  std::vector<ConcomitantEra> concomitantEras;
  concomitantEras.reserve(sortedCutPoints.size());
  for (unsigned int i = 1; i < sortedCutPoints.size(); i++) {
    ConcomitantEra concomitantEra;
    concomitantEra.start = sortedCutPoints[i - 1];
    concomitantEra.end = sortedCutPoints[i] - 1;
    concomitantEras.push_back(concomitantEra);
  }

  // Determine exposure status during each era
  std::sort(eras.begin(), eras.end()); // Sort by start date
  unsigned int startIndex = 0;
  for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
    unsigned int index = startIndex;
    while (index < concomitantEras.size() && concomitantEras[index].start < era->start) {
      index++;
    }
    startIndex = index;
    while (index < concomitantEras.size() && concomitantEras[index].end <= era->end && index < concomitantEras.size()) {
      concomitantEras[index].conceptIdToValue[era->conceptId] = era->value;
      index++;
    }
  }
  return concomitantEras;
}

void SccsConverter::addToResult(const ConcomitantEra& era, std::map<int64_t, int>& outcomeIdToCount, const int duration,
                                const int64_t& observationPeriodId, ResultStruct& resultStruct, const std::set<int64_t>& outcomeIds) {
  // Add to outcome table:
  for (int64_t outcomeId : outcomeIds) {
    resultStruct.addToOutcomes(outcomeId, outcomeIdToCount[outcomeId], duration, observationPeriodId);
  }

  // Add to covariates table:
  for(std::map<int64_t, double>::const_iterator iterator = era.conceptIdToValue.begin(); iterator != era.conceptIdToValue.end(); iterator++) {
    resultStruct.addToCovariates(observationPeriodId, iterator->first, iterator->second);
  }
  resultStruct.incRowId();
}

void SccsConverter::addToResult(std::vector<ConcomitantEra>& concomitantEras, std::vector<Era>& outcomes, const int64_t& observationPeriodId,
                                ResultStruct& resultStruct) {
  std::set<int64_t> outcomeIds;
  for (Era era : outcomes)
    outcomeIds.insert(era.conceptId);

  // Sort eras based on covariate pattern:
  std::sort(concomitantEras.begin(), concomitantEras.end(), ConcomitantEraCovariateComparator());

  // Iterate over eras, merging those with similar patterns:
  ConcomitantEra* previousPattern = NULL;
  int duration = 0;
  std::map<int64_t, int> outcomeIdToCount;
  for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
    if (previousPattern == NULL || era->conceptIdToValue.size() != previousPattern->conceptIdToValue.size() ||
        !std::equal(era->conceptIdToValue.begin(), era->conceptIdToValue.end(), previousPattern->conceptIdToValue.begin())) {
      if (previousPattern != NULL) {
        addToResult(*previousPattern, outcomeIdToCount, duration, observationPeriodId, resultStruct, outcomeIds);
      }
      previousPattern = &*era;
      outcomeIdToCount.clear();
      duration = 0;
    }
    duration += era->end - era->start + 1;
    for (Era outcome : outcomes) {
      if (outcome.start >= era->start && outcome.start <= era->end) {
        std::map<int64_t, int>::iterator found = outcomeIdToCount.find(outcome.conceptId);
        if (found == outcomeIdToCount.end()) {
          outcomeIdToCount[outcome.conceptId] = 1;
        } else {
          (*found).second++;
        }
      }
    }
  }
  if (previousPattern != NULL) {
    addToResult(*previousPattern, outcomeIdToCount, duration, observationPeriodId, resultStruct, outcomeIds);
  }
}

struct tm SccsConverter::addMonth(const struct tm &date) {
  struct tm newDate(date);
  if (newDate.tm_mon == 11){
    newDate.tm_mon = 0;
    newDate.tm_year++;
  } else {
    newDate.tm_mon++;
  }
  return newDate;
}

int SccsConverter::dateDifference(struct tm &date1, struct tm &date2) {
  std::time_t time1 = std::mktime(&date1);
  std::time_t time2 = std::mktime(&date2);
  int difference = std::difftime(time1, time2) / (60 * 60 * 24);
  return difference;
}

void SccsConverter::addMonthEras(std::vector<Era>& eras, const int startDay, const int endDay, const PersonData& personData, const int ageOffset, const NumericMatrix& ageDesignMatrix){
  struct tm startDate = { 0, 0, 12 } ;
  startDate.tm_year = personData.observationStartYear - 1900;
  startDate.tm_mon = personData.observationStartMonth - 1;
  startDate.tm_mday = personData.observationStartDay + startDay;
  mktime(&startDate); //Normalize after adding days
  struct tm startOfMonth(startDate);
  startOfMonth.tm_mday = 1;
  struct tm startOfNextMonth = addMonth(startOfMonth);
  int eraStartDay = startDay;
  int nextEraStartDay = std::min(startDay + dateDifference(startOfNextMonth, startDate), endDay + 1);
  while (eraStartDay <= endDay) {
     int ageIndex = personData.ageInDays + eraStartDay - ageOffset;
     if (ageIndex < 0) {
       ageIndex = 0;
     } else if (ageIndex >= ageDesignMatrix.nrow()) {
       ageIndex = ageDesignMatrix.nrow() - 1;
     }
     for (int i = 0; i < ageDesignMatrix.ncol(); i++){
       Era era(eraStartDay, nextEraStartDay - 1, ageIdOffset + i, ageDesignMatrix(ageIndex, i), false);
       eras.push_back(era);
       //std::cout << eraStartDay << ", " << nextEraStartDay << "\n";
     }
     eraStartDay = nextEraStartDay;
     startOfNextMonth = addMonth(startOfNextMonth);
     nextEraStartDay = std::min(startDay + dateDifference(startOfNextMonth, startDate), endDay + 1);
  }
}

void SccsConverter::processPerson(PersonData& personData, ResultStruct& resultStruct, const int covariateStart, const int covariatePersistencePeriod, const int naivePeriod,
                                  const bool firstOutcomeOnly, const bool includeAge, const int ageOffset, const NumericMatrix& ageDesignMatrix) {
  std::vector<Era> *eras = personData.eras;
  std::sort(eras->begin(), eras->end()); // Sort by start date
  std::vector<Era> outcomes = extractOutcomes(*eras);
  if (firstOutcomeOnly)
    removeAllButFirstOutcome(outcomes);
  int startDay = naivePeriod;
  int endDay = personData.daysOfObservation;

  if (startDay > endDay) // Days of observation is less than the naive period
    return;
  for (std::vector<Era>::iterator outcome = outcomes.begin(); outcome != outcomes.end(); ++outcome) {
    outcome->end = outcome->start;
  }
  for (std::vector<Era>::iterator era = eras->begin(); era != eras->end(); ++era) {
    era->start += covariateStart;
    era->end += covariatePersistencePeriod;
  }

  clipEras(outcomes, startDay, endDay);
  if (outcomes.size() == 0) // We lost all outcomes in the clipping
    return;
  clipEras(*eras, startDay, endDay);
  std::vector<Era> mergedEras = mergeOverlapping(*eras);
  if (includeAge){
    addMonthEras(mergedEras, startDay, endDay, personData, ageOffset, ageDesignMatrix);
  }
  std::vector<ConcomitantEra> concomitantEras = buildConcomitantEras(mergedEras, startDay, endDay);
  addToResult(concomitantEras, outcomes, personData.observationPeriodId, resultStruct);
}

List SccsConverter::convertToSccs(const List& cases, const List& eras, const int covariateStart, const int covariatePersistencePeriod, const int naivePeriod,
                                  bool firstOutcomeOnly, const bool includeAge, const int ageOffset, const NumericMatrix& ageDesignMatrix) {
  PersonDataIterator iterator(cases, eras);
  ResultStruct resultStruct;
  while (iterator.hasNext()) {
    PersonData personData = iterator.next();
    processPerson(personData, resultStruct, covariateStart, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly, includeAge, ageOffset, ageDesignMatrix);
  }

  return resultStruct.convertToRList();
}

}
}

#endif /* SCCSCONVERTER_CPP_ */
