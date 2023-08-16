/*
 * @file SccsConverter.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2023 Observational Health Data Sciences and Informatics
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
#include "NumericIntegration.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

SccsConverter::SccsConverter(const DataFrame& _cases,
                             const DataFrame& _outcomes,
                             const List& _eras,
                             const bool _includeAge,
                             const int _ageOffset,
                             const Rcpp::NumericMatrix& _ageDesignMatrix,
                             const bool _includeSeason,
                             const NumericMatrix& _seasonDesignMatrix,
                             const bool _includeCalendarTime,
                             const int _calendarTimeOffset,
                             const NumericMatrix& _calendarTimeDesignMatrix,
                             const NumericVector& _timeCovariateCases,
                             const List& _covariateSettingsList,
                             const bool _eventDependentObservation,
                             const List& _censorModel,
                             const bool _scri,
                             const int64_t _controlIntervalId,
                             const S4 _resultAndromeda) :
                             personDataIterator(_cases, _outcomes, _eras),
                             resultStruct(_resultAndromeda),
                             includeAge(_includeAge),
                             ageOffset(_ageOffset),
                             includeSeason(_includeSeason),
                             includeCalendarTime(_includeCalendarTime),
                             calendarTimeOffset(_calendarTimeOffset),
                             eventDependentObservation(_eventDependentObservation),
                             scri(_scri),
                             controlIntervalId(_controlIntervalId){

                               ageDesignMatrix = _ageDesignMatrix;
                               seasonDesignMatrix = _seasonDesignMatrix;
                               calendarTimeDesignMatrix = _calendarTimeDesignMatrix;

                               for (int i = 0; i < _covariateSettingsList.size(); i++) {
                                 CovariateSettings covariateSettings(as<List>(_covariateSettingsList[i]));
                                 covariateSettingsVector.push_back(covariateSettings);
                               }

                               if (eventDependentObservation) {
                                 std::vector<double> p = _censorModel["p"];
                                 double model = _censorModel["model"];
                                 if (model == 1){
                                   weightFunction = new WsmallEwad2(p);
                                 } else if (model == 2){
                                   weightFunction = new WsmallEwid2(p);
                                 } else if (model == 3){
                                   weightFunction = new WsmallEgad2(p);
                                 } else if (model == 4){
                                   weightFunction = new WsmallEgid2(p);
                                 }
                               }
                               if (_timeCovariateCases.size() != 0) {
                                 hasTimeCovariateCases = true;
                                 for (int i = 0; i < _timeCovariateCases.size(); i++) {
                                   timeCovariateCases.insert(_timeCovariateCases[i]);
                                 }
                               } else
                                 hasTimeCovariateCases = false;
                             }

std::vector<Era> SccsConverter::mergeOverlapping(std::vector<Era>& eras) {
  std::vector<Era> mergedEras;
  std::map<int64_t, Era> eraIdToRunningEra;
  for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
    std::map<int64_t, Era>::iterator found = eraIdToRunningEra.find(era->eraId);
    if (found != eraIdToRunningEra.end()) {
      Era* runningEra = &(found->second);
      if (runningEra->end >= era->start - 1) {
        if (runningEra->end < era->end) {
          runningEra->end = era->end;
        }
      } else {
        mergedEras.push_back(*runningEra);
        found->second = *era;
      }
    } else {
      eraIdToRunningEra.insert(std::pair<int, Era>(era->eraId, *era));
    }
  }
  for (std::map<int64_t, Era>::iterator iterator = eraIdToRunningEra.begin(); iterator != eraIdToRunningEra.end(); ++iterator) {
    mergedEras.push_back(iterator->second);
  }
  return mergedEras;
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
      concomitantEras[index].eraIdToValue[era->eraId] = era->value;
      index++;
    }
  }
  return concomitantEras;
}

void SccsConverter::addToResult(const ConcomitantEra& era, int outcomeCount, const double duration, const int64_t& caseId) {
  // Add to outcome table:
  resultStruct.addToOutcomes(outcomeCount, duration, caseId);


  // Add to covariates table:
  for(std::map<int64_t, double>::const_iterator iterator = era.eraIdToValue.begin(); iterator != era.eraIdToValue.end(); iterator++) {
    resultStruct.addToCovariates(caseId, iterator->first, iterator->second);
  }
  resultStruct.incRowId();
}

void SccsConverter::addToResult(std::vector<ConcomitantEra>& concomitantEras, std::vector<Era>& outcomes, const int64_t& caseId) {
  // Sort eras based on covariate pattern:
  std::sort(concomitantEras.begin(), concomitantEras.end(), ConcomitantEraCovariateComparator());

  // Iterate over eras, merging those with similar patterns:
  ConcomitantEra* previousPattern = NULL;
  double duration = 0;
  int outcomeCount = 0;
  for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
    if (previousPattern == NULL || era->eraIdToValue.size() != previousPattern->eraIdToValue.size() ||
        !std::equal(era->eraIdToValue.begin(), era->eraIdToValue.end(), previousPattern->eraIdToValue.begin())) {
        if (previousPattern != NULL) {
          addToResult(*previousPattern, outcomeCount, duration, caseId);
        }
        previousPattern = &*era;
        outcomeCount = 0;
        duration = 0;
    }
    if (eventDependentObservation) {
      duration += era->weight;
    } else {
      duration += era->end - era->start + 1;
    }
    for (Era outcome : outcomes) {
      if (outcome.start >= era->start && outcome.start <= era->end) {
        outcomeCount++;
      }
    }
  }
  if (previousPattern != NULL) {
    addToResult(*previousPattern, outcomeCount, duration, caseId);
  }
}
bool SccsConverter::isNanOrInf(const double x) {
  return ((x < 0) == (x >= 0)) || !(x <= DBL_MAX && x >= -DBL_MAX);
}

bool SccsConverter::invalidWeight(const double weight, const double startValue) {
  return isNanOrInf(weight) || isNanOrInf(startValue) || weight > 1000 * startValue;
}

void SccsConverter::removeNonRiskOrControlIntervals(std::vector<ConcomitantEra>& concomitantEras) {
  for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
    if (era->eraIdToValue.size() == 0) {
      concomitantEras.erase(era--);
    } else {
      era->eraIdToValue.erase(controlIntervalId);
    }
  }
}

void SccsConverter::computeEventDepObsWeights(std::vector<ConcomitantEra>& concomitantEras, const PersonData& personData) {
  double astart = (personData.ageInDays) / 365.25;
  double aend = (personData.ageInDays + personData.endDay + 1) / 365.25;
  double present = personData.noninformativeEndCensor?1.0:0;
  weightFunction->set(present, astart, aend);
  for (std::vector<ConcomitantEra>::iterator era = concomitantEras.begin(); era != concomitantEras.end(); ++era) {
    double start = (personData.ageInDays + era->start) / 365.25;
    double end = (personData.ageInDays + era->end + 1) / 365.25 - 1e-07;
    double startValue = weightFunction->getValue(start);
    double weight = 0;
    if (invalidWeight(weightFunction->getValue(end), startValue)) {
      // Very rare case:
      // Weight function can be problematic to compute due to numeric issues near the end of the integral
      // We'll walk backwards to find last computable point, and assume constant value after that as approximation
      double lastComputable = end - (1 / 365.25);
      double value = weightFunction->getValue(lastComputable);
      while (lastComputable > start && invalidWeight(value, startValue)) {
        lastComputable -= 1 / 365.25;
        value = weightFunction->getValue(lastComputable);
      }
      Environment base = Environment::namespace_env("base");
      Function warning = base["warning"];
      if (lastComputable <= start) {
        warning("\nCannot compute weight function for entire observation period " + personData.observationPeriodId + ". Removing from analysis", Named("call.", false));
        concomitantEras.clear();
        break;
      } else {
        warning("\nCannot compute full weight function for observation period " + personData.observationPeriodId + ", assuming constant weight for last " + std::to_string((int)round((end-lastComputable)*365.25)) + " day(s)", Named("call.", false));
        // Rcout << "CaseID: " << std::to_string(personData.caseId) << ", start: " << start*365.25 << ", end: " << end*365.25 << "\n";
        weight = ohdsi::sccs::NumericIntegration::integrate(*weightFunction, start, lastComputable, 1.490116e-08);
        weight += (end-lastComputable) * value;
      }
    } else {
      weight = ohdsi::sccs::NumericIntegration::integrate(*weightFunction, start, end, 1.490116e-08);
      if (isNanOrInf(weight)) {
        Environment base = Environment::namespace_env("base");
        Function warning = base["warning"];
        warning("\nCannot compute weight function for entire observation period " + personData.observationPeriodId + ". Removing from analysis", Named("call.", false));
        concomitantEras.clear();
        break;
      }
    }
    era->weight = weight * 365.25;
    // Rcout  << "ID: " << personData.caseId << ", astart: " << astart*365.25 << ", aend:" << aend*365.25 << ", start: " << start*365.25 << ", end:" << end*365.25 << ", present:" << present << ", weight:" << weight*365.25 << "\n";
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

void SccsConverter::addMonthEras(std::vector<Era>& eras, const PersonData& personData){
  struct tm obsStartDate = {0, 0, 12};
  obsStartDate.tm_year = personData.obsStartYear - 1900;
  obsStartDate.tm_mon = personData.obsStartMonth - 1;
  obsStartDate.tm_mday = personData.obsStartDay;
  mktime(&obsStartDate);
  struct tm startDate = {0, 0, 12};
  startDate.tm_year = personData.obsStartYear - 1900;
  startDate.tm_mon = personData.obsStartMonth - 1;
  startDate.tm_mday = personData.obsStartDay + personData.startDay;
  mktime(&startDate);
  struct tm startOfMonth(startDate);
  startOfMonth.tm_mday = 1;
  struct tm startOfNextMonth = addMonth(startOfMonth);
  int eraStartDay = personData.startDay;
  int nextEraStartDay = std::min(dateDifference(startOfNextMonth, obsStartDate), personData.endDay + 1);
  int month = startOfMonth.tm_mon;
  while (eraStartDay <= personData.endDay) {
    if (includeAge){
      int ageIndex = personData.ageInDays + eraStartDay - ageOffset;
      if (ageIndex < 0) {
        ageIndex = 0;
      } else if (ageIndex >= ageDesignMatrix.nrow()) {
        ageIndex = ageDesignMatrix.nrow() - 1;
      }
      for (int i = 0; i < ageDesignMatrix.ncol(); i++){
        Era era(eraStartDay, nextEraStartDay - 1, ageIdOffset + i, ageDesignMatrix(ageIndex, i));
        eras.push_back(era);
      }
    }
    if (includeSeason){
      for (int i = 0; i < seasonDesignMatrix.ncol(); i++){
        Era era(eraStartDay, nextEraStartDay - 1, seasonIdOffset + i, seasonDesignMatrix(month, i));
        eras.push_back(era);
      }
    }
    if (includeCalendarTime){
      int monthIndex = (startOfMonth.tm_year + 1900) * 12 + startOfMonth.tm_mon - calendarTimeOffset;
      if (monthIndex < 0) {
        monthIndex = 0;
      } else if (monthIndex >= calendarTimeDesignMatrix.nrow()) {
        monthIndex = calendarTimeDesignMatrix.nrow() - 1;
      }
      for (int i = 0; i < calendarTimeDesignMatrix.ncol(); i++){
        Era era(eraStartDay, nextEraStartDay - 1, calendarTimeIdOffset + i, calendarTimeDesignMatrix(monthIndex, i));
        eras.push_back(era);
      }
    }
    eraStartDay = nextEraStartDay;
    startOfMonth = startOfNextMonth;
    month = startOfMonth.tm_mon;
    startOfNextMonth = addMonth(startOfMonth);
    nextEraStartDay = std::min(dateDifference(startOfNextMonth, obsStartDate), personData.endDay + 1);
  }
}

void SccsConverter::addCovariateEra(std::vector<Era>& outputEras, int start, int end, int leftCensor, int rightCensor, int covariateIdRow, const CovariateSettings& covariateSettings) {
  int newStart = covariateSettings.start + (covariateSettings.addExposedDaysToStart?end:start);
  int newEnd = covariateSettings.end + (covariateSettings.addExposedDaysToEnd?end:start);
  // std::cout << "Start: " << start << ", end: " << end<< ", newStart: " << newStart << ", newEnd: " << newEnd << "\n";
  if (newStart <= leftCensor){
    newStart = leftCensor + 1;
  }
  if (newEnd >= rightCensor) {
    newEnd = rightCensor - 1;
  }
  if (newEnd < newStart)
    return;
  Era era(newStart, newEnd, covariateSettings.outputIds(covariateIdRow,0), 1);
  outputEras.push_back(era);
}

void SccsConverter::addCovariateEras(std::vector<Era>& outputEras, const std::vector<Era>& eras, const CovariateSettings covariateSettings) {
  if (covariateSettings.stratifyById) {
    for (int i = 0; i < covariateSettings.eraIds.size(); i++) {
      int64_t eraId = covariateSettings.eraIds[i];
      std::vector<Era> covariateEras;
      int start = -9999;
      int end = -9999;
      bool first = true;
      for (std::vector<Era>::const_iterator era = eras.begin(); era != eras.end(); ++era) {
        if (era->eraId == eraId) {
          if (era->start > (end + 1) && end != -9999) {
            Era mergedEra(start, end, 0, 1);
            // std::cout << "Start: " << start << ", end: " << end<< ", eraStart: " << era->start<< ", eraEnd: " << era->end << "\n";
            covariateEras.push_back(mergedEra);
            if (covariateSettings.firstOccurrenceOnly) {
              first = false;
              break;
            }
            start = era->start;
          }
          if (start == -9999){
            start = era->start;
          }
          if (era->end > end) {
            end = era->end;
          }
          // std::cout << "Start: " << start << ", end: " << end<< ", eraStart: " << era->start<< ", eraEnd: " << era->end << "\n";
        }
      }
      if (end != -9999 && (!covariateSettings.firstOccurrenceOnly || first)){
        Era mergedEra(start, end, 0, 1);
        covariateEras.push_back(mergedEra);
      }
      // std::cout << "Count: " << covariateEras.size() << "\n";
      int leftCensor = -1;
      for (unsigned int j = 0; j < covariateEras.size(); j++) {
        int rightCensor;
        if (j + 1 < covariateEras.size()) {
          rightCensor = covariateEras[j + 1].start;
        } else {
          rightCensor = 99999;
        }
        addCovariateEra(outputEras, covariateEras[j].start, covariateEras[j].end, leftCensor, rightCensor, i, covariateSettings);
      }
    }
  } else { //not stratify by ID
    std::vector<Era> covariateEras;
    int start = -9999;
    int end = -9999;
    bool first = true;
    for (std::vector<Era>::const_iterator era = eras.begin(); era != eras.end(); ++era) {
      if (covariateSettings.eraIdSet.find(era->eraId) != covariateSettings.eraIdSet.end()) {
        if (era->start > (end + 1) && end != -9999) {
          Era mergedEra(start, end, 0, 1);
          covariateEras.push_back(mergedEra);
          if (covariateSettings.firstOccurrenceOnly) {
            first = false;
            break;
          }
          start = era->start;
        }
        if (start == -9999){
          start = era->start;
        }
        if (era->end > end) {
          end = era->end;
        }
      }
    }
    if (end != -9999 && (!covariateSettings.firstOccurrenceOnly || first)){
      Era mergedEra(start, end, 0, 1);
      covariateEras.push_back(mergedEra);

    }
    int leftCensor = -1;
    for (unsigned int j = 0; j < covariateEras.size(); j++) {
      int rightCensor;
      if (j + 1 < covariateEras.size()) {
        rightCensor = covariateEras[j + 1].start;
      } else {
        rightCensor = 99999;
      }
      addCovariateEra(outputEras, covariateEras[j].start, covariateEras[j].end, leftCensor, rightCensor, 0, covariateSettings);
    }
  }
}

void SccsConverter::processPerson(PersonData& personData) {
  std::vector<Era>* eras = personData.eras;
  std::sort(eras->begin(), eras->end()); // Sort by start date
  std::vector<Era>* outcomes = personData.outcomes;
  std::vector<Era> outputEras;
  for (CovariateSettings covariateSettings : covariateSettingsVector){
    addCovariateEras(outputEras, *eras, covariateSettings);
  }
  clipEras(outputEras, personData.startDay, personData.endDay);
  outputEras = mergeOverlapping(outputEras);
  if (includeAge || includeSeason || includeCalendarTime) {
    if (outputEras.size() == 0)  // No exposures: still use to fit age and/or season splines?
      if (hasTimeCovariateCases && timeCovariateCases.find(personData.caseId) == timeCovariateCases.end())
        return;
    addMonthEras(outputEras, personData);
  }
  std::vector<ConcomitantEra> concomitantEras = buildConcomitantEras(outputEras, personData.startDay, personData.endDay);
  if (concomitantEras.size() == 1) { // Not informative
    return;
  }
  resultStruct.computeCovariateStatistics(outputEras, *outcomes, personData.personId, personData.endDay);
  if (scri) {
    removeNonRiskOrControlIntervals(concomitantEras);
  }
  if (eventDependentObservation) {
    computeEventDepObsWeights(concomitantEras, personData);
  }
  addToResult(concomitantEras, *outcomes, personData.caseId);
}

void SccsConverter::convertToSccs() {
  // Environment base = Environment::namespace_env("base");
  // Function writeLines = base["writeLines"];
  // writeLines("Check 1\n");
  while (personDataIterator.hasNext()) {
    PersonData personData = personDataIterator.next();
    processPerson(personData);
  }
  resultStruct.convertToAndromeda();
}

}
}

#endif /* SCCSCONVERTER_CPP_ */
