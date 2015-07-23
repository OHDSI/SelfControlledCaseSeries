/*
 * @file SccsConvert.h
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

#ifndef SCCSCONVERTER_H_
#define SCCSCONVERTER_H_

#include <Rcpp.h>
#include "PersonDataIterator.h"
#include "FfdfBuilder.h"
using namespace Rcpp;

namespace ohdsi {
namespace sccs {

struct ConcomitantEra {
  ConcomitantEra() :
  start(0), end(0) {
  }

  bool operator <(const ConcomitantEra &era) const {
    return (start < era.start);
  }

  bool operator ==(const ConcomitantEra &era) const {
    return (this == &era);
  }

  void print() {
    std::cout << "Start: " << start << ", end: " << end << ", size: " << conceptIdToValue.size() << std::endl;
    for (unsigned int i = 0; i < conceptIdToValue.size(); i++) {
      std::cout << "CID: " << conceptIdToValue[i] << std::endl;
    }
  }

  int start;
  int end;
  std::map<int64_t, double> conceptIdToValue;
};

struct ConcomitantEraCovariateComparator {
  bool operator()(const ConcomitantEra& era1, const ConcomitantEra& era2) const {
    if (era1.conceptIdToValue.size() != era2.conceptIdToValue.size() || era1.conceptIdToValue.size() == 0) {
      return  era1.conceptIdToValue.size() < era2.conceptIdToValue.size();
    } else {
      std::map<int64_t, double>::const_iterator iter1 = era1.conceptIdToValue.begin();
      std::map<int64_t, double>::const_iterator iter2 = era2.conceptIdToValue.begin();
      while (iter1 != era1.conceptIdToValue.end()) {
        if (iter1->first != iter2->first){
          return (iter1->first < iter2->first);
        }
        iter1++;
        iter2++;
      }
      return false;
    }
  }
};

struct ResultStruct {

  ResultStruct() :
  rowId(0) {
    outcomeRowId = new std::vector<int64_t>;
    outcomeStratumId = new std::vector<int64_t>;
    outcomeY = new std::vector<int64_t>;
    outcomeOutcomeId = new std::vector<int64_t>;
    outcomeTime = new std::vector<int64_t>;
    eraRowId = new std::vector<int64_t>;
    eraStratumId = new std::vector<int64_t>;
    eraCovariateId = new std::vector<int64_t>;
    eraCovariateValue = new std::vector<double>;
  }

  ~ResultStruct() {
    delete outcomeRowId;
    delete outcomeStratumId;
    delete outcomeY;
    delete outcomeOutcomeId;
    delete outcomeTime;
    delete eraRowId;
    delete eraStratumId;
    delete eraCovariateId;
    delete eraCovariateValue;
  }

  void addToOutcomes(const int64_t &outcomeId, const int64_t &y, const int64_t &time, const int64_t &stratumId){
    outcomeOutcomeId->push_back(outcomeId);
    outcomeY->push_back(y);
    outcomeRowId->push_back(rowId);
    outcomeTime->push_back(time);
    outcomeStratumId->push_back(stratumId);
    if (outcomeRowId->size() > 1000000){
      flushOutcomesToFfdf();
    }
  }

  void addToCovariates(const int64_t &stratumId, const int64_t &conceptId, const double &value){
    eraRowId->push_back(rowId);
    eraStratumId->push_back(stratumId);
    eraCovariateId->push_back(conceptId);
    eraCovariateValue ->push_back(value);
    if (eraRowId->size() > 1000000){
      flushErasToFfdf();
    }
  }

  void incRowId(){
    rowId++;
  }

  List convertToRList() {
    flushOutcomesToFfdf();
    flushErasToFfdf();
    return List::create(Named("outcomes") = outcomesBuiler.getFfdf(), Named("covariates") = erasBuiler.getFfdf());
  }
private:
  void flushOutcomesToFfdf(){
    if (outcomeRowId->size() > 0){
      List outcomes = List::create(Named("rowId") = wrap(*outcomeRowId), Named("stratumId") = wrap(*outcomeStratumId),
                                   Named("time") = wrap(*outcomeTime), Named("y") = wrap(*outcomeY), Named("outcomeId") = wrap(*outcomeOutcomeId));
      outcomesBuiler.append(outcomes);
      outcomeRowId->clear();
      outcomeStratumId->clear();
      outcomeY->clear();
      outcomeOutcomeId->clear();
      outcomeTime->clear();
    }
  }
  void flushErasToFfdf(){
    if (eraRowId->size() > 0){
      List covariates = List::create(Named("rowId") = wrap(*eraRowId), Named("stratumId") = wrap(*eraStratumId), Named("covariateId") =
        wrap(*eraCovariateId), Named("covariateValue") = wrap(*eraCovariateValue));
      erasBuiler.append(covariates);
      eraRowId->clear();
      eraStratumId->clear();
      eraCovariateId->clear();
      eraCovariateValue->clear();
    }
  }

  FfdfBuilder outcomesBuiler;
  FfdfBuilder erasBuiler;
  std::vector<int64_t>* outcomeRowId;
  std::vector<int64_t>* outcomeStratumId;
  std::vector<int64_t>* outcomeY;
  std::vector<int64_t>* outcomeOutcomeId;
  std::vector<int64_t>* outcomeTime;
  std::vector<int64_t>* eraRowId;
  std::vector<int64_t>* eraStratumId;
  std::vector<int64_t>* eraCovariateId;
  std::vector<double>* eraCovariateValue;
  int64_t rowId;
};

struct SccsConverter {
public:
  static List convertToSccs(const List& cases, const List& eras, const int covariateStart, const int covariatePersistencePeriod, const int naivePeriod,
                            bool firstOutcomeOnly, const bool includeAge, const int offset, const NumericMatrix& ageDesignMatrix, const bool includeSeason, const NumericMatrix& seasonDesignMatrix);
  static const int ageIdOffset = 100;
  static const int seasonIdOffset = 200;
private:
  static void processPerson(PersonData& personData, ResultStruct& resultStruct, const int covariateStart, const int covariatePersistencePeriod, const int naivePeriod,
                            const bool firstOutcomeOnly, const bool includeAge, const int ageOffset, const NumericMatrix& ageDesignMatrix, const bool includeSeason, const NumericMatrix& seasonDesignMatrix);
  static void clipEras(std::vector<Era>& eras, const int startDay, const int endDay);
  static void removeAllButFirstOutcome(std::vector<Era>& eras);
  static std::vector<Era> mergeOverlapping(std::vector<Era>& eras);
  static std::vector<Era> extractOutcomes(std::vector<Era>& eras);
  static std::vector<ConcomitantEra> buildConcomitantEras(std::vector<Era>& eras, const int startDay, const int endDay);
  static void compareToRunningEras(ConcomitantEra& newEra, std::vector<ConcomitantEra>& runningEras);
  static void addFinishedEras(const int day, std::vector<ConcomitantEra>& runningEras, std::vector<ConcomitantEra>& concomittantEras);
  static void addNonExposure(std::vector<ConcomitantEra>& concomittantEras, const int _start, const int end);
  static void addToResult(std::vector<ConcomitantEra>& concomitantEras, std::vector<Era>& outcomes, const int64_t& observationPeriodId,
                          ResultStruct& resultStruct);
  static void addToResult(const ConcomitantEra& conceptIds, std::map<int64_t, int>& outcomeIdToCount, const int duration,
                          const int64_t& observationPeriodId, ResultStruct& resultStruct, const std::set<int64_t>& outcomeIds);
  static int dateDifference(struct tm &date1, struct tm &date2);
  static struct tm addMonth(const struct tm &date);
  static void addMonthEras(std::vector<Era>& eras, const int startDay, const int endDay, const PersonData& personData, const bool includeAge, const int ageOffset, const NumericMatrix& ageDesignMatrix, const bool includeSeason, const NumericMatrix& seasonDesignMatrix);
};
}
}

#endif /* SCCSCONVERTER_H_ */
