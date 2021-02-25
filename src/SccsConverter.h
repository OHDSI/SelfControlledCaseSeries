/*
 * @file SccsConvert.h
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

#ifndef SCCSCONVERTER_H_
#define SCCSCONVERTER_H_

#include <Rcpp.h>
#include "PersonDataIterator.h"
#include "AndromedaBuilder.h"
#include "WeightFunctions.h"
using namespace Rcpp;

namespace ohdsi {
namespace sccs {

struct CovariateSettings {
  CovariateSettings(const List& _covariateSettings) :
  stratifyById(as<bool>(_covariateSettings["stratifyById"])),
  firstOccurrenceOnly(as<bool>(_covariateSettings["firstOccurrenceOnly"])),
  eraIds(as<NumericVector>(_covariateSettings["eraIds"])),
  outputIds(as<NumericMatrix>(_covariateSettings["outputIds"])),
  start(as<int>(_covariateSettings["start"])),
  addExposedDaysToStart(as<std::string>(_covariateSettings["startAnchor"]).compare("era end") == 0),
  end(as<int>(_covariateSettings["end"])),
  addExposedDaysToEnd(as<std::string>(_covariateSettings["endAnchor"]).compare("era end") == 0){
    if (_covariateSettings.containsElementNamed("splitPoints")){
      NumericVector splitPointsList = _covariateSettings["splitPoints"];
      for (int i = 0; i < splitPointsList.size(); i++){
        splitPoints.push_back(splitPointsList[i]);
      }
    }
    for (int i = 0; i < eraIds.size(); i++){
      eraIdSet.insert(eraIds[i]);
    }
  }
  bool stratifyById;
  bool firstOccurrenceOnly;
  NumericVector eraIds;
  std::set<int64_t> eraIdSet;
  NumericMatrix outputIds;
  int start;
  bool addExposedDaysToStart;
  int end;
  bool addExposedDaysToEnd;
  std::vector<int> splitPoints;

};

struct CovariateStatistics {
  CovariateStatistics() :
  observationPeriodCount(0), eraCount(0), dayCount(0), outcomeCount(0), personIds() {}
  int observationPeriodCount;
  long eraCount;
  long dayCount;
  int outcomeCount;
  std::set<std::string> personIds;

};

struct ConcomitantEra {
  ConcomitantEra() :
  start(0), end(0), weight(0) {
  }

  bool operator <(const ConcomitantEra &era) const {
    return (start < era.start);
  }

  bool operator ==(const ConcomitantEra &era) const {
    return (this == &era);
  }

  int start;
  int end;
  double weight;
  std::map<int64_t, double> eraIdToValue;
};

struct ConcomitantEraCovariateComparator {
  bool operator()(const ConcomitantEra& era1, const ConcomitantEra& era2) const {
    if (era1.eraIdToValue.size() != era2.eraIdToValue.size() || era1.eraIdToValue.size() == 0) {
      return  era1.eraIdToValue.size() < era2.eraIdToValue.size();
    } else {
      std::map<int64_t, double>::const_iterator iter1 = era1.eraIdToValue.begin();
      std::map<int64_t, double>::const_iterator iter2 = era2.eraIdToValue.begin();
      while (iter1 != era1.eraIdToValue.end()) {
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
  rowId(0), covariateIdToCovariateStatistics() {
    outcomeRowId = new std::vector<int64_t>;
    outcomeStratumId = new std::vector<int64_t>;
    outcomeY = new std::vector<int64_t>;
    outcomeTime = new std::vector<double>;
    eraRowId = new std::vector<int64_t>;
    eraStratumId = new std::vector<int64_t>;
    eraCovariateId = new std::vector<int64_t>;
    eraCovariateValue = new std::vector<double>;
  }

  ~ResultStruct() {
    delete outcomeRowId;
    delete outcomeStratumId;
    delete outcomeY;
    delete outcomeTime;
    delete eraRowId;
    delete eraStratumId;
    delete eraCovariateId;
    delete eraCovariateValue;
  }

  void addToOutcomes(const int64_t &y, const double &time, const int64_t &stratumId){
    outcomeY->push_back(y);
    outcomeRowId->push_back(rowId);
    outcomeTime->push_back(time);
    outcomeStratumId->push_back(stratumId);
    if (outcomeRowId->size() > 1000000){
      flushOutcomesToAndromeda();
    }
  }

  void addToCovariates(const int64_t &stratumId, const int64_t &eraId, const double &value){
    eraRowId->push_back(rowId);
    eraStratumId->push_back(stratumId);
    eraCovariateId->push_back(eraId);
    eraCovariateValue ->push_back(value);
    if (eraRowId->size() > 1000000){
      flushErasToAndromeda();
    }
  }

  void computeCovariateStatistics(std::vector<Era>& eras, std::vector<Era>& outcomes, const String& personId) {
    std::set<int64_t> eraIds;
    for (std::vector<Era>::iterator era = eras.begin(); era != eras.end(); ++era) {
      CovariateStatistics& covariateStatistics = covariateIdToCovariateStatistics[era->eraId];
      covariateStatistics.eraCount++;
      covariateStatistics.dayCount += era->end - era->start + 1;
      for (Era outcome : outcomes) {
        if (outcome.start >= era->start && outcome.start <= era->end) {
          covariateStatistics.outcomeCount++;
        }
      }
      eraIds.insert(era->eraId);
    }
    for (std::set<int64_t>::iterator eraId = eraIds.begin(); eraId != eraIds.end(); ++eraId) {
      CovariateStatistics& covariateStatistics = covariateIdToCovariateStatistics[*eraId];
      covariateStatistics.observationPeriodCount++;
      covariateStatistics.personIds.insert(personId);
    }
  }

  void incRowId(){
    rowId++;
  }

  S4 convertToAndromeda() {
    flushOutcomesToAndromeda();
    flushErasToAndromeda();
    writeCovariateStatisticsToAndromeda();
    return andromedaBuilder.getAndromeda();
  }
private:
  void writeCovariateStatisticsToAndromeda() {
    int size = covariateIdToCovariateStatistics.size();
    std::vector<int64_t> covariateId(size);
    std::vector<int> personCount(size);
    std::vector<long> eraCount(size);
    std::vector<long> dayCount(size);
    std::vector<int> outcomeCount(size);
    std::vector<int> observationPeriodCount(size);
    int cursor(0);
    for (std::map<int64_t, CovariateStatistics>::iterator i = covariateIdToCovariateStatistics.begin(); i != covariateIdToCovariateStatistics.end(); ++i) {
      covariateId[cursor] = i->first;
      CovariateStatistics covariateStatistics = i->second;
      personCount[cursor] = covariateStatistics.personIds.size();
      eraCount[cursor] = covariateStatistics.eraCount;
      dayCount[cursor] = covariateStatistics.dayCount;
      outcomeCount[cursor] = covariateStatistics.outcomeCount;
      observationPeriodCount[cursor] = covariateStatistics.observationPeriodCount;
      cursor++;
    }
    DataFrame covariateStatistics = DataFrame::create(Named("covariateId") = wrap(covariateId),
                                                      Named("personCount") = wrap(personCount),
                                                      Named("eraCount") = wrap(eraCount),
                                                      Named("dayCount") = wrap(dayCount),
                                                      Named("outcomeCount") = wrap(outcomeCount),
                                                      Named("observationPeriodCount") = wrap(observationPeriodCount));
    andromedaBuilder.appendToTable("covariateStatistics", covariateStatistics);
  }

  void flushOutcomesToAndromeda(){
    if (outcomeRowId->size() > 0){
      DataFrame outcomes = DataFrame::create(Named("rowId") = wrap(*outcomeRowId),
                                             Named("stratumId") = wrap(*outcomeStratumId),
                                             Named("time") = wrap(*outcomeTime),
                                             Named("y") = wrap(*outcomeY));
      andromedaBuilder.appendToTable("outcomes", outcomes);
      outcomeRowId->clear();
      outcomeStratumId->clear();
      outcomeY->clear();
      outcomeTime->clear();
    }
  }
  void flushErasToAndromeda(){
    if (eraRowId->size() > 0){
      DataFrame covariates = DataFrame::create(Named("rowId") = wrap(*eraRowId),
                                               Named("stratumId") = wrap(*eraStratumId),
                                               Named("covariateId") = wrap(*eraCovariateId),
                                               Named("covariateValue") = wrap(*eraCovariateValue));
      andromedaBuilder.appendToTable("covariates", covariates);
      eraRowId->clear();
      eraStratumId->clear();
      eraCovariateId->clear();
      eraCovariateValue->clear();
    }
  }

  AndromedaBuilder andromedaBuilder;
  std::vector<int64_t>* outcomeRowId;
  std::vector<int64_t>* outcomeStratumId;
  std::vector<int64_t>* outcomeY;
  std::vector<double>* outcomeTime;
  std::vector<int64_t>* eraRowId;
  std::vector<int64_t>* eraStratumId;
  std::vector<int64_t>* eraCovariateId;
  std::vector<double>* eraCovariateValue;
  int64_t rowId;
  std::map<int64_t, CovariateStatistics> covariateIdToCovariateStatistics;
};

class SccsConverter {
public:
  SccsConverter(const DataFrame& _cases,
                const DataFrame& _outcomes,
                const List& _eras,
                const bool _includeAge,
                const int _ageOffset,
                const Rcpp::NumericMatrix& _ageDesignMatrix,
                const bool _includeSeason,
                const NumericMatrix& _seasonDesignMatrix,
                const NumericVector _ageSeasonsCases,
                const List& _covariateSettingsList,
                const bool _eventDependentObservation,
                const List& _censorModel);
  S4 convertToSccs();
  static const int ageIdOffset = 100;
  static const int seasonIdOffset = 200;
private:
  void processPerson(PersonData& personData);
  void clipEras(std::vector<Era>& eras, const int startDay, const int endDay);
  std::vector<Era> mergeOverlapping(std::vector<Era>& eras);
  std::vector<ConcomitantEra> buildConcomitantEras(std::vector<Era>& eras, const int startDay, const int endDay);
  void addToResult(std::vector<ConcomitantEra>& concomitantEras, std::vector<Era>& outcomes, const int64_t& observationPeriodId);
  void addToResult(const ConcomitantEra& era, int outcomeCount, const double duration, const int64_t& observationPeriodId);
  void computeEventDepObsWeights(std::vector<ConcomitantEra>& concomitantEras, const PersonData& personData);
  int dateDifference(struct tm &date1, struct tm &date2);
  struct tm addMonth(const struct tm &date);
  void addMonthEras(std::vector<Era>& eras, const PersonData& personData);
  void addCovariateEra(std::vector<Era>& outputEras, int start, int end, int leftCensor, int rightCensor, int covariateIdRow, const CovariateSettings& covariateSettings);
  void addCovariateEras(std::vector<Era>& outputEras, const std::vector<Era>& eras, const CovariateSettings covariateSettings);
  bool isNanOrInf(const double x);
  bool invalidWeight(const double weight, const double startValue);

  PersonDataIterator personDataIterator;
  ResultStruct resultStruct;
  bool includeAge;
  int ageOffset;
  NumericMatrix ageDesignMatrix;
  bool includeSeason;
  NumericMatrix seasonDesignMatrix;
  std::vector<CovariateSettings> covariateSettingsVector;
  bool eventDependentObservation;
  WeightFunction* weightFunction;
  bool hasAgeSeasonsCases;
  std::set<int64_t> ageSeasonsCases;
};
}
}

#endif /* SCCSCONVERTER_H_ */
