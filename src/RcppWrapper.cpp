/*
 * @file RcppWrapper.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2025 Observational Health Data Sciences and Informatics
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

#ifndef __RcppWrapper_cpp__
#define __RcppWrapper_cpp__

#include <Rcpp.h>
#include "SccsConverter.h"
#include "SccsSimulator.h"
#include "WeightFunctions.h"

using namespace Rcpp;

// [[Rcpp::export]]
void convertToSccs(const DataFrame& cases,
                   const DataFrame& outcomes,
                   const List& eras,
                   const bool includeAge,
                   const int ageOffset,
                   const NumericMatrix& ageDesignMatrix,
                   const bool includeSeason,
                   const bool includeCalendarTime,
                   const int calendarTimeOffset,
                   const NumericMatrix& calendarTimeDesignMatrix,
                   const NumericMatrix& seasonDesignMatrix,
                   const NumericVector& timeCovariateCases,
                   const List& covariateSettingsList,
                   const int endOfObservationEraLength,
                   const int endOfObservationCovariateId,
                   const bool eventDependentObservation,
                   const List& censorModel,
                   const bool scri,
                   const int64_t controlIntervalId,
                   const S4 resultAndromeda) {

  using namespace ohdsi::sccs;

  try {
    SccsConverter sccsConverter(cases, outcomes, eras, includeAge, ageOffset, ageDesignMatrix, includeSeason,
                                seasonDesignMatrix, includeCalendarTime, calendarTimeOffset, calendarTimeDesignMatrix,
                                timeCovariateCases, covariateSettingsList, endOfObservationEraLength,
                                endOfObservationCovariateId,eventDependentObservation, censorModel, scri,
                                controlIntervalId, resultAndromeda);
    sccsConverter.convertToSccs();
  } catch (std::exception &e) {
    forward_exception_to_r(e);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  // return R_NilValue;
}

// [[Rcpp::export]]
List simulateSccsOutcomes(const DataFrame& cases, const DataFrame& eras,  const std::vector<double> baselineRates,
                          const DataFrame& eraRrs, const bool includeAgeEffect, const int ageOffset, const std::vector<double> ageRrs,
                          const bool includeSeasonality, const std::vector<double> seasonRrs, const bool includeCalendarTimeEffect,
                          const Date& minCalendarTime, const std::vector<double> calendarTimeRrs) {

  using namespace ohdsi::sccs;

  try {
    SccsSimulator sccsSimulator(cases, eras, baselineRates, eraRrs, includeAgeEffect, ageOffset, ageRrs, includeSeasonality, seasonRrs,
                                includeCalendarTimeEffect, minCalendarTime, calendarTimeRrs);
    return (sccsSimulator.simulateOutcomes());
  } catch (std::exception &e) {
    forward_exception_to_r(e);
  } catch (...) {
    ::Rf_error("c++ exception (unknown reason)");
  }
  return List::create();
}

// [[Rcpp::export]]
double testEwad(std::vector<double> p, double present, double astart, double aend, double start, double end) {
  ohdsi::sccs::WsmallEwad2 fun(p);
  fun.set(present, astart, aend);
  return ohdsi::sccs::NumericIntegration::integrate(fun, start, end, 1.490116e-08);
}

// [[Rcpp::export]]
double testEwid(std::vector<double> p, double present, double astart, double aend, double start, double end) {
  ohdsi::sccs::WsmallEwid2 fun(p);
  fun.set(present, astart, aend);
  return ohdsi::sccs::NumericIntegration::integrate(fun, start, end, 1.490116e-08);
}

// [[Rcpp::export]]
double testEgad(std::vector<double> p, double present, double astart, double aend, double start, double end) {
  ohdsi::sccs::WsmallEgad2 fun(p);
  fun.set(present, astart, aend);
  return ohdsi::sccs::NumericIntegration::integrate(fun, start, end, 1.490116e-08);
}

bool isNanOrInf(const double x) {
  return ((x < 0) == (x >= 0)) || !(x <= DBL_MAX && x >= -DBL_MAX);
}

// [[Rcpp::export]]
double testEgid(std::vector<double> p, double present, double astart, double aend, double start, double end) {
  ohdsi::sccs::WsmallEgid2 fun(p);
  fun.set(present, astart, aend);
  double weight;
  //std::cout << "end: " << end << ", aend: " << aend << ", value: " << fun.getValue(end) << "\n";
  if (end == aend && isNanOrInf(fun.getValue(end))) {
    //std::cout << "Problem detected\n";
    // Very rare case:
    // Weight function can be problematic to compute due to numeric issues near the end of the integral
    // We'll walk backwards to find last computable point, and assume constant value after that as approximation
    double step = 1.490116e-08;
    double lastComputable = end - step;
    double value = fun.getValue(lastComputable);
    //std::cout << "value at " << lastComputable << " = " << value << "\n";
    while (lastComputable > start && isNanOrInf(value)) {
      step *= 2;
      lastComputable -= step;
      value = fun.getValue(lastComputable);
      //std::cout << "value at " << lastComputable << " = " << value << "\n";
    }
    if (lastComputable <= start)
      throw "Unable to compute weight";
    weight = ohdsi::sccs::NumericIntegration::integrate(fun, start, lastComputable, 1.490116e-08);
    //std::cout << "integral" << weight << "\n";
    weight += (end-lastComputable) * value;
  } else {
    weight = ohdsi::sccs::NumericIntegration::integrate(fun, start, end, 1.490116e-08);
  }
  return weight;
}

// [[Rcpp::export]]
std::vector<double> computeCorrections(const DataFrame& cases, const DataFrame& monthAdjustments) {
  NumericVector startMonths = cases["startMonth"];
  NumericVector startMonthFractions = cases["startMonthFraction"];
  NumericVector endMonths = cases["endMonth"];
  NumericVector endMonthFractions = cases["endMonthFraction"];
  NumericVector months = monthAdjustments["month"];
  NumericVector totalRrs = monthAdjustments["totalRr"];
  std::vector<double> corrections(cases.nrows());
  for (int i = 0; i < cases.nrows(); i++) {
    double startMonth = startMonths[i];
    double startMonthFraction = startMonthFractions[i];
    double endMonth = endMonths[i];
    double endMonthFraction = endMonthFractions[i];
    double sumW = 0;
    double sumRr = 0;
    for (int j = 0; j < monthAdjustments.nrows(); j++) {
      double month = months[j];
      if (month >= startMonth) {
        double w = 1;
        if (month == startMonth)
          w = startMonthFraction;
        else if (month == endMonth)
          w = endMonthFraction;
        sumW += w;
        sumRr += w * totalRrs[j];
      }
      if (month == endMonth)
        break;
    }
    corrections[i] = sumRr / sumW;
  }
  return corrections;
}

#endif // __RcppWrapper_cpp__
