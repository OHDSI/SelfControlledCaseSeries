/**
 * @file WeightFunctions.h
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

#ifndef __WeightFunctions_h__
#define __WeightFunctions_h__

#include <Rcpp.h>
#include <cmath>
#include "NumericIntegration.h"

namespace ohdsi {
namespace sccs {

class WeightFunction: public IntegrableFunction {
public:
  WeightFunction(const std::vector<double> &_p): p(_p) {}

  virtual double getValue(const double x) = 0;

  void set(const double _present, const double _astart, const double _aend) {
    present = _present;
    astart = _astart;
    aend = _aend;
  }
protected:
  static double pgamma(const double x, const double shape, const double rate, const bool noZero);
  static double dgamma(const double x, const double shape, const double rate);

  std::vector<double> p;
  double present;
  double astart;
  double aend;
};

class WsmallEwad2: public WeightFunction {
public:
  WsmallEwad2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double getValue(const double x);
};

class WsmallEwid2: public WeightFunction {
public:
  WsmallEwid2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double getValue(const double x);
};

class WsmallEgad2: public WeightFunction {
public:
  WsmallEgad2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double getValue(const double x);
};

class WsmallEgid2: public WeightFunction {
public:
  WsmallEgid2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double getValue(const double x);
};

}
}

#endif // __WeightFunctions_h__
