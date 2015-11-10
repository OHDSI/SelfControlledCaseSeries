/**
 * @file NumericIntegration.cpp
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

#ifndef __NumericIntegration_cpp__
#define __NumericIntegration_cpp__

#include <Rcpp.h>
#include "NumericIntegration.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

double NumericIntegration::integrate(IntegratableFunction& f, const double start, const double end, const double tolerance){
  const double delta = end-start;
  const double fStart = f(start);
  const double fEnd = f(end);
  const double fMiddle = f((start+end)/2.0);
  double is = delta/8.0 * (fStart+fEnd+fMiddle+f(start + 0.9501*delta) + f(start + 0.2311*delta) + f(start + 0.6068*delta) + f(start + 0.4860*delta) + f(start + 0.8913*delta));
  if(is == 0) {
    is = delta;
  }
  is = is * tolerance;
  return recursiveIntegerate(f, start, end, fStart, fMiddle, fEnd, is, 0);
}

double NumericIntegration::recursiveIntegerate(IntegratableFunction& f, const double start, const double end, const double fStart, const double fMiddle, const double fEnd, const double is, const int count) {
  double middle = (start + end)/2.0;
  double quart = (end - start)/4.0;
  double fMidL = f(start + quart);
  double fMidR = f(end - quart);
  double i1 = quart/1.5*(fStart+4.0*fMiddle+fEnd);
  double i2 = quart/3.0*(fStart+4.0*(fMidL+fMidR)+2.0*fMiddle+fEnd);
  i1 = (16.0 * i2 - i1) / 15.0;
  double q = 0;
  if ((std::abs(i1-i2) <= std::abs(is)) || (middle <= start) || (end <= middle)) {
    q = i1;
  } else {
    if(count < 100) {
      q = recursiveIntegerate(f, start, middle, fStart, fMidL, fMiddle, is, count + 1) + recursiveIntegerate(f, middle, end, fMiddle, fMidR, fEnd, is, count + 1);
    }
  }
  return q;
}
}
}

#endif // __NumericIntegration_cpp__
