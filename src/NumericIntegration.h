/**
 * @file NumericIntegration.h
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

#ifndef __NumericIntegration_h__
#define __NumericIntegration_h__

#include <Rcpp.h>
#include "IntegrableFunction.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

struct NumericIntegration {
public:
  static double integrate(IntegrableFunction& f, const double start, const double end, const double tolerance);
private:
  static double recursiveIntegerate(IntegrableFunction& f, const double start, const double end, const double fStart, const double fMiddle, const double fEnd, const double is, const int count);
  static bool isNanOrInf(const double x);
};
}
}

#endif // __NumericIntegration_h__
