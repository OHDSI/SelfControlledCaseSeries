/**
 * @file IntegrableFunction.h
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

#ifndef __IntegrableFunction_h__
#define __IntegrableFunction_h__

#include <Rcpp.h>

using namespace Rcpp;

namespace ohdsi {
namespace sccs {


class IntegrableFunction {
public:
  virtual double getValue(const double x);
};

}
}

#endif // __IntegrableFunction_h__

