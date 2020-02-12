/**
 * @file FfdfBuilder.h
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2020 Observational Health Data Sciences and Informatics
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

#ifndef __FfdfBuilder_h__
#define __FfdfBuilder_h__

#include <Rcpp.h>

using namespace Rcpp;

namespace ohdsi {
namespace sccs {


class FfdfBuilder {
public:
  FfdfBuilder();
  void append(const List& listOfVectors);
  List getFfdf();
private:
  bool uninitialized;
  List ffs;
};
}
}

#endif // __FfdfBuilder_h__
