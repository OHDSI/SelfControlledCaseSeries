/**
 * @file AndromedaTableIterator.h
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

#ifndef __AndromedaTableIterator_h__
#define __AndromedaTableIterator_h__

#include <Rcpp.h>

using namespace Rcpp;

namespace ohdsi {
namespace sccs {


class AndromedaTableIterator {
public:
  AndromedaTableIterator(const List& _andromedaTable, const bool& _showProgressBar);
  ~AndromedaTableIterator();
  bool hasNext();
  List next();
private:
  List progressBar;
  S4 resultSet;
  bool showProgressBar;
  int total;
  int completed;
  bool done;
};
}
}

#endif // __AndromedaTableIterator_h__
