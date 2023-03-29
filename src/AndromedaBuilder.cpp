/**
 * @file AndromedaBuilder.cpp
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

#ifndef AndromedaBuilder_CPP_
#define AndromedaBuilder_CPP_

#include <Rcpp.h>
#include "AndromedaBuilder.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

AndromedaBuilder::AndromedaBuilder(S4 _andromeda) : andromeda(_andromeda) {}

void AndromedaBuilder::appendToTable(const String& tableName, const DataFrame& data) {
  Environment base = Environment::namespace_env("base");
  Function names = base["names"];
  CharacterVector tableNames = names(andromeda);
  if(std::find(tableNames.begin(), tableNames.end(), tableName) != tableNames.end()) {
    Environment andromedaPackage = Environment::namespace_env("Andromeda");
    Function appendToTable = andromedaPackage["appendToTable"];
    Function bracket = base["[["];
    appendToTable(bracket(andromeda, tableName), data);
  } else {
    Function bracket = base["[[<-"];
    bracket(Named("x") = andromeda, Named("i") = tableName, Named("value") = data);
  }
}

S4 AndromedaBuilder::getAndromeda(){
  return andromeda;
}

}
}
#endif /* AndromedaBuilder_CPP_ */
