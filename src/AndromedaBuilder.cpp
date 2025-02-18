/**
 * @file AndromedaBuilder.cpp
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

#ifndef AndromedaBuilder_CPP_
#define AndromedaBuilder_CPP_

#include <Rcpp.h>
#include "AndromedaBuilder.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

AndromedaBuilder::AndromedaBuilder(S4 _andromeda) : andromeda(_andromeda) {}

void AndromedaBuilder::appendToTable(const String& tableName, const DataFrame& data) {
  Environment andromedaPackage = Environment::namespace_env("Andromeda");
  Environment base = Environment::namespace_env("base");
  Environment dplyr = Environment::namespace_env("dplyr");
  Environment dbi = Environment::namespace_env("DBI");
  Function names = base["names"];

  CharacterVector tableNames = names(andromeda);
  if(std::find(tableNames.begin(), tableNames.end(), tableName) != tableNames.end()) {
    Function tbl = dplyr["tbl"];
    Function appendToTable = andromedaPackage["appendToTable"];

    List table = tbl(andromeda, tableName);
    appendToTable(table, data);
  } else {
    Function dbWriteTable = dbi["dbWriteTable"];

    dbWriteTable(andromeda, tableName, data, true, false);
  }
}

S4 AndromedaBuilder::getAndromeda(){
  return andromeda;
}

}
}
#endif /* AndromedaBuilder_CPP_ */
