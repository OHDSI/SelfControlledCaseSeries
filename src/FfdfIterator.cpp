/**
 * @file FfdfIterator.cpp
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

#ifndef FfdfIterator_CPP_
#define FfdfIterator_CPP_

#include <Rcpp.h>
#include "FfdfIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

FfdfIterator::FfdfIterator(const List& _ffdf, const bool &_showProgressBar) :
cursor(0), showProgressBar(_showProgressBar) {
  ffdf = _ffdf;
  Environment bit = Environment::namespace_env("bit");
  Function chunk = bit["chunk"];
  chunks = chunk(_ffdf);
  if (showProgressBar){
    Environment utils = Environment::namespace_env("utils");
    Function txtProgressBar = utils["txtProgressBar"];
    progressBar = txtProgressBar(0,chunks.size(),0,"=", NA_REAL, "" ,"", 3, "");
  }
}

bool FfdfIterator::hasNext() {
  return (cursor < chunks.length());
}

List FfdfIterator::next() {
  Environment ff = Environment::namespace_env("ff");
  Function subset = ff["[.ff"];
  Function getNames = ff["names.ffdf"];
  List physical = ffdf["physical"];
  List virtualFfdf = ffdf["virtual"];
  IntegerVector physicalElementNo = virtualFfdf["PhysicalElementNo"];
  CharacterVector names = getNames(ffdf);
  names = names[physicalElementNo - 1];
  List result(physical.length());
  for (int i = 0; i < physical.length(); i++){
    result[i] = subset(physical[i], chunks[cursor]);
  }
  result.names() = names;
  cursor++;
  if (showProgressBar){
    Environment utils = Environment::namespace_env("utils");
    Function setTxtProgressBar = utils["setTxtProgressBar"];
    setTxtProgressBar(progressBar, cursor);
    if (cursor == chunks.size()){
      Environment base = Environment::namespace_env("base");
      Function close = base["close"];
      close(progressBar);
    }
  }
  return result;
}
}
}
#endif /* FfdfIterator_CPP_ */
