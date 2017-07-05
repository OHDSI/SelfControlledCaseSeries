/**
 * @file FfdfBuilder.cpp
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

#ifndef FfdfBuilder_CPP_
#define FfdfBuilder_CPP_

#include <Rcpp.h>
#include "FfdfBuilder.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

FfdfBuilder::FfdfBuilder() :
uninitialized(true) {}

void FfdfBuilder::append(const List& listOfVectors){
  if (uninitialized) {
    Environment ff = Environment::namespace_env("ff");
    Function asFf = ff["as.ff"];
    ffs = List(listOfVectors.length());
    ffs.names() = listOfVectors.names();
    for (int i = 0; i < listOfVectors.length(); i++){
      ffs[i] = asFf(listOfVectors(i));
    }
    uninitialized = false;
  } else {
    Environment ffbase = Environment::namespace_env("ffbase");
    Function ffAppend = ffbase["ffappend"];
    for (int i = 0; i < listOfVectors.length(); i++){
      ffs[i] = ffAppend(ffs[i], listOfVectors(i));
    }
  }
}

List FfdfBuilder::getFfdf(){
  if (uninitialized) {
    return List();
  }
  Environment base = Environment::namespace_env("base");
  Function doCall = base["do.call"];
  List ffdf = doCall("ffdf", ffs, false, Environment::namespace_env("ff"));
  return ffdf;
}

}
}
#endif /* FfdfBuilder_CPP_ */
