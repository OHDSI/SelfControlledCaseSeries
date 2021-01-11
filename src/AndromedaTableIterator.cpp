/**
 * @file AndromedaTableIterator.cpp
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

#ifndef __AndromedaTableIterator_CPP__
#define __AndromedaTableIterator_CPP__

#include <Rcpp.h>
#include "AndromedaTableIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

AndromedaTableIterator::AndromedaTableIterator(const List& _andromedaTable, const bool& _showProgressBar) :
progressBar(0),
showProgressBar(_showProgressBar),
completed(0),
done(false) {

  if (showProgressBar){
    Environment dplyr = Environment::namespace_env("dplyr");
    Function count = dplyr["count"];
    Function pull = dplyr["pull"];
    total = as<int>(pull(count(_andromedaTable)));
    // Function tally = dplyr["tally"];
    // Function tally = dplyr["tally"];
    // total = tally(_andromedaTable, R_NilValue, false, R_NilValue);
    Environment utils = Environment::namespace_env("utils");
    Function txtProgressBar = utils["txtProgressBar"];
    progressBar = txtProgressBar(0, total, 0, "=", NA_REAL, "" ,"", 3, "");

  }
  Environment dbplyr = Environment::namespace_env("dbplyr");
  Function remote_con = dbplyr["remote_con"];
  Function sql_render = dbplyr["sql_render"];
  Environment dbi = Environment::namespace_env("DBI");
  Function dbSendQuery = dbi["dbSendQuery"];

  S4 connection = remote_con(_andromedaTable);
  String sql = sql_render(_andromedaTable, connection);
  resultSet = dbSendQuery(connection, sql);
}

bool AndromedaTableIterator::hasNext() {
  if (done)
    return false;
  else {
    Environment dbi = Environment::namespace_env("DBI");
    Function dbHasCompleted = dbi["dbHasCompleted"];
    if (as<bool>(dbHasCompleted(resultSet))) {
      Function dbClearResult = dbi["dbClearResult"];
      dbClearResult(resultSet);
      done = true;
      return false;
    } else {
      return true;
    }
  }
}

List AndromedaTableIterator::next() {
  Environment dbi = Environment::namespace_env("DBI");
  Function dbFetch = dbi["dbFetch"];
  DataFrame batch = dbFetch(resultSet, 100000);

  if (showProgressBar){
    completed = completed + batch.nrows();

    Environment utils = Environment::namespace_env("utils");
    Function setTxtProgressBar = utils["setTxtProgressBar"];
    setTxtProgressBar(progressBar, completed);
    if (completed == total){
      Environment base = Environment::namespace_env("base");
      Function close = base["close"];
      close(progressBar);
    }
  }

  return batch;
}
}
}
#endif /* __AndromedaTableIterator_CPP__ */
