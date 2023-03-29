/**
 * @file AndromedaTableIterator.cpp
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

#ifndef __AndromedaTableIterator_CPP__
#define __AndromedaTableIterator_CPP__

#include <Rcpp.h>
#include "AndromedaTableIterator.h"

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

AndromedaTableIterator::AndromedaTableIterator(const List& _andromedaTable) :
done(false) {
  // Environment base = Environment::namespace_env("base");
  // Function message = base["message"];
  // message("Start");
  isDbiTable = _andromedaTable.inherits("tbl_dbi");
  if (isDbiTable) {
    Environment dbplyr = Environment::namespace_env("dbplyr");
    Function remote_con = dbplyr["remote_con"];
    Function sql_render = dbplyr["sql_render"];
    Environment dbi = Environment::namespace_env("DBI");
    Function dbSendQuery = dbi["dbSendQuery"];
    S4 connection = remote_con(_andromedaTable);
    String sql = sql_render(_andromedaTable, connection);
    resultSet = dbSendQuery(connection, sql);
  } else {
    Environment arrow = Environment::namespace_env("arrow");
    Function as_record_batch_reader = arrow["as_record_batch_reader"];
    recordBatchReader = as_record_batch_reader(_andromedaTable);
  }
  loadBuffer();
}

AndromedaTableIterator::~AndromedaTableIterator() {
  if (isDbiTable) {
    Environment dbi = Environment::namespace_env("DBI");
    Function dbClearResult = dbi["dbClearResult"];
    dbClearResult(resultSet);
  } else {
    Function Close = recordBatchReader["Close"];
    Close();
  }
}

void AndromedaTableIterator::loadBuffer() {
  if (isDbiTable) {
    Environment dbi = Environment::namespace_env("DBI");
    Function dbHasCompleted = dbi["dbHasCompleted"];
    if (as<bool>(dbHasCompleted(resultSet))) {
      buffer = R_NilValue;
    } else {
      Function dbFetch = dbi["dbFetch"];
      buffer = dbFetch(resultSet, 100000);
    }
  } else {
    Environment base = Environment::namespace_env("base");
    Function asDataFrame = base["as.data.frame"];
    Function read_next_batch = recordBatchReader["read_next_batch"];
    buffer = asDataFrame(read_next_batch());
  }
  if ((Rf_isNull(buffer)) || (buffer.nrow() == 0)) {
    done = true;
  }
}

bool AndromedaTableIterator::hasNext() {
  return !done;
}

List AndromedaTableIterator::next() {
  DataFrame batch = buffer;
  loadBuffer();
return batch;
}
}
}
#endif /* __AndromedaTableIterator_CPP__ */
