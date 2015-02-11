/*
 * @file RcppWrapper.cpp
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2014 Observational Health Data Sciences and Informatics
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

#ifndef __RcppWrapper_cpp__
#define __RcppWrapper_cpp__

#include <Rcpp.h>
#include "SccsConverter.h"

using namespace Rcpp;

// [[Rcpp::export(".convertToSccs")]]
List convertToSccs(const DataFrame& cases, const DataFrame& eras, int covariatePersistencePeriod, int naivePeriod, bool firstOutcomeOnly) {

	using namespace ohdsi::sccs;

	try {
		return (SccsConverter::convertToSccs(cases, eras, covariatePersistencePeriod, naivePeriod, firstOutcomeOnly));
	} catch (std::exception &e) {
		forward_exception_to_r(e);
	} catch (...) {
		::Rf_error("c++ exception (unknown reason)");
	}
	return List::create();
}

#endif // __RcppWrapper_cpp__
