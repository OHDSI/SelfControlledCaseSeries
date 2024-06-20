/*
 * @file DateFunctions.h
 *
 * This file is part of SelfControlledCaseSeries
 *
 * Copyright 2024 Observational Health Data Sciences and Informatics
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

#ifndef DATEFUNCTIONS_H_
#define DATEFUNCTIONS_H_

#include <cmath>

using namespace Rcpp;

namespace ohdsi {
namespace sccs {

// Note: In response to https://github.com/OHDSI/SelfControlledCaseSeries/issues/59
// implementing our own date functions. Only alternatives appear to require
// C++ 20 or the Boost library.

struct Date {
  int year;
  int month; // 1 - 12
  int day;   // 1 - 31
};

bool isLeapYear(int year) {
  if (year % 4 != 0) return false;
  if (year % 100 != 0) return true;
  if (year % 400 != 0) return false;
  return true;
}

int daysInMonth(int year, int month) {
  switch (month) {
  case 1: return 31;
  case 2: return isLeapYear(year) ? 29 : 28;
  case 3: return 31;
  case 4: return 30;
  case 5: return 31;
  case 6: return 30;
  case 7: return 31;
  case 8: return 31;
  case 9: return 30;
  case 10: return 31;
  case 11: return 30;
  case 12: return 31;
  default: return 0;
  }
}

int daysFromStartOfYear(const Date& date) {
  int days = 0;
  for (int month = 1; month < date.month; ++month) {
    days += daysInMonth(date.year, month);
  }
  days += date.day;
  return days;
}

int differenceInDays(const Date& date1, const Date& date2) {
  if (date1.year < date2.year ||
      (date1.year == date2.year && date1.month < date2.month) ||
      (date1.year == date2.year && date1.month == date2.month && date1.day < date2.day)) {
    throw std::invalid_argument("date1 cannot be earlier than date2");
  }
  int daysDifference = 0;
  for (int year = date2.year; year < date1.year; ++year) {
    daysDifference += isLeapYear(year) ? 366 : 365;
  }
  daysDifference -= daysFromStartOfYear(date2);
  daysDifference += daysFromStartOfYear(date1);
  return std::abs(daysDifference);
}

Date addMonth(const Date& date1) {
  Date date2 = date1;
  date2.month++;
  if (date2.month == 13) {
    date2.month = 1;
    date2.year++;
  }
  return date2;
}

Date addDays(const Date& startDate, int daysToAdd) {
  if (daysToAdd < 0)
    throw std::invalid_argument("daysToAdd cannot be negative");
  Date newDate = startDate;

  while (daysToAdd > 0) {
    int daysThisMonth = daysInMonth(newDate.year, newDate.month);

    if (newDate.day + daysToAdd <= daysThisMonth) {
      newDate.day += daysToAdd;
      daysToAdd = 0;
    } else {
      daysToAdd -= (daysThisMonth - newDate.day + 1);
      newDate.day = 1;
      if (newDate.month == 12) {
        newDate.month = 1;
        newDate.year++;
      } else {
        newDate.month++;
      }
    }
  }
  return newDate;
}

}
}

#endif /* DATEFUNCTIONS_H_ */
