/**
 * @file WeightFunctions.h
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

#ifndef __WeightFunctions_h__
#define __WeightFunctions_h__

#include <Rcpp.h>
#include <cmath>
#include "NumericIntegration.h"

namespace ohdsi {
namespace sccs {

class WeightFunction: public IntegratableFunction {
public:
  WeightFunction(const std::vector<double> &_p): p(_p) {}

  virtual double operator() (const double x) = 0;

  void set(const double _present, const double _astart, const double _aend) {
    present = _present;
    astart = _astart;
    aend = _aend;
  }
protected:
  static double pgamma(const double x, const double shape, const double rate) {
    double result = R::pgamma(x,shape,1/rate,0,0);
    if (result == 0) {
      return 0.000000001;
    } else {
      return result;
    }
  }
  static double dgamma(const double x, const double shape, const double rate) {
    double result = R::dgamma(x,shape,1/rate,0);
    if (result == 0) {
      return 0.000000001;
    } else {
      return result;
    }
  }

  std::vector<double> p;
  double present;
  double astart;
  double aend;
};

class WsmallEwad2: public WeightFunction {
public:
  WsmallEwad2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double operator() (const double x) {
    double thetaA = p[0];
    double thetaB = p[1] + p[2] * log(astart);
    double eta = p[3] + p[4] * x;
    double gamma0 = p[5] + p[6] * log(astart);

    double lamA =(exp(-thetaA));
    double lamB =(exp(-thetaB));
    double pi0  =(exp(eta)/(1+exp(eta)));
    double nu0  =(exp(gamma0));

    double val = ((1-present)*log(pi0*lamA*exp(-lamA*(aend-x))+
                  (1-pi0)*nu0*lamB*(pow(aend*lamB,(nu0-1)))*exp(-(pow(aend*lamB,nu0)-pow(x*lamB,nu0)))) +
                  present * log(pi0*exp(-lamA*(aend-x))+
                  (1-pi0)*exp(-(pow(aend*lamB,nu0)-pow(x*lamB,nu0)))));
    return exp(val);
  }
};

class WsmallEwid2: public WeightFunction {
public:
  WsmallEwid2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double operator() (const double x) {
    double thetaA = p[0];
    double thetaB = p[1] + p[2] * log(x);
    double eta = p[3] + p[4] * x;
    double gamma0 = p[5] + p[6] * log(x);

    double lamA =(exp(-thetaA));
    double lamB =(exp(-thetaB));
    double pi0  =(exp(eta)/(1+exp(eta)));
    double nu0  =(exp(gamma0));

    double i = aend-x;

    double  val =  ((1-present)*log(pi0*lamA*exp(-lamA*i)+
                    (1-pi0)*nu0*lamB*(pow(i*lamB,nu0-1))*exp(-(pow(i*lamB,nu0)))) +
                    present *log(pi0*exp(-lamA*i)+
                    (1-pi0)*exp(-(pow(i*lamB,nu0)))));
    return exp(val);
  }
};

class WsmallEgad2: public WeightFunction {
public:
  WsmallEgad2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double operator() (const double x) {
    double thetaA = p[0];
    double thetaB = p[1] + p[2] * log(astart);
    double eta = p[3] + p[4] * x;
    double gamma0 = p[5] + p[6] * log(astart);

    double lamA =(exp(-thetaA));
    double lamB =(exp(-thetaB));
    double pi0  =(exp(eta)/(1+exp(eta)));
    double nu0  =(exp(gamma0));

    double rate0 = nu0*lamB;

    double val = ((1-present)*log(pi0*lamA*exp(-lamA*(aend-x))+
                  (1-pi0)*dgamma(aend,nu0,rate0)/pgamma(x,nu0,rate0)) +
                  present *log(pi0*exp(-lamA*(aend-x))+
                  (1-pi0)*pgamma(aend,nu0,rate0)/pgamma(x,nu0,rate0)));
    return exp(val);
  }
};

class WsmallEgid2: public WeightFunction {
public:
  WsmallEgid2(const std::vector<double> &_p): WeightFunction(_p){}

  virtual double operator() (const double x) {
    double thetaA = p[0];
    double thetaB = p[1] + p[2] * log(x);
    double eta = p[3] + p[4] * x;
    double gamma0 = p[5] + p[6] * log(x);

    double lamA =(exp(-thetaA));
    double lamB =(exp(-thetaB));
    double pi0  =(exp(eta)/(1+exp(eta)));
    double nu0  =(exp(gamma0));

    double rate0 = nu0*lamB;

    double i = aend-x;

    double val = ((1-present)*log(pi0*lamA*exp(-lamA*i)+
                  (1-pi0)*dgamma(i,nu0,rate0)) +
                  present *log(pi0*exp(-lamA*i)+
                  (1-pi0)*pgamma(i,nu0,rate0)));
    return exp(val);
  }
};

}
}

#endif // __WeightFunctions_h__
