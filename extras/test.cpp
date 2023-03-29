#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
void test(S4& andromeda, DataFrame table) {
  Environment base = Environment::namespace_env("base");
  Function message = base["message"];
  message("Start");
  // Function names = base["names"];
  // CharacterVector tableNames = names(andromeda);
  // message(tableNames);

  // Environment andromedaPackage = Environment::namespace_env("Andromeda");
  // Function appendToTable = andromedaPackage["appendToTable"];
  // Function bracket = base["[["];
  // appendToTable(bracket(andromeda, "cars"), table);

  Function bracket = base["[[<-"];
  bracket(Named("x") = andromeda, Named("i") = "cars2", Named("value") = table);

}



