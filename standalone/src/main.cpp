#include <iostream>
#include <cstdlib>
#include <Rcpp.h>

//#include <RInside.h>
#include "SccsConverter.h"

using namespace Rcpp;
//#ifdef ECLIPSE

int main(int argc, char *argv[]) {
	std::cout << "check 1" << "\n";
	// RInside R(argc, argv);
    std::cout << "check 2" << "\n";
    //R["txt"] = "Hello, world!\n";	// assign a char* (string) to 'txt'

    //R.parseEvalQ("cat(txt)");

	using namespace ohdsi;
	using namespace ohdsi::sccs;
//	IntegerVector a(10);
//	IntegerVector b(10);
//	IntegerVector c(10);
//	DataFrame x = DataFrame::create(Named("a") = a, Named("b") = b, Named("c") = c);
//	IntegerVector d(10);
//	IntegerVector e(10);
//	DataFrame y = DataFrame::create(Named("d") = d, Named("e") = e);

//	std::cout << a[1] << "\n";
	return 0;
}

//#endif
