I'va made changes that should avoid the R check issues on the less standard platforms.

There is an occasional NOTE about the installed package size. 
This appears to be because of large debugging symbols that appear in the C++ compiled code, and is beyond my control.

---

## Test environments
* Ubuntu 22.04, R 4.5.0
* MacOS, R 4.5.0
* MacOS M3, 4.4.1
* Windows 10, R 4.5.0

## R CMD check results

There were no ERRORs or WARNINGs. 

## Downstream dependencies

There are no downstream dependencies
