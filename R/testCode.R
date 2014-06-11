testcode <- function(){
  setwd("c:/temp")

  library(SelfControlledCaseSeries)
  

  convertToCcdInputFile("c:/temp/eras.csv","c:/temp/obs.csv","c:/temp/test.txt",naivePeriod=0,minCovariateSubjects=0,javaDebugFile="c:/temp/debug.txt")
  convertToCcdInputFile("c:/temp/eras.csv","c:/temp/obs.csv","c:/temp/test.txt",naivePeriod=0,minCovariateSubjects=0)
}
