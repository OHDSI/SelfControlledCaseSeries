# @file PackageMaintenance
#
# Copyright 2015 Observational Health Data Sciences and Informatics
#
# This file is part of SelfControlledCaseSeries
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

.formatAndCheckCode <- function() {
  OhdsiRTools::formatRFolder()
  OhdsiRTools::checkUsagePackage("SelfControlledCaseSeries")
}

.createManualAndVignettes <- function() {
  if (.Platform$OS.type == "unix") {
    system("rm extras/SelfControlledCaseSeries.pdf")
    system("R CMD Rd2pdf ./ --output=extras/SelfControlledCaseSeries.pdf")
  } else {
    shell("rm extras/SelfControlledCaseSeries.pdf")
    shell("R CMD Rd2pdf ./ --output=extras/SelfControlledCaseSeries.pdf")
  }

  rmarkdown::render("vignettes/SingleStudies.Rmd",
                    output_file = "../inst/doc/SingleStudies.pdf",
                    rmarkdown::pdf_document(latex_engine = "pdflatex",
                                            toc = TRUE,
                                            number_sections = TRUE))
}
