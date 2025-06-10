# Copyright 2025 Observational Health Data Sciences and Informatics
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

# Manually delete package from library. Avoids "Already in use" message when rebuilding
unloadNamespace("SelfControlledCaseSeries")
.rs.restartR()
folder <- system.file(package = "SelfControlledCaseSeries")
folder
unlink(folder, recursive = TRUE, force = TRUE)
file.exists(folder)

# Format and check code
styler::style_pkg()
OhdsiRTools::checkUsagePackage("SelfControlledCaseSeries")
OhdsiRTools::updateCopyrightYearFolder()
devtools::spell_check()

# Run simulations. These are too computationally expensive to run as unit tests
source("extras/SimpleSimulation.R")
source("extras/AgeAndSeasonSimulations.R")

# Create manual and vignette
unlink("extras/SelfControlledCaseSeries.pdf")
system("R CMD Rd2pdf ./ --output=extras/SelfControlledCaseSeries.pdf")

rmarkdown::render("vignettes/SingleStudies.Rmd",
                  output_file = "../inst/doc/SingleStudies.pdf")

rmarkdown::render("vignettes/MultipleAnalyses.Rmd",
                  output_file = "../inst/doc/MultipleAnalyses.pdf")

rmarkdown::render("vignettes/ResultsSchema.Rmd",
                  output_file = "../inst/doc/ResultsSchema.pdf")

pkgdown::build_site()
OhdsiRTools::fixHadesLogo()

# Release package --------------------------------------------------------------
devtools::check_win_devel()

rhub::rc_submit(platforms = "atlas")

devtools::release()
