Thank you for the reviews. I have made the following changes to address the issues raised:

1. There is currently no literature reference to include. We are working on a paper, and will include it in the DESCRIPTION when it is published.
2. I removed the default `outputFolder` value from `runSccsAnalyses()`. The user must now explicitly state where results will be written.
3. I have removed the use of `installed.packages()` in favor of a `tryCatch` around `find.package()`. (The only reason for these checks are so we still can pass R check even when suggested packages are not installed.)
4. I removed `set.seed()` from `R/SccsDataConversion.R`. (Now using a uniform, non-random selection to achieve the reproducability we need)

Although this is a new submission to CRAN, but this package has been in use by the Observational Health Data Science and Informatics (OHDSI) community for a long time.
It implements the self-controlled case series design and can extract all required data from the user's database in the OMOP Common Data Model. 
This package has already been used in many influential studies that have been published and have impacted regulatory decisions.

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
