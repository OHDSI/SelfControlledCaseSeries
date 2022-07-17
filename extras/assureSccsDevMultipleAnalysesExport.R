# export =======================================================================

exportSccsResults <- function(outputFolder) {

  exportFolder <- file.path(outputFolder, "export")
  if (!file.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }

  sccsSummaryFile <- file.path(outputFolder, "sccs_summary.csv")
  sccsSummary <- readr::read_csv(sccsSummaryFile,  col_types = readr::cols())


  exportAnalyses(exportFolder)
  exportExposures(exportFolder)
  exportOutcomes(exportFolder)
  # exportMetadata(outputFolder, exportFolder, databaseId, databaseId)
  # exportMainResults(outputFolder, exportFolder, databaseId) # reshape wide to long for multiple covars
  # exportDiagnostics(outputFolder, exportFolder, databaseId)

  exportAnalyses <- function(exportFolder) {
    analysisRef <- sccsSummary %>%
      dplyr::distinct(analysisId,
                      analysisDescription)
    names(analysisRef) <- SqlRender::camelCaseToSnakeCase(names(analysisRef))
    readr::write_csv(analysisRef, file.path(exportFolder, "analysis_ref.csv"))
  }

  exportExposures <- function(exportFolder) {
    exposureRef <- sccsSummary %>%
      dplyr::distinct(exposureId,
                      exposureName)
    names(exposureRef) <- SqlRender::camelCaseToSnakeCase(names(exposureRef))
    readr::write_csv(exposureRef, file.path(exportFolder, "exposure_ref.csv"))
  }

  exportOutcomes <- function(outcomeFolder) {
    outcomeRef <- sccsSummary %>%
      dplyr::distinct(outcomeId,
                      outcomeName)
    names(outcomeRef) <- SqlRender::camelCaseToSnakeCase(names(outcomeRef))
    readr::write_csv(outcomeRef, file.path(exportFolder, "outcome_ref.csv"))
  }

  exportMainResults <- function(outputFolder) {
    print("TODO")
  }

  exportDiagnostics <- function(outputFolder) {
    print("TODO")
  }

  exportMetadata <- function(outputFolder) {
    print("TODO")
  }
}
