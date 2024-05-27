#' @title Launch the TextAnalyzer shiny app
#' @name run_TextAnalyzer
#' @description
#' \code{run_TextAnalyzer} Launches the app to analyze text data with multiples techniques.

run_TextAnalyzer <- function(use_browser = TRUE) {
  appDir <- system.file("shiny-app","app", package = "TextAnalyzer")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `TextAnalyzer`.",
         call. = FALSE)
  }

  if (use_browser == TRUE)
    shiny::runApp(appDir, display.mode = "normal",
           launch.browser = TRUE)
  else
    shiny::runApp(appDir, display.mode = "normal")

}
