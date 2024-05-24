#' @title launch the stminsights shiny app
#' @name run_TextAnalizer
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

run_TextAnalizer <- function(use_browser = TRUE) {
  appDir <- system.file("shiny-app","app", package = "TextAnalizer")
  if (appDir == "") {
    stop("Could not find directory. Try re-installing `TextAnalizer`.",
         call. = FALSE)
  }

  if (use_browser == TRUE)
    shiny::runApp(appDir, display.mode = "normal",
           launch.browser = TRUE)
  else
    shiny::runApp(appDir, display.mode = "normal")

}
