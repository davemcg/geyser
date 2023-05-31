#' @title geyser
#'
#' @description Run shiny app to use SummarizedExperiment object to display RNAseq data
#'
#' @export
#' @importFrom shiny runApp
#' @importFrom shiny shinyApp
#'
#' @param rse SummarizedExperiment object
#' @param name Title name that goes on the top left of the Shiny app
#' @param ... More arguments for [shiny::runApp()].
#'
#' @details
#'
#' Shiny app uses the rowData rownames to define the genes. The colData field is made
#' fully available to make custom plot groupings.
#'
#' @author David McGaughey
#'
#' @examples
#'
#' \dontrun{
#' geyser(your_rse)
#' }
#'

geyser <- function(rse, name = "geyser", ...) {
  file_path <- system.file("app/myapp.R", package = "geyser")
  #file_path <- "inst/app/myapp.R"
  if (!nzchar(file_path)) stop("Shiny app not found")
  ui <- server <- NULL # avoid NOTE about undefined globals
  source(file_path, local = TRUE, chdir = TRUE)
  server_env <- environment(server)

  # variables for server.R
  server_env$rse <- rse
  server_env$name <- name

  app <- shiny::shinyApp(ui, server)
  shiny::runApp(app, ...)
}