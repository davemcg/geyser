library(SummarizedExperiment)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(tibble)
suppressPackageStartupMessages(library(ComplexHeatmap))

# this argument yanked via the R/geyser.R function
rse_name <- deparse(substitute(rse))

server <- function(input, output, session) {
  # select sample columns to group on -----
  shiny::updateSelectizeInput(session, 'groupings',
                              choices = colnames(colData(get(rse_name))) %>% sort(),
                              selected = '',
                              server = TRUE)
  # select genes ----
  shiny::updateSelectizeInput(session, 'genes',
                              choices = rownames(get(rse_name)) %>% sort(),
                              selected = '',
                              server = TRUE)
  # select assay slot (usually counts) ----
  shiny::updateSelectizeInput(session, 'slot',
                              choices = SummarizedExperiment::assays(get(rse_name)) %>% names() %>% sort(),
                              selected = if ('counts' %in% names(SummarizedExperiment::assays(get(rse_name)))){'counts'} else {names(assays(get(rse_name)))[1]},
                              server = TRUE)
  # expression plot ----
  #source('exp_plot.R')
  exp_plot_reactive <- shiny::eventReactive(input$exp_plot_button, {
    exp_plot(input, rse_name)
  })
  output$exp_plot <- shiny::renderPlot({
    exp_plot_reactive()$plot},
    height = shiny::eventReactive(input$exp_plot_button,
                                  {max(600, 20 * length(input$genes) * exp_plot_reactive()$grouping_length)})
  )
  # hm plot -----
  #source('heatmap.R')
  hm_plot_reactive <- shiny::eventReactive(input$hm_plot_button, {
    hm_plot(input, rse_name)
  })
  output$hm_plot <- shiny::renderPlot({
    ComplexHeatmap::draw(hm_plot_reactive()$plot)},
    height = shiny::eventReactive(input$hm_plot_button,
                                  {max(400, 0.7 * hm_plot_reactive()$grouping_length)})
  )

  # sample data table -----
  output$table <- DT::renderDataTable(
    SummarizedExperiment::colData(get(rse_name)) %>%
      dplyr::as_tibble(rownames = 'rse_sample_id') %>%
      dplyr::select('rse_sample_id', dplyr::any_of(input$groupings)) %>%
      DT::datatable(rownames= FALSE,
                    options = list(autoWidth = TRUE,
                                   pageLength = 15,
                                   dom = 'tp'),
                    filter = list(position = 'top', clear = FALSE)),
    server = TRUE
  )
  ## proxy to clear row selection -----
  ## https://yihui.shinyapps.io/DT-proxy/
  proxy = DT::dataTableProxy('table')
  observeEvent(input$clear_colData_row_selections, {
    proxy %>% DT::selectRows(NULL)
  })

  # sample data table full -----
  output$table_full <- DT::renderDataTable(
    SummarizedExperiment::colData(get(rse_name)) %>%
      dplyr::as_tibble() %>%
      DT::datatable(rownames= FALSE,
                    options = list(autoWidth = TRUE,
                                   pageLength = 25),
                    filter = list(position = 'top', clear = FALSE),
                    selection = 'none'
      ),
    server = TRUE
  )
  session$onSessionEnded(function() {
    stopApp()
  })
}
