library(SummarizedExperiment)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(tibble)

# this argument yanked via the R/geyser.R function
rse_name <- deparse(substitute(rse))

server <- function(input, output, session) {
  # select sample columns to group on -----
  updateSelectizeInput(session, 'groupings',
                       choices = colnames(colData(get(rse_name))) %>% sort(),
                       selected = '',
                       server = TRUE)
  # select genes ----
  updateSelectizeInput(session, 'genes',
                       choices = rownames(get(rse_name)) %>% sort(),
                       selected = '',
                       server = TRUE)
  # expression plot ----
  exp_plot_reactive <- eventReactive(input$exp_plot_button, {
    genes <- input$genes
    groupings <- input$groupings

    # pull gene counts and left_join with colData
    pdata <- assay(get(rse_name), value = 'counts')[genes, ,drop = FALSE] %>%
      as_tibble(rownames = 'Gene') %>%
      pivot_longer(-Gene, values_to = 'counts', names_to = 'sample_unique_id') %>%
      left_join(colData(get(rse_name)) %>%
                  as_tibble(rownames = 'sample_unique_id') %>%
                  mutate(rowid = row_number()),
                by = 'sample_unique_id')
    if (length(input$table_rows_selected)){
      pfdata <- pdata %>% filter(rowid %in% input$table_rows_selected)
    } else {
      pfdata <- pdata
    }
    # optional (but set as default) log1p scaling
    if (input$expression_scale){
      pfdata$counts <- log1p(pfdata$counts)
    }
    pfdata %>%
      # make custom column with user selected groupings of columns
      unite("group", all_of(groupings), remove = FALSE, sep = " | ") %>%
      ggplot(aes(x=group,y=counts)) +
      geom_boxplot() +
      geom_jitter() +
      coord_flip() +
      xlab(paste0(groupings, collapse = ' | ')) +
      ylab("Expression") +
      theme_bw() +
      facet_wrap(~Gene, ncol = 1)
  })
  output$exp_plot <- renderPlot({
    exp_plot_reactive()
  })
  # sample data table -----
  output$table <- DT::renderDataTable(
    colData(get(rse_name)) %>%
      as_tibble() %>%
      select(any_of(input$groupings)) %>%
      DT::datatable(rownames= FALSE,
                    options = list(autoWidth = TRUE,
                                   pageLength = 25),
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
    colData(get(rse_name)) %>%
      as_tibble() %>%
      DT::datatable(rownames= FALSE,
                    options = list(autoWidth = TRUE,
                                   pageLength = 25),
                    filter = list(position = 'top', clear = FALSE),
                    selection = 'none'),
    server = TRUE
  )
  session$onSessionEnded(function() {
    stopApp()
  })

}
