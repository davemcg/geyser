library(SummarizedExperiment)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(tibble)
load('./www/rse_gene_05.Rdata')

# groups <- c('Owner','Treatment')
# 
# pdata %>%
#   unite("group", all_of(groups), remove = FALSE, sep = " | ") %>%
#   ggplot(aes(x=group,y=log1p(counts))) +
#   geom_boxplot() +
#   geom_jitter() +
#   coord_flip() +
#   xlab(paste0(groups, collapse = ' | ')) +
#   theme_bw()




function(input, output, session) {
  # select sample columns to group on -----
  updateSelectizeInput(session, 'groupings', 
                       choices = colnames(colData(rse_gene)) %>% sort(), 
                       selected = '',
                       server = TRUE)
  # select genes ----
  updateSelectizeInput(session, 'genes', 
                       choices = rownames(rse_gene) %>% sort(), 
                       selected = '',
                       server = TRUE)
  # expression plot ----
  exp_plot_reactive <- eventReactive(input$exp_plot_button, {
    genes <- input$genes
    groupings <- input$groupings
    
    # grab gene counts and left_join with colData
    pdata <- assay(rse_gene)[genes, , 
                             drop = FALSE] %>% 
      as_tibble(rownames = 'Gene') %>% 
      pivot_longer(-Gene, values_to = 'counts', names_to = 'sample_unique_id') %>% 
      left_join(colData(rse_gene) %>% 
                  as_tibble(rownames = 'sample_unique_id') %>% 
                  mutate(rowid = row_number()),
                by = 'sample_unique_id')
    if (length(input$table_rows_selected)){
      pfdata <- pdata %>% filter(rowid %in% input$table_rows_selected)
    } else {
      pfdata <- pdata
    }
    pfdata %>%
      unite("group", all_of(groupings), remove = FALSE, sep = " | ") %>%
      ggplot(aes(x=group,y=log1p(counts))) +
      geom_boxplot() +
      geom_jitter() +
      coord_flip() +
      xlab(paste0(groupings, collapse = ' | ')) +
      theme_bw() + facet_grid(~Gene)
  })
  output$exp_plot <- renderPlot({
    exp_plot_reactive()
  })
  # sample data table -----
  output$table <- DT::renderDataTable(
    pdata %>% select(any_of(input$groupings)) %>% 
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
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
