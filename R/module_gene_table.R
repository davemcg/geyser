# GENE TABLE MODULE | R/module_gene_table.R ----

# UI | geneTableUI ----
#' UI for the Gene Table Module
#'
#' @param id Namespace ID.
geneTableUI <- function(id) {
  ns <- NS(id)
  DT::DTOutput(ns("geneTable"))
}


# Server | geneTableServer ----
#' Server for the Gene Table Module
#'
#' @param id Namespace ID.
#' @param gene_data A reactive data.table of gene information.
#' @return A reactive containing the name of the gene selected from the table.
geneTableServer <- function(id, gene_data) {
  moduleServer(id, function(input, output, session) {
    
    # Render the data table
    output$geneTable <- DT::renderDataTable({
      req(gene_data())
      DT::datatable(
        gene_data(),
        options = list(pageLength = 15, scrollX = TRUE, info = FALSE),
        rownames = FALSE,
        filter = 'top',
        selection = 'single'
      )
    })
    
    # A reactive to hold the selected gene name
    selected_gene <- reactive({
      req(gene_data(), input$geneTable_rows_selected)
      gene_data()$var_names[input$geneTable_rows_selected]
    })
    
    # Return the reactive for other modules to use
    return(selected_gene)
  })
}