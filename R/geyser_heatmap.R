#' @title hm_plot
#'
#' @description draws the expression heatmap
#'
#' @keywords internal
#'
#' @import SummarizedExperiment
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom dplyr filter left_join mutate pull select row_number 
#' @importFrom magrittr "%>%"
#' @import tibble 
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom utils head
#' @importFrom grid gpar
#' @importFrom rlang sym
#'
#' 
#' @param input From ui.R
#' @param rse rse object
#' @param data_source_name The name of the loaded dataset for the caption
#'
#' @details
#'
#' Makes the heatmap for the geyser Shiny app
#'
#' @author David McGaughey
#'
#' @returns 
#' 
#' Returns a ComplexHeatmap object
#' 
#' @examples
#' load(system.file('extdata/tiny_rse.Rdata', package = 'geyser'))
#' input <- list()
#' input$feature_col <- 'row names'
#' input$features <- c("TYRP1 (ENSG00000107165.12)","OPN1LW (ENSG00000102076.9)")
#' input$groupings <- c('disease')
#' input$slot <- 'counts'
#' input$expression_scale <- TRUE
#' input$row_clust <- TRUE
#' input$col_clust <- TRUE
#' geyser:::.hm_plot(input, tiny_rse, "tiny_rse.Rdata")$plot

.hm_plot <- function(input, rse, data_source_name){
  user_selected_feature <- rowid <- sample_unique_id <- counts <- group <- rse_row_id <- NULL
  features <- input$features
  groupings <- input$groupings
  
  if (length(features) < 1 || length(groupings) < 1){
    showModal(modalDialog(title = "Heatmap Error",
                          "Have you specified at least one grouping and one feature?",
                          easyClose = TRUE,
                          footer = NULL))
    stop()
  }
  
  # --- Feature Selection Logic ---
  # Safety check for empty rowData or specific column selection
  if (input$feature_col != 'row names' && ncol(rowData(rse)) > 0) {
    feature_logical <- rowData(rse)[,input$feature_col] %in% features
  } else {
    feature_logical <- features
  }
  
  # Pull feature counts
  pdata <- assay((rse), input$slot)[feature_logical, ,drop = FALSE] %>%
    data.frame() %>% 
    rownames_to_column('rse_row_id') %>% 
    pivot_longer(-rse_row_id, values_to = 'counts', names_to = 'sample_unique_id')
  
  # Matches the facet title logic in geyser_exp_plot.R to handle duplicate symbols
  if (input$feature_col != 'row names' && ncol(rowData(rse)) > 0) {
    row_meta <- rowData(rse) %>% 
      data.frame() %>% 
      rownames_to_column('rse_row_id') %>% 
      select(rse_row_id, !!sym(input$feature_col))
    
    pdata <- pdata %>% 
      left_join(row_meta, by = 'rse_row_id') %>% 
      mutate(user_selected_feature = paste0(.data[[input$feature_col]], " (", rse_row_id, ")"))
  } else {
    pdata <- pdata %>% mutate(user_selected_feature = rse_row_id)
  }
  
  # Join with sample metadata (colData)
  pfdata <- pdata %>%
    left_join(colData((rse)) %>%
                data.frame() %>% 
                rownames_to_column('sample_unique_id') %>% 
                mutate(rowid = row_number()),
              by = 'sample_unique_id')
  
  if (length(input$table_rows_selected)){
    pfdata <- pfdata %>% filter(rowid %in% input$table_rows_selected)
  }
  
  # optional (but set as default) log2 scaling
  if (input$expression_scale){
    pfdata$counts <- log2(pfdata$counts + 1)
    lab_text <- "scale(log2(c))"
  } else {
    lab_text <- "scale(c)"
  }
  
  if (length(groupings) == 1){
    pfdata$group <- pfdata[,groupings] %>% pull(1)
  } else {
    pfdata <- pfdata %>%
      unite("group", all_of(groupings), remove = FALSE, sep = " | ")
  }
  
  pfdf <- pfdata %>%
    select(user_selected_feature, sample_unique_id, counts) %>%
    pivot_wider(names_from = sample_unique_id, values_from = counts)
  
  col_labels <- colnames(pfdf)[-1]
  pfdf <- data.frame(pfdf)
  row.names(pfdf) <- pfdf$user_selected_feature
  pfdf <- pfdf[,-1]
  
  # Scaling across samples
  hm_data <- t(scale(t(pfdf[,pfdata$sample_unique_id %>% unique()])))
  
  row_clustering <- input$row_clust
  if (min(rowSums(hm_data) <= 1, na.rm = TRUE)){
    row_clustering <- FALSE
  }
  
  # Create Heatmap object
  hm <- Heatmap(hm_data,
                column_split = pfdata %>%  pull(group) %>% head(ncol(hm_data)),
                column_labels = col_labels,
                cluster_columns = input$col_clust,
                cluster_rows = row_clustering,
                name = lab_text,
                column_title = paste0("geyser\n", data_source_name),
                column_title_side = "bottom",
                column_title_gp = gpar(fontsize = 10, fontface = "italic")
  )
  
  output <- list()
  output$plot <- hm
  output$grouping_length <- nrow(pfdata) + (nchar(as.character(pfdata$group)) %>% max()) + (nchar(pfdf) %>% max())
  output
}