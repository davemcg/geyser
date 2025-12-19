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
  user_selected_feature <- rowid <- sample_unique_id <- counts <- group <- NULL
  features <- input$features
  groupings <- input$groupings
  
  if (length(features) < 1 || length(groupings) < 1){
    showModal(modalDialog(title = "Heatmap Error",
                          "Have you specified at least one grouping and one feature?",
                          easyClose = TRUE,
                          footer = NULL))
    stop()
  }
  
  # pull feature counts and left_join with colData
  if (input$feature_col != 'row names' && ncol(rowData(rse)) > 0) {
    feature_logical <- rowData(rse)[,input$feature_col] %in% features
  } else {
    feature_logical <- features
  }
  pdata <- assay((rse), input$slot)[feature_logical, ,drop = FALSE] %>%
    data.frame() %>% 
    rownames_to_column('user_selected_feature') %>% 
    pivot_longer(-user_selected_feature, values_to = 'counts', names_to = 'sample_unique_id') %>%
    left_join(colData((rse)) %>%
                data.frame() %>% 
                rownames_to_column('sample_unique_id') %>% 
                mutate(rowid = row_number()),
              by = 'sample_unique_id')
  if (length(input$table_rows_selected)){
    pfdata <- pdata %>% filter(rowid %in% input$table_rows_selected)
  } else {
    pfdata <- pdata
  }
  # optional (but set as default) log2 scaling
  if (input$expression_scale){
    pfdata$counts <- log2(pfdata$counts + 1)
    lab_text <- "scale(log2(c))"
  } else {
    lab_text <- "scale(c)"
  }
  output <- list()
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
  hm_data <- t(scale(t(pfdf[,pfdata$sample_unique_id %>% unique()])))
  
  row_clustering <- input$row_clust
  if (min(rowSums(hm_data) <= 1, na.rm = TRUE)){
    row_clustering <- FALSE
  }
  
  # Create main heatmap object with the caption as a bottom column title
  hm <- Heatmap(hm_data,
                column_split = pfdata %>%  pull(group) %>% head(ncol(hm_data)),
                #column_title_rot = 90,
                column_labels = col_labels,
                cluster_columns = input$col_clust,
                cluster_rows = row_clustering,
                name = lab_text,
                # Add caption as a bottom-aligned title
                column_title = paste0("geyser\n", data_source_name),
                column_title_side = "bottom",
                column_title_gp = gpar(fontsize = 10, fontface = "italic")
  )
  
  output$plot <- hm
  
  output$grouping_length <- nrow(pfdata) + (nchar(as.character(pfdata$group)) %>% max()) + (nchar(pfdf) %>% max())
  output
}