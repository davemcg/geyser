hm_plot <- function(input, rse_name, slot){
  require(ComplexHeatmap)
  genes <- input$genes
  groupings <- input$groupings
  
  # pull gene counts and left_join with colData
  pdata <- assay(get(rse_name),  input$slot)[genes, ,drop = FALSE] %>%
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
  # optional (but set as default) log2 scaling
  if (input$expression_scale){
    pfdata$counts <- log2(pfdata$counts + 1)
    lab_text <- "scale(log2(c))"
  } else {
    lab_text <- "scale(c)"
  }
  output <- list()
  pfdata <- pfdata %>%
    # make custom column with user selected groupings of columns
    unite("group", all_of(groupings), remove = FALSE, sep = " | ")
  # make df for ComplexHeatmap
  pfdf <- pfdata %>% 
    dplyr::select(Gene, sample_unique_id, counts) %>% 
    pivot_wider(names_from = sample_unique_id, values_from = counts) %>% 
    data.frame()
  row.names(pfdf) <- pfdf$Gene
  pfdf <- pfdf[,-1]
  
  output$plot <- Heatmap(t(scale(t(pfdf[,pfdata$sample_unique_id %>% unique()]))), 
                         column_split = pfdata %>% filter(Gene == genes[1]) %>% pull(group), 
                         column_title_rot = 90,
                         name = lab_text)
  output$grouping_length <- nrow(pfdata) + (nchar(pfdata$group) %>% max()) + (nchar(pfdf) %>% max())
  output
}
