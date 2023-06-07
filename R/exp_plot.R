#' @title exp_plot
#'
#' @description draws the expression box plot
#'
#' @keywords internal
#'
#' @import SummarizedExperiment
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @import ggbeeswarm 
#' @import tibble
#'
#' @param input From ui.R
#' @param rse_name Name of the rse object
#' @param slot which slot to pull the count data from the rse assay
#'
#' @details
#'
#' Makes the box plot for the geyser Shiny app
#'
#' @author David McGaughey
#'
#' @examples
#'
#' \dontrun{
#' exp_plot(input, rse_name, 'counts')
#' }
#'

exp_plot <- function(input, rse_name, slot){
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
    ylab_text <- "log2(expression)"
  } else {
    ylab_text <- "expression"
  }
  output <- list()
  pfdata <- pfdata %>%
    # make custom column with user selected groupings of columns
    unite("group", all_of(groupings), remove = FALSE, sep = " | ")
  output$plot <- pfdata %>%
    ggplot(aes(x=group,y=counts)) +
    geom_boxplot() +
    ggbeeswarm::geom_beeswarm() +
    coord_flip() +
    xlab(paste0(groupings, collapse = ' | ')) +
    ylab(ylab_text) +
    theme_linedraw(base_size = 16) +
    facet_wrap(~Gene, ncol = 1)
  output$grouping_length <- pfdata$group %>% unique() %>% length()
  output
}
