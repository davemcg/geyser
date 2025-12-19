#' @title exp_plot
#'
#' @description draws the expression box plot
#'
#' @keywords internal
#'
#' @import SummarizedExperiment
#' @import ggplot2
#' @import pals
#' @import RColorBrewer
#' @importFrom ggbeeswarm geom_quasirandom position_quasirandom
#' @importFrom ggrepel geom_text_repel
#' @importFrom dplyr filter left_join mutate pull row_number 
#' @importFrom magrittr "%>%"
#' @importFrom tidyr all_of pivot_longer pivot_wider unite
#' @importFrom tibble rownames_to_column
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head
#' @importFrom stats setNames
#' 
#' @param input From ui.R
#' @param rse The rse object
#' @param data_source_name The name of the loaded dataset for the caption
#'
#' @details
#'
#' Makes the box plot for the geyser Shiny app
#'
#' @author David McGaughey
#'
#' @returns 
#' 
#' Returns a list with the $plot slot holding ggplot object and $grouping_length contains
#' the number of features to scale the plot
#' 
#' @examples
#'
#' load(system.file('extdata/tiny_rse.Rdata', package = 'geyser'))
#' input <- list()
#' input$feature_col <- "row names"
#' input$features <- c("TYRP1 (ENSG00000107165.12)","OPN1LW (ENSG00000102076.9)")
#' input$groupings <- c('disease')
#' input$slot <- 'counts'
#' input$expression_scale <- TRUE
#' input$color_by <- 'tissue'
#' input$color_palette <- 'polychrome'
#' input$show_points <- TRUE
#' geyser:::.exp_plot(input, tiny_rse, "tiny_rse.Rdata")$plot

.exp_plot <- function(input, rse, data_source_name){
  user_selected_feature <- rowid <- group <- counts <- geyser_group <- geyser_color_by <- geyser_label_by <- NULL
  
  features <- input$features
  groupings <- input$groupings
  
  if (length(features) < 1 || length(groupings) < 1){
    showModal(modalDialog(title = "Box Plot Error",
                          "Have you specified at least one grouping and one feature?",
                          easyClose = TRUE,
                          footer = NULL))
    stop()
  }
  
  # Pull feature counts and left_join with colData slot
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
  
  # Optional (but set as default) log2 scaling
  if (input$expression_scale){
    pfdata$counts <- log2(pfdata$counts + 1)
    ylab_text <- paste0("log2(", input$slot, ")")
  } else {
    ylab_text <- input$slot
  }
  output <- list()
  
  # --- Data Preparation Step ---
  if (length(groupings) == 1){
    pfdata$geyser_group <- pfdata[,groupings] %>% pull(1)
  } else {
    pfdata <- pfdata %>%
      unite("geyser_group", all_of(groupings), remove = FALSE, sep = " | ")
  }
  
  if (input$color_by != ''){
    pfdata$geyser_color_by <- as.factor(pfdata[,input$color_by] %>% pull(1))
  }
  
  if (!is.null(input$label_by) && input$label_by != '') {
    pfdata$geyser_label_by <- pfdata[, input$label_by] %>% pull(1)
  }
  
  # --- Plotting Step ---
  if (input$color_by != ''){
    p <- ggplot(pfdata, aes(y=geyser_group, x=counts, color = geyser_color_by, group = geyser_group))
  } else {
    p <- ggplot(pfdata, aes(y=geyser_group, x=counts, group = geyser_group))
  }
  
  p <- p + geom_boxplot(alpha = 0.5, outlier.shape = NA)
  
  if (!is.null(input$show_points) && input$show_points) {
    pos <- position_quasirandom(dodge.width = 0.75)
    if (input$color_by != '') {
      p <- p + geom_quasirandom(position = pos, orientation = 'y')
    } else {
      p <- p + geom_quasirandom(orientation = 'y')
    }
  }
  
  if (!is.null(input$label_by) && input$label_by != '') {
    p <- p + geom_text_repel(
      aes(label = geyser_label_by),
      position = pos,
      max.overlaps = Inf,
      box.padding = 0.6,
      min.segment.length = 0,
      size = 3.5
    )
  }
  
  # Add remaining plot elements
  p <- p +
    labs(caption = paste0("geyser\n",data_source_name)) +
    xlab(paste0(groupings, collapse = ' | ')) +
    ylab(ylab_text) +
    theme_linedraw(base_size = 16) +
    facet_wrap(~user_selected_feature, ncol = 1)
  
  # Add legend title and custom color palette if color is used
  if (input$color_by != '') {
    p <- p + guides(col = guide_legend(title = input$color_by))
    
    if (!is.null(input$color_palette) && input$color_palette != 'Default') {
      the_levels <- levels(pfdata$geyser_color_by)
      num_colors <- length(the_levels)
      pals_palettes <- c("polychrome", "glasbey", "kelly",  "okabe", "watlington", "stepped", "tol", "trubetskoy")
      brewer_palettes <- c("Set1", "Set2", "Set3", "Paired", "Accent", "Dark2", "Pastel1", "Pastel2")
      unnamed_colors <- NULL
      if (input$color_palette %in% brewer_palettes) {
        palette_info <- RColorBrewer::brewer.pal.info[input$color_palette, ]
        max_colors <- palette_info$maxcolors
        if (num_colors <= max_colors) {
          if (num_colors < 3) {
            base_colors <- RColorBrewer::brewer.pal(3, input$color_palette)
            unnamed_colors <- head(base_colors, num_colors)
          } else {
            unnamed_colors <- RColorBrewer::brewer.pal(num_colors, input$color_palette)
          }
        } else {
          base_colors <- RColorBrewer::brewer.pal(max_colors, input$color_palette)
          color_func <- grDevices::colorRampPalette(base_colors)
          unnamed_colors <- color_func(num_colors)
        }
      } else if (input$color_palette %in% pals_palettes) {
        palette_func <- get(input$color_palette, asNamespace("pals"))
        base_colors <- palette_func(num_colors)
        if (length(base_colors) < num_colors) {
          color_func <- grDevices::colorRampPalette(base_colors)
          unnamed_colors <- color_func(num_colors)
        } else {
          unnamed_colors <- base_colors
        }
      }
      if (!is.null(unnamed_colors)) {
        custom_colors <- setNames(unnamed_colors, the_levels)
        p <- p + scale_color_manual(values = custom_colors)
      }
    }
  }
  
  output$plot <- p
  
  output$grouping_length <- pfdata$geyser_group %>% unique() %>% length()
  output
}