library(shiny)
library(bslib)
library(htmltools)
library(DT)


ui <-  page_navbar(
  tags$style(HTML('table.dataTable tr.active td, table.dataTable tr.active {background-color: #3A5836 !important;}')),
  title = app_name,
  theme = bslib::bs_theme(version =  5,
                          primary = "#3A5836",
                          secondary = "#d5673e",
                          font_scale = 0.8,
                          bootswatch = 'united',
                          "accordion-button-active-bg" = "#3A5836",
                          "accordion-button-bg" = "#3A5836",
                          "accordion-button-color" = "white",
                          "accordion-button-active-color" = "white",
                          "accordion-icon-color" = "white",
                          "accordion-icon-active-color" = "white",
                          "dark" = "#3A5836"),
  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: pink !important;}')),
  selected = "Full Sample Metadata",
  collapsible = TRUE,
  nav_panel(
    title = "Full Sample Metadata",
    card(
      full_screen = TRUE,
      card_header("colData", class = 'bg-dark'),
      card_body(
        DT::dataTableOutput("table_full", width = "85%",fill = FALSE)
      )
    )
  ),
  nav_panel(
    title = "Plotting",
    page_fluid(
      #    width = NULL, fill = TRUE,
      #style = css(grid_template_columns = "1.5fr 1fr"),
      layout_sidebar(
        height = '100%',
        sidebar = sidebar(
          accordion(
            multiple = TRUE,
            accordion_panel(
              "Plot Parameters",
              selectizeInput("groupings",
                             "Sample Grouping(s):",
                             choices = NULL,
                             multiple = TRUE,
              ),
              selectizeInput("genes",
                             "Gene(s): ",
                             choices = NULL,
                             multiple = TRUE),
              selectizeInput("slot",
                             "Assay Type:",
                             choices = NULL,
                             multiple = FALSE
              ),
              layout_column_wrap(width = 0.5, checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE)),
            ),
            accordion_panel("Sample Filtering",
                            card(full_screen = TRUE,
                                 DT::dataTableOutput("table",width = "105%",fill = FALSE),
                                 actionButton('clear_colData_row_selections', 'Clear Rows'))
            )
          ),
          width = '40%'),
        navset_card_tab(
          full_screen = TRUE,
          nav_panel("Box Plot",
                    actionButton('exp_plot_button','Draw Box Plot'),
                    plotOutput("exp_plot",height = '100%')
          ),
          nav_panel("Heatmap",
                    layout_columns(fill = FALSE,
                                   checkboxInput("col_clust", label = "Cluster Columns", value = TRUE),
                                   checkboxInput("row_clust", label = "Cluster Rows", value = TRUE)),
                    actionButton('hm_plot_button','Draw Heatmap'),
                    plotOutput("hm_plot",height = '100%')
          )
        )
      )
    )
  ),
  nav_spacer(),
  # nav_panel(title = "Help",
  #           align = 'right',
  #           fluidRow(column(8,"HELP")))
  nav_menu(
    title = "Info",
    align = "right",
    nav_panel("Quick Start",
              page_fluid(
                tags$code("# If needed: BiocManager::install(\"recount3\")"),
                tags$code("library(recount3)"), br(),
                tags$code("library(geyser)"), br(),
                tags$code("library(dplyr)"), br(),
                tags$code("human_projects <- available_projects()"), br(),
                tags$code("proj_info <- subset("), br(),
                tags$code(HTML('&nbsp;'), "  human_projects,"), br(),
                tags$code(HTML('&nbsp;'), "  project == \"SRP107937\" & project_type == \"data_sources\""), br(),
                tags$code(")"), br(),
                tags$code("rse_SRP107937 <- create_rse(proj_info)"), br(),
                tags$code("assay(rse_SRP107937, \"counts\") <- transform_counts(rse_SRP107937)"), br(),
                tags$code("# first tweak that glues the gene name onto the gene id in the row names"), br(),
                tags$code("rownames(rse_SRP107937) <- paste0(rowData(rse_SRP107937)$gene_name, ' (', row.names(rse_SRP107937), ')')"), br(),
                tags$code("# creates two new metadataa fields "), br(),
                tags$code("colData(rse_SRP107937)$tissue <- colData(rse_SRP107937)$sra.sample_title %>% stringr::str_extract(.,'PRC|PR')"), br(),
                tags$code("colData(rse_SRP107937)$disease <- colData(rse_SRP107937)$sra.sample_title %>% stringr::str_extract(.,'AMD|Normal')"), br(),
                tags$code("geyser::geyser(rse_SRP107937, \" geyser: SRP107937\")"), br(), br(),
                tags$iframe(width="800",
                            height="506",
                            src="geyser_example.mp4", frameborder="0", allow="accelerometer; autoplay; gyroscope; picture-in-picture"))
    ),
    nav_panel("Overview",
              tags$h3("Start with the full metadata"),
              img(src = 'help_01.png', width = "600px"),
              "The idea is to figure out which columns to use for the plotting",
              tags$h3("Plotting"),
              img(src = 'help_02.png', width = "600px"),
              "Then go to the plotting section and pick the columns (\"groupings\")",
              tags$h3("Genes"),
              img(src = 'help_03.png', width = "600px"),
              "Now pick the genes",
              tags$h3("Plot!"),
              img(src = 'help_04.png', width = "600px"),
              "Click the plot button to generate your plot of gene by custom grouping",
              tags$h3("Custom filtering"),
              img(src = 'help_05.png', width = "600px"),
              "You can select rows of the table (circle) to only plot these (arrow row NOT selected). You have to click the plot button again to regenerate the new, filtered, plot. The filtering is applied to all plots.",
              img(src = 'help_06.png', width = "200px"),
              "Click the \"Clear Rows\" button to reset your sample filtering"
    ),
    nav_item(tags$a("Code Source (external link)", href = "https://github.com/davemcg/geyser", target = "_blank"))
  )
)

