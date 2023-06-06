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
  tabPanel(
    title = "Full Sample Metadata",
    card(
      full_screen = TRUE,
      card_header("colData", class = 'bg-dark'),
      card_body(
        DT::dataTableOutput("table_full", width = "85%",fill = FALSE)
      )
    )
  ),
  tabPanel(
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
  )
)

