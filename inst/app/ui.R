# set new image path folder
addResourcePath(prefix = 'assets', directoryPath = system.file("assets", package = "geyser"))

ui <-  page_navbar(
  tags$style(HTML('table.dataTable tr.active td, table.dataTable tr.active {background-color: #3A5836 !important;}')),
  title = app_name,
  theme = theme_ui(),
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
  nav_menu(
    title = "Info",
    align = "right",
    quick_start_ui(),
    overview_ui(),
    nav_item(tags$a("Code Source (external link)", href = "https://github.com/davemcg/geyser", target = "_blank"))
  )
)
