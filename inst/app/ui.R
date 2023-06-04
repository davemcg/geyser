library(shiny)
library(bslib)
library(htmltools)
ui <-  page_navbar(
  title = app_name,
  theme = bslib::bs_theme(version =  5,
                          primary = "#3A5836",
                          secondary = "#3A5836",
                          font_scale = 0.8,
                          bootswatch = 'united',
                          "dark" = "#3A5836"),
  selected = "Full Sample Metadata",
  collapsible = TRUE,
  tabPanel(
    title = "Full Sample Metadata",
    card(
      full_screen = TRUE,
      card_header("colData", class = 'bg-dark'),
      card_body_fill(
        DT::dataTableOutput("table_full", width = "85%",fill = FALSE)
      )
    )
  ),
  
  tabPanel(
    title = "Plotting",
    fluidRow(
      selectizeInput("groupings",
                     "Sample Grouping(s):",
                     choices = NULL,
                     multiple = TRUE,
      ),
      selectInput("plot",
                  "Plot:",
                  choices = c("Box Plot", "Heatmap"),
                  selected = "Box Plot",
                  multiple = FALSE,
      )),
    layout_column_wrap(
      width = NULL, fill = TRUE,
      style = css(grid_template_columns = "1.5fr 1fr"),
      card(
        full_screen = TRUE,
        card_header("Visualization", class = 'bg-dark'),
        card_footer(
          layout_column_wrap(width = 0.5,
                             selectizeInput("genes",
                                            "Gene(s): ",
                                            choices = NULL,
                                            multiple = TRUE),
                             selectizeInput("slot",
                                            "Assay Type:",
                                            choices = NULL,
                                            multiple = FALSE
                             )
          ),
          conditionalPanel(condition = "input.plot == 'Box Plot'",
                           layout_column_wrap(width = 0.5, checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE))),
          conditionalPanel(condition = "input.plot == 'Heatmap'",
                           layout_column_wrap(width = "100px",
                                              checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE),
                                              checkboxInput("col_clust", label = "Cluster Columns", value = TRUE),
                                              checkboxInput("row_clust", label = "Cluster Rows", value = TRUE))),
          actionButton('exp_plot_button','(Re)Draw Plot!')
        ),
        card_body_fill(
          conditionalPanel(condition = "input.plot == 'Box Plot'",
                           plotOutput("exp_plot",height = '100%')
          ),
          conditionalPanel(condition = "input.plot == 'Heatmap'",
                           plotOutput("hm_plot",height = '100%')
          ))
        
      ),
      card(full_screen = TRUE,
           card_header("Grouping Table (select rows to filter plot)", class = 'bg-dark'),
           card_body_fill(
             DT::dataTableOutput("table",width = "85%",fill = FALSE)
           )
      )
    )
  )
)
