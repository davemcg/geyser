library(shiny)
library(bslib)

ui <-  navbarPage(
  title = "geyser",
  theme = bslib::bs_theme(version =  5,
                          primary = "#3A5836",
                          secondary = "#3A5836",
                          font_scale = 0.8,
                          bootswatch = 'united',
                          "bg-dark" = "black",
                          "navbar-bg" = "#86cecb"),
  selected = "Plotting",
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
      )),
    layout_column_wrap(
      width = NULL, fill = TRUE,
      style = css(grid_template_columns = "1.5fr 1fr"),
      card(
        full_screen = TRUE,
        card_header("Box Plot"),
        card_footer(
          fluidRow(
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
          checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE),
          actionButton('exp_plot_button','(Re)Draw Exp Plot!')
        ),
        card_body_fill(
          plotOutput("exp_plot",height = '100%')
        )

      ),
      card(full_screen = TRUE,
           card_header("Grouping Table (select rows to filter plot)"),
           card_body_fill(
             DT::dataTableOutput("table",width = "85%",fill = FALSE)
           )
      )
    )
  )
)
#
# ui <- shinyUI(
#   navbarPage(
# theme = bs_theme(bg = "rgb(253, 253, 253)", fg = "rgb(2,2,2)",
#                  primary = "#3A5836",
#                  secondary = "#3A5836",
#                  font_scale = 0.8,
#                  bootswatch = "united"),
#     app_name,
#     # Sidebar
#     sidebarLayout(
#       sidebarPanel(
# selectizeInput("groupings",
#                "Select Grouping(s):",
#                choices = NULL,
#                multiple = TRUE,
# ),
#   selectizeInput("genes",
#                  "Select Gene(s): ",
#                  choices = NULL,
#                  multiple = TRUE),
#   selectizeInput("slot",
#                  "Select Assay Type:",
#                  choices = NULL,
#                  multiple = FALSE),
#   width = 3
# ),
#       # Show a plot of the generated distribution
#       mainPanel(
#         tabsetPanel(
#           tabPanel("Box Plot",
#                    br(),
#                    plotOutput("exp_plot",height = '100%'),
#                    checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE),
#                    actionButton('exp_plot_button','(Re)Draw Exp Plot!')),
#           tabPanel("Sample Data Grouping(s)",
#                    br(),
#                    "(Optional) Select rows to filter the samples used in the box plot",
#                    DT::dataTableOutput("table"),
#                    actionButton('clear_colData_row_selections', 'Clear Rows')),
#           tabPanel("Sample Data Full",
#                    br(),
#                    DT::dataTableOutput("table_full")))
#       )
#     )
#   )
# )
