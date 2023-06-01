library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- shinyUI(
  navbarPage(
    theme = bs_theme(bg = "rgb(253, 253, 253)", fg = "rgb(2,2,2)",
                     primary = "#3A5836",
                     secondary = "#3A5836",
                     font_scale = 0.8,
                     bootswatch = "united"),
    app_name,
    # Sidebar 
    sidebarLayout(
      sidebarPanel(
        selectizeInput("groupings",
                       "Select Grouping(s):",
                       choices = NULL,
                       multiple = TRUE,
        ),
        selectizeInput("genes",
                       "Select Gene(s): ",
                       choices = NULL,
                       multiple = TRUE),
        selectizeInput("slot",
                       "Select Assay Type:",
                       choices = NULL,
                       multiple = FALSE), 
        width = 3
      ),
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Box Plot",
                   br(),
                   plotOutput("exp_plot",height = '100%'),
                   checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE),
                   actionButton('exp_plot_button','(Re)Draw Exp Plot!')),
          tabPanel("Sample Data Grouping(s)",
                   br(),
                   "(Optional) Select rows to filter the samples used in the box plot",
                   DT::dataTableOutput("table"),
                   actionButton('clear_colData_row_selections', 'Clear Rows')),
          tabPanel("Sample Data Full",
                   br(),
                   DT::dataTableOutput("table_full")))
      )
    )
  )
)
