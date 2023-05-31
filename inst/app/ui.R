library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bg = "rgb(253, 253, 253)", fg = "rgb(2,2,2)",
                   secondary = "#3A5836",
                   font_scale = 0.8,
                   bootswatch = "zephyr"),
  # Application titlerna
  titlePanel(name),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("groupings",
                     "Select Column Grouping(s):",
                     choices = NULL,
                     multiple = TRUE,
      ),
      selectizeInput("genes",
                     "Select Gene(s): ",
                     choices = NULL,
                     multiple = TRUE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Exp Plot",
                 br(),
                 plotOutput("exp_plot"),
                 checkboxInput("expression_scale", label = 'log1p scale expression', value = TRUE),
                 actionButton('exp_plot_button','(Re)Draw Exp Plot!')),
        tabPanel("Sample Data Grouped",
                 br(),
                 DT::dataTableOutput("table"),
                 actionButton('clear_colData_row_selections', 'Clear Rows')),
        tabPanel("Sample Data Full",
                 br(),
                 DT::dataTableOutput("table_full")))

    )

  )
)
