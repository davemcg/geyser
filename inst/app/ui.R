library(shiny)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bs_theme(bootswatch = "pulse"),
  # Application titlerna
  titlePanel("Bhar-ma RNAseq"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectizeInput("groupings",
                     "Grouping(s):",
                     choices = NULL,
                     multiple = TRUE,
      ),
      selectizeInput("genes",
                     "Select gene(s): ",
                     choices = NULL,
                     multiple = TRUE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Exp Plot",
                 br(),
                 plotOutput("exp_plot")),
        tabPanel("Sample Data Groups",
                 br(),
                 DT::dataTableOutput("table"),
                 actionButton('clear_colData_row_selections', 'Clear Rows')),
        tabPanel("Sample Data Full",
                 br(),
                 DT::dataTableOutput("table_full")))

    )

  ),
  actionButton('exp_plot_button','(Re)Draw Exp Plot!')
)
