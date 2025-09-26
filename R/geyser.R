#' @title geyser
#'
#' @description Run shiny app to use SummarizedExperiment object to display genomics data
#'
#' @export
#' 
#' @import bslib
#' @importFrom dplyr any_of select pull row_number filter
#' @importFrom ComplexHeatmap draw HeatmapAnnotation anno_text
#' @import htmltools
#' @import SummarizedExperiment
#' @importFrom magrittr "%>%"
#' @importFrom tibble rownames_to_column
#' @importFrom shiny NS actionButton br checkboxInput column conditionalPanel
#' @importFrom shiny downloadButton downloadHandler em eventReactive fluidRow
#' @importFrom shiny h4 h5 hr HTML icon isolate modalDialog moduleServer need
#' @importFrom shiny numericInput
#' @importFrom shiny observeEvent p plotOutput
#' @importFrom shiny reactive reactiveVal renderPlot renderText req
#' @importFrom shiny selectInput showModal tagList tags updateSelectInput
#' @importFrom shiny validate verbatimTextOutput showNotification removeNotification
#' @importFrom shiny nearPoints reactiveValues callModule fileInput
#' @importFrom shiny renderUI uiOutput observe
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of
#' @import pals
#' 
#' @param rse SummarizedExperiment object. If NULL (the default), the app will start with a file upload screen.
#' @param app_name Title name that goes on the top left of the Shiny app
#' @param primary_color The title bar color
#' @param secondary_color The plot action button color
#'
#' @details
#'
#' Shiny app uses the rowData rownames to define the genes. The colData field is made
#' fully available to make custom plot groupings.
#'
#' @author David McGaughey
#'
#' @returns 
#' 
#' Shiny app
#' 
#' @examples
#'
#' if (interactive()){
#'   load(system.file('extdata/tiny_rse.Rdata', package = 'geyser'))
#'   geyser(tiny_rse)
#'   # Or launch with no RSE to see the file upload screen
#'   # geyser()
#' }
#'

geyser <- function(rse = NULL, 
                   app_name = "geyser",
                   primary_color = "#3A5836",
                   secondary_color = "#d5673e") {
  
  # Set max file upload size to 1GB
  options(shiny.maxRequestSize = 1000*1024^2)
  
  # Assumes a 'data' subfolder in the app directory
  server_data_dir <- "geyserdata"
  available_files <- if (dir.exists(server_data_dir)) {
    list.files(server_data_dir, pattern = "\\.rds$", ignore.case = TRUE)
  } else {
    character(0)
  }
  
  ui <- page_navbar(
    id = "main_nav", # ID for observing active tab
    title = app_name,
    theme = theme_ui(primary_color = primary_color, 
                     secondary_color = secondary_color),
    header = tagList(
      tags$style(HTML('table.dataTable tr.active td, table.dataTable tr.active 
                      {background-color: #3A5836 !important;}')),
      tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected 
                      {background-color: pink !important;}'))
    ),
    sidebar = sidebar(
      width = 380,
      # Conditionally show data loading controls
      conditionalPanel(
        condition = "input.main_nav == 'Data Overview'",
        h4("Load Data"),
        navset_card_tab(
          id = "load_method",
          nav_panel("Upload",
                    p(em("Upload an .rds file containing a SummarizedExperiment object.")),
                    fileInput("rse_upload", "Upload .rds File:", accept = ".rds")
          ), 
          nav_panel("From Server",
                    p(em("Select a pre-loaded dataset.")),
                    selectInput("server_file_select", 
                                "Available Datasets:", 
                                choices = available_files),
                    actionButton("load_server_file_button", "Load Selected File", 
                                 icon = icon("hdd"), class = "btn-primary w-100")
          )
        )
      ),
      # Conditionally show plotting controls
      conditionalPanel(
        condition = "input.main_nav == 'Plotting'",
        h4("Plot Parameters"),
        accordion(
          multiple = TRUE,
          open = "Grouping and Features",
          accordion_panel(
            "Grouping and Features",
            selectizeInput("groupings", "Sample Grouping(s):", choices = NULL, multiple = TRUE),
            selectizeInput("feature_col", "Assay Feature: ", choices = NULL, multiple = FALSE),
            selectizeInput('features', "Features:", choices = NULL, multiple = TRUE),
            selectizeInput("slot", "Assay Type:", choices = NULL, multiple = FALSE),
            selectizeInput("color_by", "Color by:", choices = NULL, multiple = FALSE),
            selectInput("color_palette", "Color Palette:",
                        choices = list(
                          "ggplot2" = c("Default", "Set1", "Set2", "Set3", "Paired", "Accent", "Dark2", "Pastel1", "Pastel2"),
                          "pals" = c("polychrome", "glasbey", "kelly",  "okabe", "watlington", "stepped", "tol", "trubetskoy")
                        ),
                        selected = "okabe"),
            selectizeInput("label_by", "Label Points by:", choices = NULL, multiple = FALSE),
            layout_column_wrap(width = 0.5, 
                               checkboxInput("expression_scale", label = 'log2(expression)', value = TRUE),
                               checkboxInput("show_points", label = "Plot individual points", value = TRUE)
            ),
            numericInput("custom_plot_height", "Custom Plot Height (pixels, optional):", value = NA, min = 200, step = 50)
          ),
          accordion_panel("Group Filtering", uiOutput("dynamic_group_filters_ui")),
          accordion_panel("Sample Filtering",
                          card(DT::dataTableOutput("table", width = "105%", fill = FALSE),
                               actionButton('clear_colData_row_selections', 'Clear Rows'))
          )
        )
      )
    ),
    nav_panel(
      title = "Data Overview",
      value = "Data Overview",
      uiOutput("data_load_content")
    ),
    nav_panel(
      title = "Plotting",
      value = "Plotting",
      uiOutput("plotting_content")
    ),
    nav_spacer(),
    nav_menu(
      title = "Info",
      align = "right",
      quick_start_ui(),
      nav_item(tags$a("Code Source (external link)", 
                      href = "https://github.com/davemcg/geyser", 
                      target = "_blank"))
    )
  )
  
  server <- function(input, output, session) {
    
    rv <- reactiveValues(
      rse_object = rse,
      data_source_name = if (!is.null(rse)) "pre-loaded data" else NULL
    )
    
    # ---- Dynamic UI Rendering ----
    
    # Render the content for the "Data Overview" tab
    output$data_load_content <- renderUI({
      if (is.null(rv$rse_object)) {
        card(
          card_header("Welcome to Geyser!"),
          card_body(
            p("To begin, please load a SummarizedExperiment object using the controls in the sidebar."),
            p("You can either upload your own .rds file or select a pre-loaded dataset from the server, if available.")
          )
        )
      } else {
        card(
          full_screen = TRUE,
          card_header("Full Sample Metadata (colData)", class = 'bg-dark'),
          card_body(
            DT::dataTableOutput("table_full", width = "100%", fill = FALSE)
          )
        )
      }
    })
    
    # Render the content for the "Plotting" tab
    output$plotting_content <- renderUI({
      if (is.null(rv$rse_object)) {
        card(
          card_header("Plotting Area"),
          card_body(
            p("Please load a dataset to enable plotting."),
            p("Once data is loaded, the plotting controls will appear in the sidebar when this tab is selected.")
          )
        )
      } else {
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
      }
    })
    
    # ---- Data Loading Observers ----
    
    # Observer to handle the file upload
    observeEvent(input$rse_upload, {
      req(input$rse_upload)
      tryCatch({
        uploaded_rse <- readRDS(input$rse_upload$datapath)
        if (inherits(uploaded_rse, "SummarizedExperiment")) {
          rv$rse_object <- uploaded_rse
          rv$data_source_name <- input$rse_upload$name
        } else {
          showNotification("Error: Uploaded file is not a SummarizedExperiment object.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error reading RDS file:", e$message), type = "error")
      })
    })
    
    # Observer to handle loading a server-side file
    observeEvent(input$load_server_file_button, {
      req(input$server_file_select)
      file_path <- file.path(server_data_dir, input$server_file_select)
      
      tryCatch({
        loaded_rse <- readRDS(file_path)
        if (inherits(loaded_rse, "SummarizedExperiment")) {
          rv$rse_object <- loaded_rse
          rv$data_source_name <- input$server_file_select
        } else {
          showNotification("Error: Selected file is not a SummarizedExperiment object.", type = "error")
        }
      }, error = function(e) {
        showNotification(paste("Error reading RDS file:", e$message), type = "error")
      })
    })
    
    # ---- Core Application Logic ----
    
    # Observer to populate inputs and perform checks once the RSE object is available
    observeEvent(rv$rse_object, {
      req(rv$rse_object)
      
      # error checking for input rse -----
      if ((colnames(rv$rse_object) %>% grep("^\\d", .) %>% length()) > 0){
        showModal(modalDialog(title = "Column name error!",
                              "R hates column names that begin with a digit!
                              Close this app and edit the SummarizedExperiment object
                              `colnames` to not begin with numbers please.",
                              easyClose = FALSE,
                              footer = NULL))
      }
      
      # Populate inputs -----
      updateSelectizeInput(session, 'groupings',
                           choices = colnames(colData(rv$rse_object)) %>% sort(),
                           server = TRUE)
      
      updateSelectizeInput(session, 'feature_col',
                           label = 'Assay Feature: ',
                           choices = c("row names", colnames(rowData(rv$rse_object))),
                           selected = 'row names',
                           server = TRUE)
      
      updateSelectizeInput(session, 'slot',
                           choices = assays(rv$rse_object) %>% names() %>% sort(),
                           selected = if ('counts' %in% names(assays(rv$rse_object))) {'counts'} 
                           else {names(assays(rv$rse_object))[1]},
                           server = TRUE)
      
      updateSelectizeInput(session, 'color_by',
                           choices = c("", colnames(colData(rv$rse_object))),
                           selected = '',
                           server = TRUE)
      
      updateSelectizeInput(session, 'label_by',
                           choices = c("", colnames(colData(rv$rse_object))),
                           selected = '',
                           server = TRUE)
    })
    
    observeEvent(input$feature_col, {
      req(rv$rse_object, input$feature_col)
      if (input$feature_col == 'row names'){
        updateSelectizeInput(session, 'features', 
                             label = "Features:",
                             choices = row.names(rv$rse_object), 
                             selected = NULL, 
                             server = TRUE)
      } else {
        the_col <- input$feature_col
        updateSelectizeInput(session, 'features', 
                             label = "Features:",
                             choices = rowData(rv$rse_object)[,the_col],
                             selected = NULL, 
                             server = TRUE)
      }
    })
    
    # Render the dynamic filter controls UI
    output$dynamic_group_filters_ui <- renderUI({
      req(rv$rse_object, length(input$groupings) > 0)
      
      filter_inputs <- lapply(input$groupings, function(group_col) {
        choices <- colData(rv$rse_object)[[group_col]] %>% as.character() %>% unique() %>% sort()
        selectizeInput(inputId = paste0("dynamic_filter_", group_col),
                       label = paste("Filter by", group_col, ":"),
                       choices = choices,
                       multiple = TRUE,
                       selected = NULL)
      })
      tagList(filter_inputs)
    })
    
    # Reactive for RSE object filtered by the intersection of dynamic selections
    rse_filtered_by_group <- reactive({
      rse <- rv$rse_object
      req(rse)
      
      active_groupings <- input$groupings
      filtered_cd <- colData(rse) %>% as.data.frame()
      any_filter_active <- FALSE
      
      for (g in active_groupings) {
        filter_input_id <- paste0("dynamic_filter_", g)
        selected_values <- input[[filter_input_id]]
        
        if (!is.null(selected_values) && length(selected_values) > 0) {
          any_filter_active <- TRUE
          filtered_cd <- filtered_cd %>%
            dplyr::filter(.data[[g]] %in% selected_values)
        }
      }
      
      if (any_filter_active) {
        samples_to_keep <- rownames(filtered_cd)
        if (length(samples_to_keep) == 0) {
          showModal(modalDialog(
            title = "No Samples Found",
            "Zero samples left, please revise the filtering",
            easyClose = TRUE,
            footer = NULL
          ))
        }
        return(rse[, samples_to_keep])
      } else {
        return(rse)
      }
    })
    
    # expression plot ----
    exp_plot_reactive <- eventReactive(input$exp_plot_button, {
      req(rse_filtered_by_group())
      .exp_plot(input, rse_filtered_by_group(), rv$data_source_name)
    })
    
    output$exp_plot <- renderPlot({
      exp_plot_reactive()$plot
    },
    height = function() {
      input$exp_plot_button 
      isolate({
        if (!is.null(input$custom_plot_height) && !is.na(input$custom_plot_height) && input$custom_plot_height >= 200) {
          return(input$custom_plot_height)
        } else {
          req(exp_plot_reactive())
          return(max(600, 30 * length(input$features) * exp_plot_reactive()$grouping_length))
        }
      })
    })
    
    # hm plot -----
    hm_plot_reactive <- eventReactive(input$hm_plot_button, {
      req(rse_filtered_by_group())
      .hm_plot(input, rse_filtered_by_group(), rv$data_source_name)
    })
    
    output$hm_plot <- renderPlot({
      draw(hm_plot_reactive()$plot)
    },
    height = function() {
      input$hm_plot_button 
      isolate({
        if (!is.null(input$custom_plot_height) && !is.na(input$custom_plot_height) && input$custom_plot_height >= 200) {
          return(input$custom_plot_height)
        } else {
          req(hm_plot_reactive())
          return(max(400, 0.7 * hm_plot_reactive()$grouping_length))
        }
      })
    })
    
    # sample data table -----
    output$table <- DT::renderDataTable({
      req(rse_filtered_by_group())
      colData(rse_filtered_by_group()) %>%
        data.frame() %>% 
        rownames_to_column('rse_sample_id') %>% 
        select('rse_sample_id', any_of(input$groupings)) %>%
        DT::datatable(rownames= FALSE,
                      options = list(autoWidth = TRUE, pageLength = 15, dom = 'tp'),
                      filter = list(position = 'top', clear = FALSE))
    }, server = TRUE)
    
    proxy <- DT::dataTableProxy('table')
    observeEvent(input$clear_colData_row_selections, {
      proxy %>% DT::selectRows(NULL)
    })
    
    # sample data table full -----
    output$table_full <- DT::renderDataTable({
      req(rv$rse_object)
      colData(rv$rse_object) %>%
        data.frame() %>% 
        rownames_to_column('rse_sample_id') %>% 
        DT::datatable(rownames= FALSE,
                      options = list(autoWidth = TRUE, pageLength = 25),
                      filter = list(position = 'top', clear = FALSE),
                      selection = 'none')
    }, server = TRUE)
    
    session$onSessionEnded(function() {
      stopApp()
    })
  }
  
  app <- shinyApp(ui, server)
  return(app)
}