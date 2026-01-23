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
#' @importFrom shiny numericInput selectizeInput shinyApp
#' @importFrom shiny observeEvent p plotOutput
#' @importFrom shiny reactive reactiveVal renderPlot renderText req
#' @importFrom shiny selectInput showModal tagList tags updateSelectInput
#' @importFrom shiny validate verbatimTextOutput showNotification removeNotification
#' @importFrom shiny nearPoints reactiveValues callModule fileInput renderPrint
#' @importFrom shiny renderUI uiOutput observe withProgress incProgress
#' @importFrom tidyr unite
#' @importFrom tidyselect all_of
#' @importFrom shinyjs useShinyjs delay
#' @import yaml
#' @import pals
#' 
#' @param rse SummarizedExperiment object. If NULL (the default), the app will start with a file upload screen.
#' @param app_name Title name that goes on the top left of the Shiny app
#' @param primary_color The title bar color
#' @param secondary_color The plot action button color
#' @param computer_data_dir Optional folder path to existing SummarizedExperiment RDS files.
#'
geyser <- function(rse = NULL, 
                   app_name = "geyser",
                   primary_color = "#3A5836",
                   secondary_color = "#d5673e",
                   computer_data_dir = NULL) {
  
  # Set max file upload size to 1GB
  options(shiny.maxRequestSize = 1000*1024^2)
  
  available_files <- if (!is.null(computer_data_dir) && dir.exists(computer_data_dir)) { 
    list.files(computer_data_dir, pattern = "\\.rds$", ignore.case = TRUE) 
  } else {
    character(0) 
  }
  
  # Identify available preset YAML files in a 'presets' directory
  preset_choices <- if (dir.exists("presets")) {
    list.files("presets", pattern = "\\.yaml$|\\.yml$")
  } else {
    NULL
  }
  
  ui <- page_navbar(
    id = "main_nav",
    title = app_name,
    theme = theme_ui(primary_color = primary_color, 
                     secondary_color = secondary_color),
    header = tagList(
      useShinyjs(), # Enable shinyjs for delays
      tags$style(HTML('table.dataTable tr.active td, table.dataTable tr.active 
                      {background-color: #3A5836 !important;}')),
      tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected 
                      {background-color: pink !important;}'))
    ),
    sidebar = sidebar(
      width = 380,
      conditionalPanel(
        condition = "input.main_nav == 'Data Overview'",
        h4("Load Data"),
        navset_card_tab(
          id = "load_method",
          nav_panel("Upload",
                    fileInput("rse_upload", "Upload .rds File:", accept = ".rds")
          ), 
          if (!is.null(computer_data_dir) && length(available_files) > 0) {
            nav_panel("From Folder",
                      selectizeInput("computer_file_select", "Available Datasets:", choices = available_files, options = list(dropdownParent = 'body')),
                      actionButton("load_computer_file_button", "Load Selected File", 
                                   icon = icon("hdd"), class = "btn-primary w-100")
            )
          }
        )
      ),
      conditionalPanel(
        condition = "input.main_nav == 'Plotting'",
        h4("Plot Parameters"),
        
        # --- Config & Presets Card ---
        card(
          h5("Config & Presets"),
          if (!is.null(preset_choices) && length(preset_choices) > 0) {
            selectInput("config_preset", "Choose a Preset:", choices = c("None" = "", preset_choices))
          },
          fileInput("config_upload", "Upload Config (.yaml):", accept = c(".yaml", ".yml")),
          downloadButton("config_download", "Download Current Config", class = "btn-outline-secondary btn-sm w-100"),
          style = "margin-bottom: 10px;"
        ),
        
        accordion(
          multiple = TRUE,
          open = c("Grouping and Features", "Group Filtering"),
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
    nav_panel(title = "Data Overview", value = "Data Overview", uiOutput("data_load_content")),
    nav_panel(title = "Plotting", value = "Plotting", uiOutput("plotting_content")),
    nav_spacer(),
    nav_menu(
      title = "Info", align = "right",
      quick_start_ui(),
      nav_item(tags$a("Code Source (external link)", href = "https://github.com/davemcg/geyser", target = "_blank"))
    )
  )
  
  server <- function(input, output, session) {
    rv <- reactiveValues(
      rse_object = rse,
      data_source_name = if (!is.null(rse)) "pre-loaded data" else NULL,
      pending_config = NULL,  # Store config to apply after feature_col updates
      pending_group_filters = NULL,  # Store group filters to apply after UI is created
      pending_sample_filter = NULL,  # Store sample filter rows to apply
      clear_group_filters = FALSE,  # Flag to clear group filters
      clear_sample_filter = FALSE  # Flag to clear sample filter
    )
    
    # --- Helper Function to Apply Config ---
    apply_config <- function(config) {
      req(config)
      if (!is.null(config$groupings)) updateSelectizeInput(session, "groupings", selected = config$groupings)
      if (!is.null(config$slot)) updateSelectizeInput(session, "slot", selected = config$slot)
      if (!is.null(config$color_by)) updateSelectizeInput(session, "color_by", selected = config$color_by)
      if (!is.null(config$color_palette)) updateSelectInput(session, "color_palette", selected = config$color_palette)
      if (!is.null(config$label_by)) updateSelectizeInput(session, "label_by", selected = config$label_by)
      
      # Store the config for the feature_col observer to use
      if (!is.null(config$feature_col) || !is.null(config$features)) {
        rv$pending_config <- config
        if (!is.null(config$feature_col)) {
          updateSelectizeInput(session, "feature_col", selected = config$feature_col)
        }
      }
      
      # Apply group filters
      if (!is.null(config$group_filters)) {
        # Store group filters to apply after dynamic inputs are created
        rv$pending_group_filters <- config$group_filters
      }
      
      # Apply sample filtering (row selections in the table)
      if (!is.null(config$sample_filter_rows)) {
        rv$pending_sample_filter <- config$sample_filter_rows
      }
    }
    
    # --- Config Handlers ---
    observeEvent(input$config_upload, {
      req(input$config_upload)
      config <- yaml::read_yaml(input$config_upload$datapath)
      apply_config(config)
      showNotification("External configuration applied.", type = "message")
    })
    
    observeEvent(input$config_preset, {
      req(input$config_preset != "")
      config <- yaml::read_yaml(file.path("presets", input$config_preset))
      apply_config(config)
      showNotification(paste("Preset", input$config_preset, "applied."), type = "message")
    })
    
    output$config_download <- downloadHandler(
      filename = function() { paste0("geyser_config_", Sys.Date(), ".yaml") },
      content = function(file) {
        # Collect group filters
        group_filters <- list()
        if (!is.null(input$groupings) && length(input$groupings) > 0) {
          for (g in input$groupings) {
            filter_input_id <- paste0("dynamic_filter_", g)
            filter_value <- input[[filter_input_id]]
            if (!is.null(filter_value) && length(filter_value) > 0) {
              group_filters[[g]] <- as.character(filter_value)
            }
          }
        }
        
        # Get selected rows from sample filtering table
        selected_rows <- input$table_rows_selected
        
        config_list <- list(
          groupings = input$groupings,
          feature_col = input$feature_col,
          features = input$features,
          slot = input$slot,
          color_by = input$color_by,
          color_palette = input$color_palette,
          label_by = input$label_by
        )
        
        # Always add group_filters (empty string if none)
        if (length(group_filters) > 0) {
          config_list$group_filters <- group_filters
        } else {
          config_list$group_filters <- ""
        }
        
        # Always add sample_filter_rows (empty string if none)
        if (!is.null(selected_rows) && length(selected_rows) > 0) {
          config_list$sample_filter_rows <- as.integer(selected_rows)
        } else {
          config_list$sample_filter_rows <- ""
        }
        
        yaml::write_yaml(config_list, file)
      }
    )
    
    # ---- Dynamic UI Rendering ----
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
        meta_list <- S4Vectors::metadata(rv$rse_object)
        tagList(
          card(
            full_screen = TRUE,
            min_height = "200px",
            card_header("RSE Metadata", class = 'bg-dark'),
            card_body(
              if (length(meta_list) > 0) {
                verbatimTextOutput("meta_raw")
              } else {
                p("No metadata available in this SummarizedExperiment object.")
              }
            )
          ),
          card(
            full_screen = TRUE,
            card_header("Full Sample Metadata (colData)", class = 'bg-dark'),
            card_body(DT::dataTableOutput("table_full", width = "100%", fill = FALSE))
          )
        )
      }
    })
    
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
                                   checkboxInput("row_clust", label = "Cluster Rows", value = TRUE),
                                   radioButtons("heatmap_axis", label = "Assign Row Axis: ", choices = c("Feature", "Sample"), selected = "Feature", inline = TRUE ),
                                   checkboxInput("collapse_samples", label = "Collapse Samples (mean)", value = FALSE)),
                    actionButton('hm_plot_button','Draw Heatmap'),
                    plotOutput("hm_plot",height = '100%')
          )
        )
      }
    })
    
    # ---- Data Loading Observers ----
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
    
    observeEvent(input$load_computer_file_button, {
      req(input$computer_file_select)
      file_path <- file.path(computer_data_dir, input$computer_file_select)
      withProgress(message = paste("Loading", input$computer_file_select), value = 0, {
        incProgress(0.3, detail = "Reading file from disk...")
        tryCatch({
          loaded_rse <- readRDS(file_path)
          incProgress(0.6, detail = "Validating SummarizedExperiment object...")
          if (inherits(loaded_rse, "SummarizedExperiment")) {
            rv$rse_object <- loaded_rse
            rv$data_source_name <- input$computer_file_select
          } else {
            showNotification("Error: Selected file is not a SummarizedExperiment object.", type = "error")
          }
        }, error = function(e) {
          showNotification(paste("Error reading RDS file:", e$message), type = "error")
        })
        incProgress(0.1, detail = "Done.")
      })
    })
    
    # ---- Core Application Logic ----
    observeEvent(rv$rse_object, {
      req(rv$rse_object)
      if ((colnames(rv$rse_object) %>% grep("^\\d", .) %>% length()) > 0){
        showModal(modalDialog(title = "Column name error!",
                              "R hates column names that begin with a digit!
                              Close this app and edit the SummarizedExperiment object
                              `colnames` to not begin with numbers please.",
                              easyClose = FALSE, footer = NULL))
      }
      updateSelectizeInput(session, 'groupings', choices = colnames(colData(rv$rse_object)) %>% sort(), server = TRUE)
      updateSelectizeInput(session, 'feature_col', choices = c("row names", colnames(rowData(rv$rse_object))), selected = 'row names', server = TRUE)
      updateSelectizeInput(session, 'slot', choices = assays(rv$rse_object) %>% names() %>% sort(),
                           selected = if ('counts' %in% names(assays(rv$rse_object))) {'counts'} else {names(assays(rv$rse_object))[1]}, server = TRUE)
      updateSelectizeInput(session, 'color_by', choices = c("", colnames(colData(rv$rse_object))), selected = '', server = TRUE)
      updateSelectizeInput(session, 'label_by', choices = c("", colnames(colData(rv$rse_object))), selected = '', server = TRUE)
      
      # After updating choices, check for and apply embedded config
      shinyjs::delay(300, {
        apply_embedded_config(isolate(rv$rse_object), session, rv)
      })
    })
    
    observeEvent(input$feature_col, {
      req(rv$rse_object, input$feature_col)
      
      # Update the choices
      if (input$feature_col == 'row names'){
        available <- row.names(rv$rse_object)
      } else {
        available <- as.character(rowData(rv$rse_object)[,input$feature_col])
      }
      
      # Check if we have a pending config with features to select
      has_pending_features <- !is.null(rv$pending_config) && !is.null(rv$pending_config$features)
      
      if (has_pending_features) {
        # If we're applying a config, we need to include the selected features in the initial options
        # so they're available on the client side (important for server-side selectize)
        features_to_select <- as.character(isolate(rv$pending_config$features))
        valid_features <- intersect(features_to_select, available)
        
        # Update with the valid features pre-selected
        updateSelectizeInput(session, 'features', 
                             choices = available, 
                             selected = valid_features, 
                             server = TRUE)
        
        # Warn about invalid features
        invalid_features <- setdiff(features_to_select, available)
        if (length(invalid_features) > 0) {
          showNotification(
            paste("Warning: Features not found:", paste(invalid_features, collapse = ", ")),
            type = "warning",
            duration = 5
          )
        }
        
        rv$pending_config <- NULL  # Clear the pending config
      } else {
        # Normal update without selection
        updateSelectizeInput(session, 'features', choices = available, selected = NULL, server = TRUE)
      }
    })
    
    output$dynamic_group_filters_ui <- renderUI({
      req(rv$rse_object, length(input$groupings) > 0)
      filter_inputs <- lapply(input$groupings, function(group_col) {
        # Get values and handle NAs explicitly
        values <- colData(rv$rse_object)[[group_col]]
        
        # Convert to character and replace NA with a string representation
        char_values <- as.character(values)
        char_values[is.na(values)] <- "NA"
        
        # Get unique choices and sort (NA will be sorted alphabetically)
        choices <- unique(char_values) %>% sort()
        
        selectizeInput(inputId = paste0("dynamic_filter_", group_col), 
                       label = paste("Filter by", group_col, ":"), 
                       choices = choices, 
                       multiple = TRUE, 
                       selected = NULL)
      })
      
      # Apply pending group filters using helper function
      apply_pending_group_filters(rv, input, session)
      
      tagList(filter_inputs)
    })
    
    rse_filtered_by_group <- reactive({
      rse <- rv$rse_object
      req(rse)
      active_groupings <- input$groupings
      filtered_cd <- colData(rse) %>% as.data.frame()
      any_filter_active <- FALSE
      for (g in active_groupings) {
        selected_values <- input[[paste0("dynamic_filter_", g)]]
        if (!is.null(selected_values) && length(selected_values) > 0) {
          any_filter_active <- TRUE
          
          # Handle "NA" string by converting back to actual NA for filtering
          if ("NA" %in% selected_values) {
            # Include rows where value is NA OR in the selected non-NA values
            other_values <- selected_values[selected_values != "NA"]
            if (length(other_values) > 0) {
              filtered_cd <- filtered_cd %>% 
                dplyr::filter(is.na(.data[[g]]) | .data[[g]] %in% other_values)
            } else {
              # Only "NA" is selected
              filtered_cd <- filtered_cd %>% dplyr::filter(is.na(.data[[g]]))
            }
          } else {
            # No "NA" selected, normal filtering
            filtered_cd <- filtered_cd %>% dplyr::filter(.data[[g]] %in% selected_values)
          }
        }
      }
      if (any_filter_active) {
        samples_to_keep <- rownames(filtered_cd)
        if (length(samples_to_keep) == 0) {
          showModal(modalDialog(title = "No Samples Found", "Zero samples left, please revise the filtering", easyClose = TRUE, footer = NULL))
        }
        return(rse[, samples_to_keep])
      } else { return(rse) }
    })
    
    exp_plot_reactive <- eventReactive(input$exp_plot_button, {
      req(rse_filtered_by_group())
      .exp_plot(input, rse_filtered_by_group(), rv$data_source_name)
    })
    
    output$exp_plot <- renderPlot({ exp_plot_reactive()$plot }, height = function() {
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
    
    hm_plot_reactive <- eventReactive(input$hm_plot_button, {
      req(rse_filtered_by_group())
      .hm_plot(input, rse_filtered_by_group(), rv$data_source_name)
    })
    
    output$hm_plot <- renderPlot({ draw(hm_plot_reactive()$plot) }, height = function() {
      input$hm_plot_button 
      isolate({
        if (!is.null(input$custom_plot_height) && !is.na(input$custom_plot_height) && input$custom_plot_height >= 200) {
          return(input$custom_plot_height)
        } else {
          req(hm_plot_reactive())
          return(max(400, 0.3 * hm_plot_reactive()$grouping_length))
        }
      })
    })
    
    output$table <- DT::renderDataTable({
      req(rse_filtered_by_group())
      colData(rse_filtered_by_group()) %>% data.frame() %>% rownames_to_column('rse_sample_id') %>% 
        select('rse_sample_id', any_of(input$groupings)) %>%
        DT::datatable(rownames= FALSE, options = list(autoWidth = TRUE, pageLength = 15, dom = 'tp'), filter = list(position = 'top', clear = FALSE))
    }, server = TRUE)
    
    # Apply pending sample filter using helper function
    apply_pending_sample_filter(rv, input)
    
    proxy <- DT::dataTableProxy('table')
    observeEvent(input$clear_colData_row_selections, { proxy %>% DT::selectRows(NULL) })
    
    output$table_full <- DT::renderDataTable({
      req(rv$rse_object)
      colData(rv$rse_object) %>% data.frame() %>% rownames_to_column('rse_sample_id') %>% 
        DT::datatable(rownames= FALSE, options = list(autoWidth = TRUE, pageLength = 25), filter = list(position = 'top', clear = FALSE), selection = 'none')
    }, server = TRUE)
    
    
    output$meta_raw <- renderPrint({
      S4Vectors::metadata(rv$rse_object)
    })
    
    session$onSessionEnded(function() { stopApp() })
  }

  
  app <- shinyApp(ui, server)
  return(app)
}