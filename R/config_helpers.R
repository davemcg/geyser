#' Configuration Helper Functions for Geyser
#'
#' @description Functions to handle loading, saving, and applying configurations
#'

#' Apply a configuration to the Shiny session
#'
#' @param config List containing configuration parameters
#' @param session Shiny session object
#' @param rv reactiveValues object containing app state
#'
#' @importFrom shiny updateSelectizeInput updateSelectInput isolate
#'
apply_config <- function(config, session, rv) {
  req(config)
  
  # Update simple inputs
  if (!is.null(config$groupings)) {
    updateSelectizeInput(session, "groupings", selected = config$groupings)
  }
  if (!is.null(config$slot)) {
    updateSelectizeInput(session, "slot", selected = config$slot)
  }
  if (!is.null(config$color_by)) {
    updateSelectizeInput(session, "color_by", selected = config$color_by)
  }
  if (!is.null(config$color_palette)) {
    updateSelectInput(session, "color_palette", selected = config$color_palette)
  }
  if (!is.null(config$label_by)) {
    updateSelectizeInput(session, "label_by", selected = config$label_by)
  }
  
  # Store the config for the feature_col observer to use
  if (!is.null(config$feature_col) || !is.null(config$features)) {
    rv$pending_config <- config
    if (!is.null(config$feature_col)) {
      updateSelectizeInput(session, "feature_col", selected = config$feature_col)
    }
  }
  
  # Apply group filters - empty string means clear all
  if (!is.null(config$group_filters)) {
    if (is.character(config$group_filters) && config$group_filters == "") {
      rv$pending_group_filters <- list()
      rv$clear_group_filters <- TRUE
    } else {
      rv$pending_group_filters <- config$group_filters
    }
  }
  
  # Apply sample filtering - empty string means clear
  if (!is.null(config$sample_filter_rows)) {
    if (is.character(config$sample_filter_rows) && config$sample_filter_rows == "") {
      rv$pending_sample_filter <- integer(0)
      rv$clear_sample_filter <- TRUE
    } else {
      rv$pending_sample_filter <- config$sample_filter_rows
    }
  }
  
  # checkbox controls
  if (!is.null(config$expression_scale)) {
    shiny::updateCheckboxInput(session, "expression_scale", value = config$expression_scale)
  }
  if (!is.null(config$show_points)) {
    shiny::updateCheckboxInput(session, "show_points", value = config$show_points)
  }
}

#' Extract geyser config from SummarizedExperiment metadata
#'
#' @param rse SummarizedExperiment object
#'
#' @return List containing config or NULL if not found
#'
#' @importFrom S4Vectors metadata
#'
extract_config_from_metadata <- function(rse) {
  if (is.null(rse)) return(NULL)
  
  meta <- S4Vectors::metadata(rse)
  
  # Check for config in metadata under 'geyser_config' key
  if (!is.null(meta$geyser_config)) {
    return(meta$geyser_config)
  }
  
  return(NULL)
}

#' Apply embedded config from SummarizedExperiment if present
#'
#' @param rse SummarizedExperiment object
#' @param session Shiny session object
#' @param rv reactiveValues object
#'
#' @importFrom shiny showNotification
#'
apply_embedded_config <- function(rse, session, rv) {
  config <- extract_config_from_metadata(rse)
  
  if (!is.null(config)) {
    apply_config(config, session, rv)
    showNotification("Embedded configuration from dataset metadata applied.", 
                     type = "message", 
                     duration = 5)
    return(TRUE)
  }
  
  return(FALSE)
}

#' Create configuration download handler
#'
#' @param input Shiny input object
#'
#' @importFrom shiny downloadHandler
#' @import yaml
#'
create_config_download_handler <- function(input) {
  downloadHandler(
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
}

#' Setup configuration upload observer
#'
#' @param input Shiny input object
#' @param session Shiny session object
#' @param rv reactiveValues object
#'
#' @importFrom shiny observeEvent req showNotification
#' @import yaml
#'
setup_config_upload_observer <- function(input, session, rv) {
  observeEvent(input$config_upload, {
    req(input$config_upload)
    config <- yaml::read_yaml(input$config_upload$datapath)
    apply_config(config, session, rv)
    showNotification("External configuration applied.", type = "message")
  })
}

#' Setup configuration preset observer
#'
#' @param input Shiny input object
#' @param session Shiny session object
#' @param rv reactiveValues object
#'
#' @importFrom shiny observeEvent req showNotification
#' @import yaml
#'
setup_config_preset_observer <- function(input, session, rv) {
  observeEvent(input$config_preset, {
    req(input$config_preset != "")
    config <- yaml::read_yaml(file.path("presets", input$config_preset))
    apply_config(config, session, rv)
    showNotification(paste("Preset", input$config_preset, "applied."), type = "message")
  })
}

#' Apply pending group filters to dynamic UI
#'
#' @param rv reactiveValues object
#' @param input Shiny input object
#' @param session Shiny session object
#'
#' @importFrom shiny isolate updateSelectizeInput
#' @importFrom shinyjs delay
#'
apply_pending_group_filters <- function(rv, input, session) {
  if (!is.null(rv$pending_group_filters) || rv$clear_group_filters) {
    # Use a small delay to ensure the UI inputs have been created
    shinyjs::delay(200, {
      if (isolate(rv$clear_group_filters)) {
        # Clear all group filters
        for (group_name in isolate(input$groupings)) {
          updateSelectizeInput(session, paste0("dynamic_filter_", group_name), selected = character(0))
        }
        rv$clear_group_filters <- FALSE
      } else {
        # Apply specific filters from config
        for (group_name in names(isolate(rv$pending_group_filters))) {
          filter_values <- isolate(rv$pending_group_filters[[group_name]])
          if (group_name %in% isolate(input$groupings)) {
            updateSelectizeInput(session, paste0("dynamic_filter_", group_name), selected = filter_values)
          }
        }
        # Also clear any groupings that aren't in the pending filters
        for (group_name in isolate(input$groupings)) {
          if (!(group_name %in% names(isolate(rv$pending_group_filters)))) {
            updateSelectizeInput(session, paste0("dynamic_filter_", group_name), selected = character(0))
          }
        }
      }
      rv$pending_group_filters <- NULL
    })
  }
}

#' Apply pending sample filter to table
#'
#' @param rv reactiveValues object
#' @param input Shiny input object
#'
#' @importFrom shiny isolate req observe
#' @importFrom shinyjs delay
#' @importFrom DT dataTableProxy selectRows
#'
apply_pending_sample_filter <- function(rv, input) {
  observe({
    req(!is.null(rv$pending_sample_filter) || rv$clear_sample_filter)
    # Wait for table to be ready
    req(input$table_rows_all)
    
    shinyjs::delay(200, {
      proxy <- DT::dataTableProxy('table')
      
      if (isolate(rv$clear_sample_filter)) {
        proxy %>% DT::selectRows(NULL)
        rv$clear_sample_filter <- FALSE
      } else {
        rows_to_select <- isolate(rv$pending_sample_filter)
        if (length(rows_to_select) > 0) {
          proxy %>% DT::selectRows(rows_to_select)
        } else {
          proxy %>% DT::selectRows(NULL)
        }
      }
      rv$pending_sample_filter <- NULL
    })
  })
}