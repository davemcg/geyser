# primary = "#3A5836", secondary = "#d5673e"
theme_ui <- function(primary_color, secondary_color){
  bs_theme(version =  5,
           primary = primary_color,
           secondary = secondary_color,
           font_scale = 0.8,
           bootswatch = 'united',
           "accordion-button-active-bg" = primary_color,
           "accordion-button-bg" = primary_color,
           "accordion-button-color" = "white",
           "accordion-button-active-color" = "white",
           "accordion-icon-color" = "white",
           "accordion-icon-active-color" = "white",
           "dark" = primary_color)
}

quick_start_ui <- function(){
  nav_panel("Quick Start",
            page_fluid(
              "Code that you can paste into your R console that does the following: ", br(),
              "1. Load recount3 and download project SRP107937", br(),
              "2. Extract the raw counts", br(),
              "3. Does some light metadata tweaking and loads the RSE object into geyser", br(), br(),
              tags$pre("# If needed: BiocManager::install(\"recount3\")
library(recount3)
library(geyser)
library(dplyr)
human_projects <- available_projects()
proj_info <- subset( 
  human_projects,
  project == \"SRP107937\" & project_type == \"data_sources\" 
)
rse_SRP107937 <- create_rse(proj_info)
assay(rse_SRP107937, \"counts\") <- transform_counts(rse_SRP107937)
# first tweak that glues the gene name onto the gene id in the row names
rownames(rse_SRP107937) <- paste0(rowData(rse_SRP107937)$gene_name, ' (', row.names(rse_SRP107937), ')')
# creates two new metadata fields 
colData(rse_SRP107937)$tissue <- colData(rse_SRP107937)$sra.sample_title %>% stringr::str_extract(.,'PRC|PR')
colData(rse_SRP107937)$disease <- colData(rse_SRP107937)$sra.sample_title %>% stringr::str_extract(.,'AMD|Normal')
geyser(rse_SRP107937, \" geyser: SRP107937\")"), br()
            #           tags$iframe(width="800",
            #                       height="506",
            #                       src="https://rawcdn.githack.com/davemcg/geyser/5e89c4ac27e786eb80ec1baa0bf0ab1f90fb6f01/inst/assets/geyser_example.mp4", frameborder="0", allow="accelerometer; autoplay; gyroscope; picture-in-picture"))
            )
  )
}

overview_ui <- function(){
  nav_panel("Overview",
            tags$h3("Start with the full metadata"),
            img(src = 'assets/help_01.png', width = "600px"),
            "The idea is to figure out which columns to use for the plotting",
            tags$h3("Plotting"),
            img(src = "assets/help_02.png", width = "600px"),
            "Then go to the plotting section and pick the columns (\"groupings\")",
            tags$h3("Genes"),
            img(src = "assets/help_03.png", width = "600px"),
            "Now pick the genes",
            tags$h3("Plot!"),
            img(src = "assets/help_04.png", width = "600px"),
            "Click the plot button to generate your plot of gene by custom grouping",
            tags$h3("Custom filtering"),
            img(src = "assets/help_05.png", width = "600px"),
            "You can select rows of the table (circle) to only plot these (arrow row NOT selected). You have to click the plot button again to regenerate the new, filtered, plot. The filtering is applied to all plots.",
            img(src = "assets/help_06.png", width = "200px"),
            "Click the \"Clear Rows\" button to reset your sample filtering"
  )
}
