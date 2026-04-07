# geyser

## Gene Expression displaY of SummarizedExperiment in R

Shiny-based app that uses the [SummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) data structure to flexibly plot gene (or transcript) expression by user-led sample grouping(s).

Solves the problem of  "uh, here is a bunch of RNAseq data and I just want to quickly look at some gene expression patterns across arbitrary sample metadata labels without writing code"

## Install
```
# if needed: install.packages('remotes') 
remotes::install_github("davemcg/geyser")
```

## Quick Start
Note: Use tiny included SE object just to confirm the app functions for you
```
library(geyser)
load(system.file('extdata/tiny_rse.Rdata', package = 'geyser'))
geyser(tiny_rse,'Test RSE')
```

## Version 1.0 Updates

  - More flexibility for file import
    - Can load RDS files from the GUI
    - Can give a folder when starting to show available RDS files (`geyser()) in a drop down menu
  - More sample filtering options 
  - More color palettes
  - Optional point labeling
  - Custom plot height
  - Can select which rowData slot to plot with (e.g. for RSE with ensembl or gene symbols)
    
## Use [recount3](https://www.bioconductor.org/packages/devel/bioc/vignettes/recount3/inst/doc/recount3-quickstart.html) to make a SE object
```
library(recount3)
library(geyser)
library(dplyr)
human_projects <- available_projects()
proj_info <- subset(
  human_projects,
  project == "SRP107937" & project_type == "data_sources"
)
rse_SRP107937 <- create_rse(proj_info)
assay(rse_SRP107937, "counts") <- transform_counts(rse_SRP107937)
# first tweak that glues the gene name onto the gene id in the row names
rownames(rse_SRP107937) <- paste0(rowData(rse_SRP107937)$gene_name, ' (', row.names(rse_SRP107937), ')')
# creates two new metadataa fields 
colData(rse_SRP107937)$tissue <- colData(rse_SRP107937)$sra.sample_title %>% stringr::str_extract(.,'PRC|PR')
colData(rse_SRP107937)$disease <- colData(rse_SRP107937)$sra.sample_title %>% stringr::str_extract(.,'AMD|Normal')
geyser::geyser(rse_SRP107937, " geyser: SRP107937")
```

## R Pseudocode to turn your count matrix and metadata into a SE object
```
# YOUR METADATA ROWS NEED TO MATCH YOUR COUNT MATRIX COLUMNS
# your_count_matrix is an actual matrix (not a tibble!) with the row.names set to the gene
library(SummarizedExperiment)
rse <- SummarizedExperiment(assays = list(counts = your_count_matrix), colData = your_metadata)
colnames(rse) <- your_metadata$sample_id
```

## Auto Config

You can embed a default configuration directly into your SummarizedExperiment (or SingleCellExperiment) object. 
When users load the .rds file into the geyser app, the tool will automatically read these settings, populate the dropdowns, and pre-filter the data.

```
# 1. Define all available configuration settings
# (all are optional)
default_geyser_config <- list(
  
  # --- Core Plotting Parameters ---
  groupings = c("disease", "treatment"),  # colData column(s) for the x-axis
  feature_col = "row names",              # "row names" or a specific rowData column
  features = c("TYRP1", "OPN1LW"),        # Default features (genes) to plot
  slot = "counts",                        # Assay to plot (e.g., "counts", "logcounts")
  
  # --- Aesthetics ---
  color_by = "tissue",                    # colData column to color points by
  color_palette = "okabe",                # Color palette (e.g., "okabe", "polychrome", "Set1")
  label_by = "sample_id",                 # colData column to label specific points
  
  # --- Custom Toggles  ---
  expression_scale = FALSE,               # Set to FALSE to uncheck 'log2(expression)'
  show_points = FALSE,                    # Set to FALSE to uncheck 'Plot individual points'
  
  # --- Pre-applied Data Filters (Optional) ---
  group_filters = list(                   # Pre-filter specific colData groups
    disease = c("AMD", "Normal") 
  ),
  sample_filter_rows = c(1, 2, 3, 4, 5)   # Pre-select specific row indices in the data table
)

# 2. Embed the configuration list into the metadata
S4Vectors::metadata(rse)$geyser_config <- default_geyser_config

# 3. Save the ready-to-use object
saveRDS(rse, "geyser_ready_dataset.rds")
```

## Related tools

The theme between these tools is that they do A LOT OF STUFF. **geyser** just does one thing - shows gene expression across your samples. Which, effectively, means less energy spent trying to figure out how to get started.

 - [iSEE](https://bioconductor.org/packages/release/bioc/html/iSEE.html)
 - [DEVis](https://cran.r-hub.io/web/packages/DEVis/vignettes/DEVis_vignette.html)
 - [omicsViewer](https://bioconductor.org/packages/devel/bioc/vignettes/omicsViewer/inst/doc/quickStart.html#1_Introduction)
