# geyser

Gene Expression Visualizer of SummarizedExperiment in R

*(the `v` is a `y` because gevser is not a word and geyser is and `v` is SO close to a `y`)*

Shiny-based app that uses the [SummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) data structure to flexibly display gene (or transcript) expression by user-led sample grouping(s).

## Quick Start
```
 # if needed: install.packages('remotes') 
remotes::install_github("davemcg/geyser")
library(geyser)
load('your_se_object.Rdata')
geyser(your_se_object)
```

## Use [recount3](https://www.bioconductor.org/packages/devel/bioc/vignettes/recount3/inst/doc/recount3-quickstart.html) to make a SE object
```
library(recount3)
human_projects <- available_projects()
proj_info <- subset(
    human_projects,
    project == "SRP009615" & project_type == "data_sources"
)
rse_gene_SRP009615 <- create_rse(proj_info)
assay(rse_gene_SRP009615, "counts") <- transform_counts(rse_gene_SRP009615)
# one tweak that glues the gene name onto the gene id in the row names
rownames(rse_gene_SRP009615) <- paste0(rowData(rse_gene_SRP009615)$gene_name, ' (', row.names(rse_gene_SRP009615), ')')
geyser(rse_gene_SRP009615, "SRP009615")
```
