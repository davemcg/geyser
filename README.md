# geyser

## Gene Expression displaY of SummarizedExperiment in R

Shiny-based app that uses the [SummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) data structure to flexibly plot gene (or transcript) expression by user-led sample grouping(s).

Solves the problem of "uh, here is a bunch of RNAseq data and I just want to quickly look at some gene expression patterns across arbitrary sample metadata labels"

[](https://github.com/davemcg/geyser/assets/10225430/3fe874fb-97fb-4bda-8dcf-d1d7505b3eae)

## Install
```
# if needed: install.packages('remotes') 
remotes::install_github("rstudio/bslib") # unnecessary soon as 0.5.0 submitted to cran
remotes::install_github("davemcg/geyser")
```

## Quick Start
Note: only quick if you already have a SE object. See below if you want to quickly make a SE.
```
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

## related tools

The theme between these tools is that they do A LOT OF STUFF. geyser just does one thing - shows gene expression across your samples. Which, effectively, means less energy spent trying to figure out how to get started.

 - [iSEE](https://bioconductor.org/packages/release/bioc/html/iSEE.html)
   - in short, geyser is a WAAAAAY LESS POWERFUL version of iSEE
 - [DEVis](https://cran.r-hub.io/web/packages/DEVis/vignettes/DEVis_vignette.html)
 - [omicsViewer](https://bioconductor.org/packages/devel/bioc/vignettes/omicsViewer/inst/doc/quickStart.html#1_Introduction)
