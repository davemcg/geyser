# exPlotR

**EX**pression **PLOT**ter (in **R**)

Shiny-based app that uses the [SummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) data structure to flexibly display gene (or transcript) expression by user-led sample grouping(s).

## Quick Start
```
 # if needed: install.packages('remotes') 
remotes::install_github("davemcg/exPlotR")
library(exPlotR)
load('your_rse_object.Rdata')
exPlotR(your_rse_object)
```
