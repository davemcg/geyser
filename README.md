# geyser

Gene Expression Visualizer of SummarizedExperiment in R

*(and yes, the v is a y because gevser is not a word and geyser is and v is pretty close to a y)*

Shiny-based app that uses the [SummarizedExperiment](https://bioconductor.org/packages/release/bioc/html/SummarizedExperiment.html) data structure to flexibly display gene (or transcript) expression by user-led sample grouping(s).

## Quick Start
```
 # if needed: install.packages('remotes') 
remotes::install_github("davemcg/geyser")
library(geyser)
load('your_rse_object.Rdata')
exPlotR(your_rse_object)
```
