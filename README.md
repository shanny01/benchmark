
<!-- README.md is generated from README.Rmd. Please edit that file -->
# benchmark

<!-- badges: start -->
<!-- badges: end -->
CITEdb(<https://citedb.cn/#/>), a cell-cell interaction database at cellular level that has been manually curated from literatures, provides an initial benchmark dataset when interpreting and evaluating the computational results of cell-cell interactions derived from different tools. benchmark is developed to enjoy great flexibility by allowing automatic benchmark analysis of directed and undirected cell-cell interactions using CITEdb.

<font size=4>How to cite:</font>

<font size=4>Contact:</font> Nayang Shan \[<shanny01@foxmail.com>\]

## Installation

To use benchmark, the following R packages are required: yardstick, ggplot2, dplyr

To install benchmark, run the following command:

``` r
if(!require(devtools)) install.packages("devtools");
devtools::install_github("shanny01/benchmark")
```

## Tutorial

You can follow the tutorial below to get started using benchmark for the benchmark analysis of the prediction algorithms.

The input of the package is text files with predicted cell-cell interactions and the outputs are precision-recall curves and precision-recall values. Required data and formats can be prepared as follows:

1: return a list of predicted ligand-receptor interactions with its own format and significance assessment by the prediction algorithms;

2: summarize the results using the sum of ligand-receptor scores, the number of significant ligand-receptor pairs, or directly using Bray-Curtis score and the enrichment score as the overall weight of cell-cell interactions;

3: unify the coding of cell types in Step 2 to cell type class in CITEdb;

4: represent the interaction data as a four-column table, with Method, Source cell type class, Target cell type class, and weight (as in the following example):

|       Method      | Source.cell.type.class | Target.cell.type.class |   weight  |
|:-----------------:|:----------------------:|:----------------------:|:---------:|
| SingleCellSignalR |         B cell         |       macrophage       |  0.903825 |
| SingleCellSignalR |       fibroblast       |         B cell         |  1.832844 |
| SingleCellSignalR |       fibroblast       |       fibroblast       | 27.396210 |

You can follow the example\_code.R in the example directory of the package or the following example to use benchmark for your own datasets.

## Example

This is a basic example which shows you how to use benchmark:

``` r
library(ggplot2)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(yardstick)
#> For binary classification, the first factor level is assumed to be the event.
#> Use the argument `event_level = "second"` to alter this as needed.
library(benchmark)

data("CITEdb")

## load example data with directed cell-cell interactions
## Cell–cell interactions in human metastatic melanoma predicted by CellChat, CellPhoneDB, Connectome, iTALK, NATMI,  and SingleCellSignalR using the default LR resources based on the sum of ligand-receptor scores.
data('Default.sum')

## benchmark one algorithm
benchmark(assess = Default.sum[[1]],directed = T)
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
#> Scale for 'colour' is already present. Adding another scale for 'colour',
#> which will replace the existing scale.
```

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" />

``` r

##benchmark multiple algorithms at once
input = do.call(rbind,Default.sum)
benchmark(assess = input,directed = T)
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
#> Scale for 'colour' is already present. Adding another scale for 'colour',
#> which will replace the existing scale.
```

<img src="man/figures/README-unnamed-chunk-2-2.png" width="100%" />

``` r

## load example data with undirected cell-cell interactions
## Cell–cell interactions in human metastatic melanoma based on Bray-Curtis score and the enrichment score using the concensus LR resource in LIANA.
data('Bray.Curtis.score_Enrichment.score')

## benchmark multiple algorithms at once
input = do.call(rbind,Bray.Curtis.score_Enrichment.score)
benchmark(assess = input,directed = F)
#> Coordinate system already present. Adding new coordinate system, which will replace the existing one.
#> Scale for 'colour' is already present. Adding another scale for 'colour',
#> which will replace the existing scale.
```

<img src="man/figures/README-unnamed-chunk-2-3.png" width="100%" />
