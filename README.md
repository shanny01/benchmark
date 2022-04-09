
<!-- README.md is generated from README.Rmd. Please edit that file -->
# benchmark

<!-- badges: start -->
<!-- badges: end -->
CITEdb(<https://citedb.cn/#/>), a cell-cell interaction database at cellular level that has been manually curated from literatures, provides an initial benchmark dataset when interpreting and evaluating the computational results of cell-cell interactions derived from different tools. benchmark is provided for automatic benchmark analysis using CITEdb.

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

You can follow the tutorial below to get started using benchmark for the bemchmark analysis of the prediction algorithms.

The input of the package is text files with predicted cell-cell interactions and the outputs are precision-recall curves and tables of precision-recall values. Required data and formats can be prepared as follows:

1: return a list of predicted ligand-receptor interactions with its own format and significance assessment by the prediction algorithms;

2: summarize the results using the sum of ligand-receptor scores, the number of significant ligand-receptor pairs, or other modes as the weight of cell-cell interactions;

3: unify the coding of cell types in Step 2 to cell type class in CITEdb;

4: represent the interaction data as a four-column table, with Method, Source cell type class, Target cell type class, and weight (as in the following example):

|       Method      | Source.cell.type.class | Target.cell.type.class |   weight  |
|:-----------------:|:----------------------:|:----------------------:|:---------:|
| SingleCellSignalR |         B cell         |       macrophage       |  0.903825 |
| SingleCellSignalR |       fibroblast       |         B cell         |  1.832844 |
| SingleCellSignalR |       fibroblast       |       fibroblast       | 27.396210 |

## Example

This is a basic example which shows you how to use benchmark or you can follow the example\_code.R in the example directory of the package:

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

## load example data
data('Default.sum')

## benchmark one algorithm
benchmark(asses=Default.sum[[1]])
#> [[1]]
```

<img src="man/figures/README-example-1.png" width="100%" />

    #> 
    #> [[2]]
    #> # A tibble: 1 × 4
    #>   Method   .metric .estimator .estimate
    #>   <chr>    <chr>   <chr>          <dbl>
    #> 1 CellChat pr_auc  binary         0.624

    ##benchmark multiple algorithms at once
    input = do.call(rbind,Default.sum)
    benchmark(asses=input)
    #> [[1]]

<img src="man/figures/README-example-2.png" width="100%" />

    #> 
    #> [[2]]
    #> # A tibble: 6 × 4
    #>   Method            .metric .estimator .estimate
    #>   <chr>             <chr>   <chr>          <dbl>
    #> 1 CellChat          pr_auc  binary         0.624
    #> 2 CellPhoneDB       pr_auc  binary         0.751
    #> 3 Connectome        pr_auc  binary         0.749
    #> 4 iTALK             pr_auc  binary         0.642
    #> 5 NATMI             pr_auc  binary         0.801
    #> 6 SingleCellSignalR pr_auc  binary         0.845
