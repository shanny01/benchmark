library(ggplot2)
library(dplyr)
library(yardstick)
library(benchmark)

## load example data with directed cell-cell interactions
## Cell–cell interactions in human metastatic melanoma predicted by CellChat, CellPhoneDB, Connectome, iTALK, NATMI,  and SingleCellSignalR using the default LR resources based on the sum of ligand-receptor scores.
data('Default.sum')

## benchmark one algorithm
benchmark(assess = Default.sum[[1]],directed = T)

## benchmark multiple algorithms at once
input = do.call(rbind,Default.sum)
benchmark(assess = input,directed = T)

## load example data with undirected cell-cell interactions
## #' Cell–cell interactions in human metastatic melanoma based on Bray-Curtis score and the enrichment score using the concensus LR resource in LIANA.
data('Bray.Curtis.score_Enrichment.score')

## benchmark multiple algorithms at once
input = do.call(rbind,Bray.Curtis.score_Enrichment.score)
benchmark(assess = input,directed = F)

