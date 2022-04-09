library(ggplot2)
library(dplyr)
library(yardstick)
library(benchmark)

## load example data
## Cell–cell interactions in human metastatic melanoma predicted by CellChat, CellPhoneDB, Connectome, iTALK, NATMI,  and SingleCellSignalR using the default LR resources based on the sum of ligand-receptor scores
data('Default.sum')

## benchmark one algorithm
benchmark(asses=Default.sum[[1]])

##benchmark multiple algorithms at once
input = do.call(rbind,Default.sum)
benchmark(asses=input)
