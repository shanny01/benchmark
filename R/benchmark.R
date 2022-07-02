#' Returns the precision-recall curves and precision-recall values of benchmark analysis using CITEdb
#'
#' @param assess text files with predicted cell-cell interactions
#' @param directed whether the predicted cell-cell interactions is directed or not
#'
#' @return the precision-recall curves and precision-recall values
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' library(yardstick)
#' library(benchmark)
#' #load example data with directed cell-cell interactions
#' data('Default.sum')
#' #benchmark one algorithm
#' benchmark(assess = Default.sum[[1]],directed = T)
#'
#' #benchmark multiple algorithms at once
#' input = do.call(rbind,Default.sum)
#' benchmark(assess = input,directed = T)
#'
#' #load example data with undirected cell-cell interactions
#' data('Bray.Curtis.score_Enrichment.score')
#' #benchmark multiple algorithms at once
#' input = do.call(rbind,Bray.Curtis.score_Enrichment.score)
#' benchmark(assess = input,directed = F)
benchmark <- function(assess,directed){
  CITEdb.1 = CITEdb[CITEdb$Information==1,]

  if(directed == T){
    CITEdb.2 = CITEdb.1[CITEdb.1$Method=='experimental' & CITEdb.1$Clear.direction==1,]
    cc.reference = paste(CITEdb.2$Source.cell.type.class,CITEdb.2$Target.cell.type.class)

    CITEdb.2.reciprocal = CITEdb.2[CITEdb.2$Reciprocal.direction==1,]
    cc.reference.reciprocal = paste(CITEdb.2.reciprocal$Target.cell.type.class,CITEdb.2.reciprocal$Source.cell.type.class)

    cc.reference.all = c(cc.reference,cc.reference.reciprocal)
  }else{
    CITEdb.2 = CITEdb.1[CITEdb.1$Method=='experimental',]
    cc.reference.1 = paste(CITEdb.2$Source.cell.type.class,CITEdb.2$Target.cell.type.class)
    cc.reference.2 = paste(CITEdb.2$Target.cell.type.class,CITEdb.2$Source.cell.type.class)
    cc.reference.all = c(cc.reference.1,cc.reference.2)
  }

  cc = paste(assess[,2],assess[,3])
  truth = ifelse(cc%in%cc.reference.all,'Class1','Class2')

  input = data.frame(assess,truth = truth)

  input$truth = as.factor(input$truth)
  input$weight = as.numeric(input$weight)

  auc = input %>%
    group_by(Method) %>%
    pr_auc(truth, weight)
  auc3 = sprintf('%0.3f',auc$.estimate)

  pp = input %>%
    group_by(Method) %>%
    pr_curve(truth, weight) %>%
    ggplot(aes(x = recall, y = precision))+
    geom_path(aes(colour=Method))+
    coord_equal() +
    theme_bw()+
    coord_cartesian(ylim = c(0,1.0))+
    labs(colour='Method (AUC)')+
    scale_colour_manual(values = rainbow(length(unique(input$Method))))+
    scale_colour_discrete(labels=paste(auc$Method,' (',auc3,')',sep = ''))

  return(pp)
}
