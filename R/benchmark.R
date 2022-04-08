#' Returns the precision-recall curves and tables of precision-recall values of benchmark analysis using CITEdb
#'
#' @param assess text files with predicted cell-cell interactions
#'
#' @return the precision-recall curves and tables of precision-recall values
#' @export
#'
#' @examples
#' library(benchmark)
#' #load example data
#' load('Default.sum.RData')
#' #benchmark one algorithm
#' benchmark(Default.sum[[6]])
#'
#' #benchmark multiple algorithms at once
#' input = do.call(rbind,Default.sum)
#' benchmark(asses=input)
benchmark <- function(assess){
  citedb = read.delim('https://citedb.cn/CITEdb.txt')
  citedb.1 = citedb[citedb$Information==1,]
  citedb.2 = citedb.1[citedb.1$Method=='experimental' & citedb.1$Clear.direction==1,]
  cc.reference = paste(citedb.2$Source.cell.type.class,citedb.2$Target.cell.type.class)

  cc = paste(assess[,2],assess[,3])
  truth = ifelse(cc%in%cc.reference,'Class1','Class2')

  input = data.frame(assess,truth = truth)

  input$truth = as.factor(input$truth)
  input$weight = as.numeric(input$weight)

  #pr_curve() constructs the full precision recall curve and returns a tibble.

  curve = input %>%
    group_by(Method) %>%
    pr_curve(truth, weight) %>%
    autoplot()

  #pr_auc() is a metric that computes the area under the precision recall curve.
  auc = input %>%
    group_by(Method) %>%
    pr_auc(truth, weight)

  return(list(curve,auc))
}

