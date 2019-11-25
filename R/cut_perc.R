#' Creates factor by percentile from numeric vector.
#'
#' Cuts numeric vector in equal groups set by percentile.
#' Export is af factor with numeric labels.
#' @param vec name of vector
#' @param percentile percentile for cuts. No check is made, sholud be a divisor of 1 as .25 og .1. Standard is .1 for centiles.
#' @keywords percentile
#' @export
#' @examples
#'   ##Tests the function by quantiles
#'   cut_perc(sample(1:100, 50, replace=TRUE),.25)

cut_perc<-function (vec,percentile=.1)
{
  p=percentile
  v=as.numeric(vec)

  perc <- factor(
    findInterval(v, c(-Inf,
                      quantile(v, probs=seq(from=p,to=1-p,by=p), na.rm=TRUE), Inf)),
    labels=c(1:(1/p)
    ))
  return(perc)
}

