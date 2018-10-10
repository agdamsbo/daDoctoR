#' Forrest plot from ordinal logistic regression
#'
#' Heavily inspired by https://www.r-bloggers.com/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/
#' @param x ordinal logistic regression model.
#' @param title plot title
#' @param dec decimals for labels
#' @param lbls labels for variable names. Carefull, as the right order is not checked automatically!
#' @param short flag to half number of ticks on horizontal axis.
#' @keywords forestplot
#' @export
#' @examples
#' plot_ord_odds()

plot_ord_odds<-function(x, title = NULL,dec=3,lbls=NULL,short=FALSE){

  require(ggplot2)

  odds<-data.frame(cbind(exp(coef(x)), exp(confint(x))))
  names(odds)<-c("or", "lo", "up")
  rodds<-round(odds,digits = dec)

  if (!is.null(lbls)){
    odds$vars<-paste0(lbls," \n",paste0(rodds$or," [",rodds$lo,":",rodds$up,"]"))
  }
  else {
    odds$vars<-paste0(row.names(odds)," \n",paste0(rodds$or," [",rodds$lo,":",rodds$up,"]"))
  }

  ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))

  if (short==TRUE){
    ticks<-ticks[seq(1, length(ticks), 2)]
  }
  else {ticks<-ticks}

  odds$ord<-c(nrow(odds):1)

  ggplot(odds, aes(y= or, x = reorder(vars,ord))) +
    geom_point() +
    geom_errorbar(aes(ymin=lo, ymax=up), width=.2) +
    scale_y_log10(breaks=ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(title = title, x = "Variables", y = "OR (95 % CI)") +
    theme_bw()
}
