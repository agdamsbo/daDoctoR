#' Forrest plot from ordinal logistic regression, version2.
#'
#' Heavily inspired by https://www.r-bloggers.com/plotting-odds-ratios-aka-a-forrestplot-with-ggplot2/
#' @param meas outcome meassure variable name or response in data-data.frame as a string. Should be factor, preferably ordered.
#' @param vars variables to compare against. As vector of columnnames.
#' @param data dataframe of data.
#' @param title plot title
#' @param dec decimals for labels
#' @param lbls labels for variable names. Carefull, as the right order is not checked automatically!
#' @param hori labels the horizontal axis (this i the y axis as the plot is rotated)
#' @param vert labels the horizontal axis (this i the x axis as the plot is rotated)
#' @param short flag to half number of ticks on horizontal axis.
#' @param analysis can be either "biv", or "multi", for creation of forest plot from either bivariate (unadjusted) or multivariate (adjusted) ordinal logistic regression.
#' @keywords forestplot
#' @export

plot_ord_odds_v2<-function(meas,vars,data, title = NULL,dec=3,lbls=NULL,hori="OR (95 % CI)",vert="Variables",short=FALSE,analysis=c("biv","multi")){

  require(ggplot2)

  d <- data
  x <- data.frame(d[, c(ad)])
  y <- d[, c(meas)]

  if (analysis=="biv"){

    dt <- cbind(y, x)
    odds<-c(matrix(ncol = 3))
    nms<-c("or","lo","hi")
    for (i in 1:ncol(x)) {
      dat <- data.frame(y = y, x[, i])
      m <- polr(y ~ ., data = dat, Hess = TRUE)

      mat<-suppressMessages(matrix(c(exp(coef(m)), exp(confint(m))),ncol=3,byrow=FALSE))
      colnames(mat)<-nms

      odd <- data.frame(mat)
      odds<-rbind(odds,odd)
    }

  }

  if (analysis=="multi"){

    m<-polr(y~.,data = dta2,Hess = TRUE)
    odds<-data.frame(cbind(exp(coef(m)), exp(confint(m))))
  }
  names(odds)<-c("or", "lo", "up")
  rodds<-round(odds,digits = dec)

  if (!is.null(lbls)){
    odds$vars<-paste0(lbls," \n",paste0(rodds$or," [",rodds$lo,":",rodds$up,"]"))
  }
  else {
    odds$vars<-paste0(row.names(odds)," \n",paste0(rodds$or," [",rodds$lo,":",rodds$up,"]"))
  }

  ticks<-c(seq(0, 1, by =.1), seq(1, 10, by =1), seq(10, 100, by =10))

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
    labs(title = title, x = vert, y = hori) +
    theme_bw()
}
