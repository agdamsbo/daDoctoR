#' For collection of datapoints for bivariate ordinal logistic regression plotting.
#'
#' Use with plot_ord_odds(), model="df".
#' @param meas outcome meassure variable name or response in data-data.frame as a string. Should be factor, preferably ordered.
#' @param vars variables to compare against. As vector of columnnames.
#' @param data dataframe of data.
#' @keywords olr
#' @export

biv_olr_plot_col<-function(meas,vars,data){
  d <- data
  x <- data.frame(d[, c(ad)])
  y <- d[, c(meas)]
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
  return(odds[-1,])
}
