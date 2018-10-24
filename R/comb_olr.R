#' An ordinal logistic regression function for plotting
#'
#' Should be combined with "rep_olr()". The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param meas primary outcome (factor with >2 levels).
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param dta data frame to pull variables from.
#' @keywords olr
#' @export
#' @examples
#' comb_olr()


comb_olr<-function(meas,vars,data){
  require(MASS)

  ad<-vars
  d<-data
    d2<-d[,c(meas,ad)]
    names(d2)[1]<-"meas"
    x<-polr(meas~.,data = d2,Hess = TRUE)
    mat<-cbind(c(exp(coef(x)), exp(confint(x))))
  return(data.frame(mat,stringsAsFactors = FALSE))
}
