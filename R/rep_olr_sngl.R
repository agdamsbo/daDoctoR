#' A repeated ordinal logistic regression function for plotting
#'
#' Should be combined with "rep_olr()". For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param meas primary outcome (factor with >2 levels).
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param dta data frame to pull variables from.
#' @keywords olr
#' @export
#' @examples
#' rep_olr_sngl()


rep_olr_sngl<-function(meas,vars,data){
  require(MASS)

  ad<-vars
  d<-data
  mat<-matrix(ncol = 3)
  for (i in 1:length(ad)){
    d2<-d[,c(meas,ad[i])]
    names(d2)[1]<-"meas"
    x<-polr(meas~.,data = d2,Hess = TRUE)
    mat<-rbind(mat,c(exp(coef(x)), exp(confint(x))))
  }
  return(data.frame(mat[-1,]))
}
