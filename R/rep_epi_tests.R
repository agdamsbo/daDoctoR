#' A repeated epi.tests function
#'
#' Repeats the epi.tests from the epiR package. Either gs or test should be of length 1.
#' @description For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param gold the test or meassure used as "golden standard". Format as list of variable names to include. All variables should be formated as dichotomised factor.
#' @param test possible predictive tests to evaluate. Format as list of variable names to include. All variables should be formated as dichotomised factor.
#' @param data dataframe to draw variables from.
#' @keywords ppv npv sensitivity specificity
#' @export
#' @examples
#' rep_epi_tests()


rep_epi_tests<-function(gold,test,data){
  require(epiR)

  d<-data
  tst<-d[,c(test)]

  gs<-d[,c(gold)]

  ls<-list()

if (length(gold)==1){
  for (i in 1:ncol(tst)){
    t<-table(tst[,i],gs)
    rval <- epi.tests(t, conf.level = 0.95)
    n<-names(tst)[i]
    ls[[i]]<-list(n,rval)
  }}
else {
  for (i in 1:ncol(gs)){
    t<-table(tst,gs[,i])
    rval <- epi.tests(t, conf.level = 0.95)
    n<-names(gs)[i]
    ls[[i]]<-list(n,rval)
  }
}
  return(ls)
}
