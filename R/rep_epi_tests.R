#' A repeated epi.tests function
#'
#' Repeats the epi.tests from the epiR package.
#' @description For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param gs the test or meassure used as "golden standard". Format as dichotomised factor.
#' @param test possible predictive tests to evaluate. Format as dichotomised factor.
#' @keywords ppv npv sensitivity specificity
#' @export
#' @examples
#' rep_epi_tests()


rep_epi_tests<-function(gs,test){
  require(epiR)

  ls<-list()
  for (i in 1:ncol(test)){
    t<-table(test[,i],gs)
    rval <- epi.tests(t, conf.level = 0.95)
    n<-names(test)[i]
    ls[[i]]<-list(n,rval)
  }
  return(ls)
}
