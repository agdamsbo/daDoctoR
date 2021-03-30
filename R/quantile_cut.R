#' Easy function for splitting numeric variable in quantiles
#'
#' Using base/stats functions cut() and quantile().
#' @param x Variable to cut.
#' @param groups Number of groups.
#' @param na.rm Remove NA's. Default is TRUE.
#' @param group.names Names of groups to split to. Default is NULL, giving intervals as names.
#' @param ordered.f Set resulting vector as ordered. Default is FALSE.
#' @keywords quantile
#' @export
#' @examples
#' aa <- as.numeric(sample(1:1000,2000,replace = TRUE))
#' summary(quantile_cut(aa,groups=4)) ## Cuts quartiles

quantile_cut<-function(x,groups,na.rm=TRUE,group.names=NULL,ordered.f=FALSE){
  cut(x, quantile(x,probs = seq(0, 1, 1/groups), na.rm = na.rm,names = TRUE, type = 7),include.lowest = TRUE,labels = group.names,ordered_result = ordered.f)
}
