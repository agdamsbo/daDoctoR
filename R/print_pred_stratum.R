#' Extension to the print_pred function, for by stratum analysis.
#'
#' Outputs list of results from 'print_pred' for the whole data set and for each stratum defined by 'strat'.
#' Suitable to assist in determining whether a variable is a confounder or effect modifier.
#' Ref: https://open.oregonstate.education/epidemiology/chapter/effect-modification/
#' @param meas binary outcome measure variable, column name in data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param adj variables to adjust for, as string.
#' @param strat stratum to stratify, variable name as string
#' @param data dataframe of data.
#' @param include.stratum flag to set if stratum variable should be included in first analysis of non-stratified data.
#' @keywords stratum
#' @export
#' @examples
#'   data('mtcars')
#'   mtcars$vs<-factor(mtcars$vs)
#'   mtcars$am<-factor(mtcars$am)
#'   print_pred_stratum(meas="mpg",strat="vs",adj=c("disp","wt","am"),data=mtcars,include.stratum=TRUE)

print_pred_stratum<-function(meas,adj,strat,data,dec,include.stratum=TRUE){
  require(daDoctoR)
  require(dplyr)

  if (include.stratum==TRUE){
    ls<-list(all=print_pred(meas = meas,adj=c(strat,adj),data=data))
  }
  if (include.stratum==FALSE) {
    ls<-list(all=print_pred(meas = meas,adj=adj,data=data))
  }

  strt<-data[, c(strat)]
  for (i in 1:length(levels(factor(strt)))){
    d_str<-data[data[[strat]]==levels(data[[strat]])[i], c(meas,adj)]
    ls_str<-list(print_pred(meas = meas,adj=adj,data=d_str))
    names(ls_str)<-levels(factor(strt))[i]
    ls<-append(ls,ls_str)
  }
  return(ls)
}
