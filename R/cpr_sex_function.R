#' Determine sex from CPR
#'
#' Format "ddmmyy-xxxx"
#' @param x cpr as "ddmmyy-xxxx".
#' @keywords cpr
#' @export
#' @examples
#' cpr_sex("231045-0637")

cpr_sex<-function(x){
  ##Input as vector of DK cpr numbers, format "ddmmyy-xxxx", returns sex according to cpr

  last<-as.integer(substr(x, start = 11, stop = 11))
  sex<-ifelse(last %% 2 == 0, "female", "male")
  return(sex)
}
