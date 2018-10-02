#' CPR check
#'
#' Checking validity of cpr number
#' @param x cpr as "ddmmyy-xxxx".
#' @keywords cpr
#' @export
#' @examples
#' cpr_check()

cpr_check<-function(x){
  #Check validity of CPR number, format ddmmyy-xxxx
  
  p1<-as.integer(substr(x,1,1))
  p2<-as.integer(substr(x,2,2))
  p3<-as.integer(substr(x,3,3))
  p4<-as.integer(substr(x,4,4))
  p5<-as.integer(substr(x,5,5))
  p6<-as.integer(substr(x,6,6))
  p7<-as.integer(substr(x,8,8))
  p8<-as.integer(substr(x,9,9))
  p9<-as.integer(substr(x,10,10))
  p10<-as.integer(substr(x,11,11))
  
  result<-ifelse((p1*4+p2*3+p3*2+p4*7+p5*6+p6*5+p7*4+p8*3+p9*2+p10) %% 11 == 0,"valid","invalid")
  return(result)
}