#' A repeated function 
#'
#' For bivariate analyses, for gating by p-value or change-in-estimate.
#' @param y Effect meassure.
#' @param v1 Main variable in model
#' @keywords logistic regression
#' @export
#' @examples
#' rep_biv()

rep_biv<-function(y,v1,string,data,method="pval",logistic=FALSE,ci=FALSE,cut=0.1,v2=NULL,v3=NULL){

require(rep_lm)
  require(rep_glm)
  require(cie_test)
  
if (method=="pval"&logistic==FALSE){
rep_lm(y=y,v1=v1,string=string,data=data,ci=ci)
}
if (method=="pval"&logistic==TRUE){
rep_lm(y=y,v1=v1,string=string,data=data,ci=ci)
}
if (method=="cie"){
cie_test(y=y,v1=v1,string=string,data=data,logistic=logistic,cut=cut,v2=v2,v3=v3)
}
return(df)
}
