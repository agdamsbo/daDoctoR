#' A repeated function for bivariate analyses
#'
#' For bivariate analyses, for gating by p-value or change-in-estimate.
#' @param y Effect meassure.
#' @param v1 Main variable in model
#' @param string String of columnnames to include. Obtained with dput(). Input as c().
#' @param data dataframe of data to analyse
#' @param method method of gating analysis. Can be "pval" for a simple analysis of p-values below or equal to 0.1 or "cie" for change in estimate to asses the change of the estimate when a second variable is added to the model.
#' @param logistic flag for logistic binomial regression or not (linear is then selected).
#' @keywords logistic regression
#' @export
#' @examples
#' rep_biv()

rep_biv<-function(y,v1,string,data,method="pval",logistic=FALSE,ci=FALSE,cut=0.1,v2=NULL,v3=NULL){

  a<-y
  b<-v1
  s<-string
  dat<-data
  me<-method
  log<-logistic
  CI<-ci
  ct<-cut

if (me=="pval"&log==FALSE){
daDoctoR::rep_lm(y=a,v1=b,string=s,data=dat,ci=CI)
}
if (me=="pval"&log==TRUE){
  daDoctoR::rep_lm(y=a,v1=b,string=s,data=dat,ci=CI)
}
if (method=="cie"){
  daDoctoR::cie_test(y=a,v1=b,string=s,data=dat,logistic=log,cut=ct)
}
return(df)
}
