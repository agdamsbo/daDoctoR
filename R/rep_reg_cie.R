#' A repeated regression function for change-in-estimate analysis
#'
#' For bivariate analyses, binary logistic or linear regression. From "Modeling and variable selection in epidemiologic analysis." - S. Greenland, 1989.
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param string variables to test. Input as c() of columnnames, use dput().
#' @param data data frame to pull variables from.
#' @param logistic flag to set logistic (TRUE) or linear (FALSE,standard) analysis.
#' @param cut cut value for gating if including or dropping the tested variable. As suggested bu S. Greenland (1989).
#' @keywords estimate-in-estimate
#' @export

rep_reg_cie<-function(meas,vars,string,data,cut=0.1){

  require(broom)

  d<-data
  x<-data.frame(d[,c(string)])
  v<-data.frame(d[,c(vars)])
  names(v)<-c(vars)
  y<-d[,c(meas)]
  dt<-cbind(y,v)

  c<-as.numeric(cut)

  if(!is.factor(y)){

    meth<-"linear regression"

  e<-as.numeric(round(coef(lm(y~.,data = dt)),3))[1]
       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){
    dat<-cbind(dt,x[,i])
    m<-lm(y~.,data=dat)

     b<-as.numeric(round(coef(m),3))[1]

     pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b)) }

       di<-as.vector(round(abs(e-as.numeric(df[-1,2]))/e,3))
       dif<-c(NA,di)
       t<-c(NA,ifelse(di>=c,"include","drop"))
       r<-cbind(df,dif,t) }

if(is.factor(y)){
  meth="logistic regression"

  e<-as.numeric(round(exp(coef(glm(y~.,family=binomial(),data=dt))),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){
    dat<-cbind(dt,x[,i])
    m<-glm(y~.,family=binomial(),data=dat)

     b<-as.numeric(round(exp(coef(m)),3))[1]

     pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b)) }

       di<-as.vector(round(abs(e-as.numeric(df[-1,2]))/e,3))
       dif<-c(NA,di)
       t<-c(NA,ifelse(di>=c,"include","drop"))
       r<-cbind(df,dif,t)
}
    return(list("method"=meth,"analyses"=r))
}
