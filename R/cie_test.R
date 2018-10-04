#' A repeated regression function for change-in-estimate analysis
#'
#' For bivariate analyses. From "Modeling and variable selection in epidemiologic analysis." - S. Greenland, 1989.
#' @param y Effect meassure.
#' @param v1 Main variable in model
#' @param string String of columnnames from dataframe to include. Use dput().
#' @keywords change-in-estimate
#'
#' @examples
#' l<-5
#' y<-factor(rep(c("a","b"),l))
#' x<-rnorm(length(y), mean=50, sd=10)
#' v1<-factor(rep(c("r","s"),length(y)/2))
#' v2<-as.numeric(sample(1:100, length(y), replace=FALSE))
#' v3<-as.numeric(1:length(y))
#' d<-data.frame(y,x,v1,v2,v3)
#' preds<-dput(names(d)[3:ncol(d)])
#' cie_test(meas="y",vars="x",string=preds,data=d,logistic = TRUE,cut = 0.1)
#'
#' @export
#'

cie_test<-function(meas,vars,string,data,logistic=FALSE,cut=0.1){

  require(broom)

  d<-data
  x<-data.frame(d[,c(string)])
  v<-data.frame(d[,c(vars)])
  names(v)<-c(vars)
  y<-d[,c(meas)]
  dt<-cbind(y,v)

  c<-as.numeric(cut)

  if(logistic==FALSE){

if (is.factor(y)){stop("Logistic is flagged as FALSE, but the provided meassure is formatted as a factor!")}

  e<-as.numeric(round(coef(lm(y~.,data = dt)),3))[1]
       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){
    dat<-cbind(dt,x[,i])
    m<-lm(y~.,data=dat)

     b<-as.numeric(round(coef(m),3))[1]

     pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b)) }

       di<-as.vector(abs(e-as.numeric(df[-1,2]))/e)
       dif<-c(NA,di)
       t<-c(NA,ifelse(di>=c,"include","drop"))
       r<-cbind(df,dif,t) }

if(logistic==TRUE){

if (!is.factor(y)){stop("Logistic is flagged as TRUE, but the provided meassure is NOT formatted as a factor!")}

  e<-as.numeric(round(exp(coef(glm(y~.,family=binomial(),data=dt))),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){
    dat<-cbind(dt,x[,i])
    m<-glm(y~.,family=binomial(),data=dat)

     b<-as.numeric(round(exp(coef(m)),3))[1]

     pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b)) }

       di<-as.vector(abs(e-as.numeric(df[-1,2]))/e)
       dif<-c(NA,di)
       t<-c(NA,ifelse(di>=c,"include","drop"))
       r<-cbind(df,dif,t)
}
    return(r)
}
