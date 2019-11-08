#' A repeated ordinal logistic regression function
#'
#' For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param string variables to test. Input as c() of columnnames, use dput().
#' @param ci flag to get results as OR with 95% confidence interval.
#' @param data data frame to pull variables from.
#' @keywords olr
#' @export

rep_olr<-function(meas,vars,string,ci=FALSE,data){

  require(broom)
  require(MASS)

  d<-data
  x<-data.frame(d[,c(string)])
  v<-data.frame(d[,c(vars)])
  names(v)<-c(vars)
  y<-d[,c(meas)]
  dt<-cbind(y,v)
  m1<-length(coef(polr(y~.,data = dt,Hess=TRUE)))

  if (!is.factor(y)){stop("y should be a factor!")}

  if (ci==TRUE){

    df<-data.frame(matrix(ncol = 3))
    names(df)<-c("pred","or_ci","pv")

    for(i in 1:ncol(x)){
      dat<-cbind(dt,x[,i])
      m<-polr(y~.,data=dat,Hess=TRUE)

      ctable <- coef(summary(m))

      l<-suppressMessages(round(exp(confint(m))[-c(1:m1),1],2))
      u<-suppressMessages(round(exp(confint(m))[-c(1:m1),2],2))
      or<-round(exp(coef(m))[-c(1:m1)],2)
      or_ci<-paste0(or," (",l," to ",u,")")

      p <- (pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2)[1:length(coef(m))]
      pv<-round(p[-c(1:m1)],3)


      x1<-x[,i]

      if (is.factor(x1)){
        pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")}

      else {pred<-names(x)[i]}

      df<-rbind(df,cbind(pred,or_ci,pv))
    }}

  if (ci==FALSE){

    df<-data.frame(matrix(ncol = 3))
    names(df)<-c("pred","b","pv")

    for(i in 1:ncol(x)){
      dat<-cbind(dt,x[,i])
      m<-polr(y~.,data=dat,Hess=TRUE)

      ctable <- coef(summary(m))

      b<-round(coef(m)[-c(1:m1)],2)

      p <- (pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2)[1:length(coef(m))]
      pv<-round(p[-c(1:m1)],3)

      x1<-x[,i]

      if (is.factor(x1)){
        pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")
      }

      else {pred<-names(x)[i]}

      df<-rbind(df,cbind(pred,b,pv))

    }}

  pa<-as.numeric(df[,c("pv")])
  t <- ifelse(pa<=0.1,"include","drop")
  pa<-ifelse(pa<0.001,"<0.001",pa)
  pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
               ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

  r<-data.frame(df[,1:2],pa,t)[-1,]

  return(r)
}




