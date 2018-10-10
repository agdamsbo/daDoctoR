#' A repeated linear regression function
#'
#' For bivariate analyses, to determine which variables to include in adjusted model.
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param string variables to test. Input as c() of columnnames, use dput().
#' @param ci flag to get results as OR with 95% confidence interval.
#' @param data data frame to pull variables from.
#' @keywords linear
#' @export
#' @examples
#' rep_lm()

rep_lm<-function(meas,vars,string,ci=FALSE,data){

  require(broom)

  d<-data
  x<-data.frame(d[,c(string)])
  v<-data.frame(d[,c(vars)])

  y<-d[,c(meas)]
  dt<-cbind(y=y,v)
  m1<-length(coef(lm(y~.,data = dt)))

  names(v)<-c(vars)

  if (is.factor(y)){stop("y should not be a factor!")}

  if (ci==TRUE){

    df<-data.frame(matrix(NA,ncol = 3))
    names(df)<-c("pred","or_ci","pv")

    for(i in 1:ncol(x)){
      dat<-cbind(dt,x[,i])
      m<-lm(y~.,data=dat)

      ci<-suppressMessages(confint(m))
      l<-round(ci[-c(1:m1),1],2)
      u<-round(ci[-c(1:m1),2],2)
      or<-round(coef(m)[-c(1:m1)],2)
      or_ci<-paste0(or," (",l," to ",u,")")
      pv<-round(tidy(m)$p.value[-c(1:m1)],3)
      x1<-x[,i]

      if (is.factor(x1)){
        pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")}

      else {pred<-names(x)[i]}

      df<-rbind(df,cbind(pred,or_ci,pv))}}

  else {

    df<-data.frame(matrix(NA,ncol = 3))
    names(df)<-c("pred","b","pv")

    for(i in 1:ncol(x)){
      dat<-cbind(dt,x[,i])

      m<-lm(y~.,data=dat)
      b<-round(coef(m)[-c(1:m1)],3)
      pv<-round(tidy(m)$p.value[-c(1:m1)],3)
      x1<-x[,i]

      if (is.factor(x1)){
        pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")
      }
      else {pred<-names(x)[i]}
      df<-rbind(df,cbind(pred,b,pv))
    }}

  pa<-as.numeric(df[,3])
  t <- ifelse(pa<=0.1,"include","drop")
  pa<-ifelse(pa<0.001,"<0.001",pa)
  pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
               ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

  r<-data.frame(df[,1:2],pa,t)[-1,]

  return(r)
}
