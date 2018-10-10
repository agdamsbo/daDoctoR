#' A repeated logistic regression function
#'
#' For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param str variables to test. Input as c() of columnnames, use dput().
#' @param ci flag to get results as OR with 95% confidence interval.
#' @param dta data frame to pull variables from.
#' @keywords logistic
#' @export
#' @examples
#'   l<-50
#'   y<-factor(rep(c("a","b"),l))
#'   x<-rnorm(length(y), mean=50, sd=10)
#'   v1<-factor(rep(c("r","s"),length(y)/2))
#'   v2<-sample(1:100, length(y), replace=FALSE)
#'   v3<-as.numeric(1:length(y))
#'   d<-data.frame(y,x,v1,v2,v3)
#'   preds<-c("v1","v2","x")
#'   rep_glm(meas="y",vars="v3",string=preds,ci=F,data=d)


rep_glm<-function(meas,vars,string,ci=FALSE,data){

  require(broom)

  d<-data
  x<-data.frame(d[,c(string)])
  v<-data.frame(d[,c(vars)])
  names(v)<-c(vars)
  y<-d[,c(meas)]
  dt<-cbind(y,v)
  m1<-length(coef(glm(y~.,family = binomial(),data = dt)))

  if (!is.factor(y)){stop("Some kind of error message would be nice, but y should be a factor!")}

  if (ci==TRUE){

    df<-data.frame(matrix(ncol = 3))
    names(df)<-c("pred","or_ci","pv")

    for(i in 1:ncol(x)){
      dat<-cbind(dt,x[,i])
      m<-glm(y~.,family = binomial(),data=dat)

      l<-suppressMessages(round(exp(confint(m))[-c(1:m1),1],2))
      u<-suppressMessages(round(exp(confint(m))[-c(1:m1),2],2))
      or<-round(exp(coef(m))[-c(1:m1)],2)
      or_ci<-paste0(or," (",l," to ",u,")")
      pv<-round(tidy(m)$p.value[-c(1:m1)],3)
      x1<-x[,i]

      if (is.factor(x1)){
        pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")}

      else {pred<-names(x)[i]}

      df<-rbind(df,cbind(pred,or_ci,pv))}}

  if (ci==FALSE){

    df<-data.frame(matrix(ncol = 3))
    names(df)<-c("pred","b","pv")

    for(i in 1:ncol(x)){
      dat<-cbind(dt,x[,i])
      m<-glm(y~.,family = binomial(),data=dat)

      b<-round(coef(m)[-c(1:m1)],3)
      pv<-round(tidy(m)$p.value[-c(1:m1)],3)

      x1<-x[,i]

      if (is.factor(x1)){
        pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")
      }

      else {pred<-names(x)[i]}

      df<-rbind(df,cbind(pred,b,pv))

    }}

  pa<-as.numeric(df[,"pv"])
  t <- ifelse(pa<=0.1,"include","drop")

  pa<-ifelse(pa<0.001,"<0.001",pa)
  pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
               ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

  r<-data.frame(df[,1:2],pa,t)[-1,]

  return(r)
}




