#' A repeated linear regression function
#'
#' For bivariate analyses, to determine which variables to include in adjusted model.
#' Output is a list with two elements: data frame with test results and vector of variable names (from 'string') to include determined by set cutoff ('cut.p').
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param string variables to test. Input as c() of columnnames, use dput().
#' @param ci flag to get results as OR with 95 percent confidence interval.
#' @param data data frame to pull variables from.
#' @param fixed.var flag to set "vars" as fixed in the model. When FALSE, then true bivariate linear regression is performed.
#' @keywords linear regression
#' @export

rep_lm<-function(meas,vars=NULL,string,ci=FALSE,data,fixed.var=FALSE,cut.p=0.1){

  require(broom)
  y<-data[,c(meas)]

  if(is.factor(y)){stop("y is factor")}

  if (fixed.var==FALSE){
    d<-data
    x<-data.frame(d[,c(vars,string)])

    y<-d[,c(meas)]

    names(x)<-c(vars,string)

    if (ci==TRUE){

      df<-data.frame(matrix(NA,ncol = 3))
      names(df)<-c("pred","coef_ci","pv")

      for(i in 1:ncol(x)){
        dat<-data.frame(y=y,x[,i])
        names(dat)<-c("y",names(x)[i])
        m<-lm(y~.,data=dat)

        ci<-suppressMessages(confint(m))
        l<-round(ci[-1,1],2)
        u<-round(ci[-1,2],2)
        or<-round(coef(m)[-1],2)
        coef_ci<-paste0(or," (",l," to ",u,")")
        pv<-round(tidy(m)$p.value[-1],3)
        x1<-x[,i]

        if (is.factor(x1)){
          pred<-paste(names(x)[i],levels(x1)[-1],sep = ".")
        }

        else {pred<-names(x)[i]}

        df<-rbind(df,cbind(pred,coef_ci,pv))
      }
    }

    else {

      df<-data.frame(matrix(NA,ncol = 3))
      names(df)<-c("pred","b","pv")

      for(i in 1:ncol(x)){
        dat<-data.frame(y=y,x[,i])
        names(dat)<-c("y",names(x)[i])
        m<-lm(y~.,data=dat)

        b<-round(coef(m)[-1],3)
        pv<-round(tidy(m)$p.value[-1],3)
        x1<-x[,i]

        if (is.factor(x1)){
          pred<-paste(names(x)[i],levels(x1)[-1],sep = ".")
        }
        else {pred<-names(x)[i]}
        df<-rbind(df,cbind(pred,b,pv))
      }}

    pa<-as.numeric(df[,3])
    t <- ifelse(pa<=cut.p,"include","drop")
    pa<-ifelse(pa<0.001,"<0.001",pa)
    pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                 ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

    r<-data.frame(df[,1:2],pa,t)[-1,]
  }

  if (fixed.var==TRUE){
    d<-data
    x<-data.frame(d[,c(string)])
    v<-data.frame(d[,c(vars)])

    y<-d[,c(meas)]
    dt<-cbind(y=y,v)
    m1<-length(coef(lm(y~.,data = dt)))

    names(v)<-c(vars)

    if (ci==TRUE){

      df<-data.frame(matrix(NA,ncol = 3))
      names(df)<-c("pred","coef_ci","pv")

      for(i in 1:ncol(x)){
        dat<-cbind(dt,x[,i])
        m<-lm(y~.,data=dat)

        ci<-suppressMessages(confint(m))
        l<-round(ci[-c(1:m1),1],2)
        u<-round(ci[-c(1:m1),2],2)
        or<-round(coef(m)[-c(1:m1)],2)
        coef_ci<-paste0(or," (",l," to ",u,")")
        pv<-round(tidy(m)$p.value[-c(1:m1)],3)
        x1<-x[,i]

        if (is.factor(x1)){
          pred<-paste(names(x)[i],levels(x1)[-1],sep = ".")}

        else {pred<-names(x)[i]}

        df<-rbind(df,cbind(pred,coef_ci,pv))}}

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
          pred<-paste(names(x)[i],levels(x1)[-1],sep = ".")
        }
        else {pred<-names(x)[i]}
        df<-rbind(df,cbind(pred,b,pv))
      }}

    pa<-as.numeric(df[,3])
    t <- ifelse(pa<=cut.p,"include","drop")
    pa<-ifelse(pa<0.001,"<0.001",pa)
    pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                 ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

    r<-data.frame(df[,1:2],pa,t)[-1,]
  }

  p<-r$pred[r$t=="include"]
  s<-c()
  for (i in 1:length(p)){
    s<-c(s,unlist(strsplit(p[i], "[.]"))[1])
  }
  return(list(tests=r,to_include=unique(s)))
}
