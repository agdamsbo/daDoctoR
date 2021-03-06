#' A repeated logistic regression function
#'
#' For bivariate analyses. The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.
#' @param meas Effect meassure. Input as c() of columnnames, use dput().
#' @param vars variables in model. Input as c() of columnnames, use dput().
#' @param string variables to test. Input as c() of columnnames, use dput().
#' @param ci flag to get results as OR with 95 percent confidence interval.
#' @param data dataframe to pull variables from.
#' @param fixed.var flag to set "vars" as fixed in the model. When FALSE, then true bivariate logistic regression is performed.
#' @keywords logistic
#' @export

rep_glm<-function(meas,vars=NULL,string,ci=FALSE,data,fixed.var=FALSE)
  {
  ## Intro and definitions
  require(broom)
  y<-data[,c(meas)]
  ## Factor check
  if(!is.factor(y)){stop("y is not a factor")}
  ## Running "true" bivariate analysis
  if (fixed.var==FALSE){
    d<-data
    x<-data.frame(d[,c(vars,string)])

    y<-d[,c(meas)]

    names(x)<-c(vars,string)

    if (ci==TRUE){

      df<-data.frame(matrix(NA,ncol = 3))
      names(df)<-c("pred","or_ci","pv")

      for(i in 1:ncol(x)){
        dat<-data.frame(y=y,x[,i])
        names(dat)<-c("y",names(x)[i])
        m<-glm(y~.,family = binomial(),data=dat)

        suppressMessages(ci<-exp(confint(m)))
        l<-round(ci[-1,1],2)
        u<-round(ci[-1,2],2)
        or<-round(exp(coef(m))[-1],2)
        or_ci<-paste0(or," (",l," to ",u,")")
        pv<-round(tidy(m)$p.value[-1],3)
        x1<-x[,i]

        if (is.factor(x1)){
          pred<-paste(names(x)[i],levels(x1)[-1],sep = "_")
        }

        else {pred<-names(x)[i]}

        df<-rbind(df,cbind(pred,or_ci,pv))
      }
    }

    else {

      df<-data.frame(matrix(NA,ncol = 3))
      names(df)<-c("pred","b","pv")

      for(i in 1:ncol(x)){
        dat<-data.frame(y=y,x[,i])
        names(dat)<-c("y",names(x)[i])
        m<-glm(y~.,family = binomial(),data=dat)

        b<-round(coef(m)[-1],3)
        pv<-round(tidy(m)$p.value[-1],3)
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
  }

  ## Running multivariate analyses (eg "bivariate" analyses with fixed variables)
  if (fixed.var==TRUE){
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

        ci<-exp(confint(m))
        l<-suppressMessages(round(ci[-c(1:m1),1],2))
        u<-suppressMessages(round(ci[-c(1:m1),2],2))
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
  }
  return(r)
}
