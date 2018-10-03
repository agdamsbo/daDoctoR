#' A repeated regression function for change-in-estimate analysis
#'
#' For bivariate analyses.
#' @param y Effect meassure.
#' @param v1 Main variable in model
#' @param string String of columnnames from dataframe to include. Use dput().
#' @keywords change-in-estimate
#' @export
#' @examples
#' cie_test()

cie_test<-function(y,v1,string,data,logistic=FALSE,cut=0.1,v2=NULL,v3=NULL){
## Calculating variables, that should be included for a change in estimate analysis.
## v1-3 are possible locked variables, y is the outcome vector.
## String defines variables to test, and is provided as vector of variable names. Use dput().
## From "Modeling and variable selection in epidemiologic analysis." - S. Greenland, 1989.

  require(broom)

  d<-data
  x<-select(d,one_of(c(string)))
  if(logistic==FALSE){
if (is.factor(y)){stop("Some kind of error message would be nice, but y should not be a factor!")}

if (is.null(v2)&is.null(v3)){

  e<-as.numeric(round(coef(lm(y~v1)),3))[1]


       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){

     m<-lm(y~v1+x[,i])

     b<-as.numeric(round(coef(m),3))[1]

     v<-x[,i]

   pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))
  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*cut),"include","drop"))
df<-cbind(df,t)
       }

       if (!is.null(v2)&is.null(v3)){

           e<-as.numeric(round(coef(lm(y~v1+v2)),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){

     m<-lm(y~v1+v2+x[,i])

     b<-as.numeric(round(coef(m),3))[1]

     v<-x[,i]

       pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))
  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*cut),"include","drop"))
df<-cbind(df,t)
       }

       if (!is.null(v2)&!is.null(v3)){

           e<-as.numeric(round(coef(lm(y~v1+v2+v3)),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){

     m<-lm(y~v1+v2+v3+x[,i])

     b<-as.numeric(round(coef(m),3))[1]

     v<-x[,i]

       pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))

  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*cut),"include","drop"))
df<-cbind(df,t)
}}

if(logistic==TRUE){

if (!is.factor(y)){stop("Some kind of error message would be nice, but y should be a factor!")}

if (is.null(v2)&is.null(v3)){

  e<-as.numeric(round(exp(coef(glm(y~v1,family=binomial()))),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){

     m<-glm(y~v1+x[,i],family=binomial())

     b<-as.numeric(round(exp(coef(m)),3))[1]

     v<-x[,i]

   pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))
  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*cut),"include","drop"))
df<-cbind(df,t)
       }

       if (!is.null(v2)&is.null(v3)){

           e<-as.numeric(round(exp(coef(glm(y~v1+v2,family=binomial()))),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){

     m<-glm(y~v1+v2+x[,i],family=binomial())

     b<-as.numeric(round(exp(coef(m)),3))[1]

     v<-x[,i]

       pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))
  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*cut),"include","drop"))
df<-cbind(df,t)
       }

       if (!is.null(v2)&!is.null(v3)){

           e<-as.numeric(round(exp(coef(glm(y~v1+v2+v3,family=binomial()))),3))[1]

       df<-data.frame(pred="base",b=e)

  for(i in 1:ncol(x)){

     m<-glm(y~v1+v2+v3+x[,i],family=binomial())

     b<-as.numeric(round(exp(coef(m)),3))[1]

     v<-x[,i]

       pred<-paste(names(x)[i])

     df<-rbind(df,cbind(pred,b))

  }
       t<-c(NA,ifelse(abs(e-as.numeric(df[-1,2]))>=(e*cut),"include","drop"))
df<-cbind(df,t)
}}

    return(df)
}
