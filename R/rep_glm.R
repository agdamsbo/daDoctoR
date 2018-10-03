#' A repeated logistic regression function
#'
#' For bivariate analyses.
#' @param y Effect meassure.
#' @param v1 Main variable in model
#' @keywords logistic regression
#' @export
#' @examples
#' rep_glm()

rep_glm<-function(y,v1,string,ci=FALSE,data,v2=NULL,v3=NULL){
## x is data.frame of predictors, y is vector of an aoutcome as a factor
## output is returned as coefficient, or if or=TRUE as OR with 95 % CI.
## The confint() function is rather slow, causing the whole function to hang when including many predictors and calculating the ORs with CI.

  require(broom)

  d<-data
  x<-select(d,one_of(c(string)))
  m1<-length(coef(glm(y~v1,family = binomial())))

if (!is.factor(y)){stop("Some kind of error message would be nice, but y should be a factor!")}

    if (ci==TRUE){

             df<-data.frame(matrix(ncol = 4))
  names(df)<-c("pred","or_ci","pv","t")

      for(i in 1:ncol(x)){
     m<-glm(y~v1+x[,i],family = binomial())

     l<-suppressMessages(round(exp(confint(m))[-c(1:m1),1],2))
     u<-suppressMessages(round(exp(confint(m))[-c(1:m1),2],2))
     or<-round(exp(coef(m))[-c(1:m1)],2)

     or_ci<-paste0(or," (",l," to ",u,")")

     pv<-round(tidy(m)$p.value[-c(1:m1)],3)
     pv<-ifelse(pv<0.001,"<0.001",pv)

         t <- ifelse(pv<=0.1|pv=="<0.001","include","drop")

     pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),
                  ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv))


    v<-x[,i]

    if (is.factor(v)){
       pred<-paste(names(x)[i],levels(v)[-1],sep = "_")
    }

       else {pred<-names(x)[i]}

      df<-rbind(df,cbind(pred,or_ci,pv,t))

    }}

  if (ci==FALSE){

       df<-data.frame(matrix(ncol = 4))
  names(df)<-c("pred","b","pv","t")

  for(i in 1:ncol(x)){
     m<-glm(y~v1+x[,i],family = binomial())

     b<-round(coef(m)[-c(1:m1)],3)

     pv<-round(tidy(m)$p.value[-c(1:m1)],3)
     pv<-ifelse(pv<0.001,"<0.001",pv)

    t <- ifelse(pv<=0.1|pv=="<0.001","include","drop")

     pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),
                  ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv))


     v<-x[,i]

     if (is.factor(v)){
       pred<-paste(names(x)[i],levels(v)[-1],sep = "_")
     }

       else {pred<-names(x)[i]}

     df<-rbind(df,cbind(pred,b,pv,t))

  }}
  result<-df
  return(df)
}
