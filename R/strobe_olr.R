#' Print regression results according to STROBE
#'
#' Printable table of logistic regression analysis oaccording to STROBE.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param vars variables to compare against. As vector of columnnames.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @keywords olr
#' @export
#' @examples
#' strobe_olr()

strobe_olr<-function(meas,vars,data,dec=2){

  require(MASS)
  require(dplyr)

  d<-data
  m<-d[,c(meas)]
  v<-d[,c(vars)]

  dat<-data.frame(m,v)

  ma <- polr(m ~ ., data = dat, Hess=TRUE)

  actable <- coef(summary(ma))
  pa <- pnorm(abs(actable[, "t value"]), lower.tail = FALSE) * 2
  pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
  pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
               ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

  apv<-pa[1:length(coef(ma))]

  aco<-round(exp(coef(ma)),dec)
  aci<-round(exp(confint(ma)),dec)
  alo<-aci[,1]
  aup<-aci[,2]
  aor_ci<-paste0(aco," (",alo," to ",aup,")")

  dat2<-ma$model[,-1]
  # names(dat2)<-c(var,names(ads))
  nq<-c()

  for (i in 1:ncol(dat2)){
    if (is.factor(dat2[,i])){
      vec<-dat2[,i]
      ns<-names(dat2)[i]
      for (r in 1:length(levels(vec))){
        vr<-levels(vec)[r]
        dr<-vec[vec==vr]
        n<-as.numeric(length(dr))
        nall<-as.numeric(nrow(dat2))
        nl<-paste0(ns,levels(vec)[r])
        pro<-round(n/nall*100,0)
        rt<-paste0(n," (",pro,"%)")
        nq<-rbind(nq,cbind(nl,rt))
      }
    }
    if (!is.factor(dat2[,i])){
      num<-dat2[,i]
      ns<-names(dat2)[i]
      n<-as.numeric(nrow(dat2))
      nall<-as.numeric(nrow(dat2))
      pro<-round(n/nall*100,0)
      rt<-paste0(n," (",pro,"%)")
      nq<-rbind(nq,cbind(ns,rt))
    }
  }

  rnames<-c()

  for (i in 1:ncol(dat2)){
    if (is.factor(dat2[,i])){
      rnames<-c(rnames,names(dat2)[i],paste0(names(dat2)[i],levels(dat2[,i])))
    }
    if (!is.factor(dat2[,i])){
      rnames<-c(rnames,paste0(names(dat2)[i],".all"),names(dat2)[i])
    }
  }
  res<-cbind(aor_ci,apv)
  rest<-data.frame(names=row.names(res),res,stringsAsFactors = F)

  numb<-data.frame(names=nq[,c("nl")],N=nq[,c("rt")],stringsAsFactors = F)
  namt<-data.frame(names=rnames,stringsAsFactors = F)

  coll<-left_join(left_join(namt,numb,by="names"),rest,by="names")

  df<-data.frame(coll)

  names(df)<-c("Variable","N","OR (95 % CI)","p value")

  return(df)
}
