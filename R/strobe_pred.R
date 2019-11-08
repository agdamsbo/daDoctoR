#' Logistic regression of predictors according to STROBE
#'
#' Printable table of logistic regression analysis according to STROBE.
#' @param meas binary outcome meassure variable, column name in data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @param n.by.adj flag to indicate wether to count number of patients in adjusted model or overall for outcome meassure not NA.
#' @keywords logistic
#' @export

strobe_pred<-function(meas,adj,data,dec=2,n.by.adj=FALSE){
  ## Ønskeliste:
  ##
  ## - Tæl selv antal a NA'er

  require(dplyr)

  d<-data
  m<-d[,c(meas)]

  ads<-d[,c(adj)]

  ## Crude ORs

  dfcr<-data.frame(matrix(NA,ncol = 3))
  names(dfcr)<-c("pred","or_ci","pv")
  n.mn<-c()

  nref<-c()

  for(i in 1:ncol(ads)){
    dat<-data.frame(m=m,ads[,i])
    names(dat)<-c("m",names(ads)[i])
    mn<-glm(m~.,family = binomial(),data=dat)
    n.mn<-c(n.mn,nrow(mn$model))

    suppressMessages(ci<-exp(confint(mn)))
    l<-round(ci[-1,1],2)
    u<-round(ci[-1,2],2)
    or<-round(exp(coef(mn))[-1],2)
    or_ci<-paste0(or," (",l," to ",u,")")
    pv<-round(tidy(mn)$p.value[-1],3)
    x1<-ads[,i]

    if (is.factor(x1)){
      pred<-paste0(names(ads)[i],levels(x1)[-1])
    }

    else {
      pred<-names(ads)[i]
    }

    dfcr<-rbind(dfcr,cbind(pred,or_ci,pv))
  }


  ## Mutually adjusted  ORs

  dat<-data.frame(m=m,ads)
  ma <- glm(m ~ .,family = binomial(), data = dat)
  miss<-length(ma$na.action)

  actable <- coef(summary(ma))
  pa <- actable[,4]
  pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
  pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
               ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

  apv<-pa[1:length(coef(ma))]

  aco<-round(exp(coef(ma)),dec)
  suppressMessages(aci<-round(exp(confint(ma)),dec))
  alo<-aci[,1]
  aup<-aci[,2]
  aor_ci<-paste0(aco," (",alo," to ",aup,")")

  # names(dat2)<-c(var,names(ads))

  nq<-c()

  if (n.by.adj==TRUE){
    dat2<-ma$model[,-1]
    for (i in 1:ncol(dat2)){
      if (is.factor(dat2[,i])){
        vec<-dat2[,i]
        ns<-names(dat2)[i]
        for (r in 1:length(levels(vec))){
          vr<-levels(vec)[r]
          n<-as.numeric(length(vec[vec==vr&!is.na(vec)]))
          nall<-as.numeric(length(dat2[,c(ns)]))
          n.meas<-nall
          nl<-paste0(ns,levels(vec)[r])
          pro<-round(n/nall*100,0)
          rt<-paste0(n," (",pro,"%)")
          nq<-rbind(nq,cbind(nl,rt))
        }}
      if (!is.factor(dat2[,i])){
        num<-dat2[,i]
        nl<-names(dat2)[i]
        n<-as.numeric(length(num[!is.na(num)]))
        nall<-as.numeric(nrow(dat2))
        n.meas<-nall
        pro<-round(n/nall*100,0)
        rt<-paste0(n," (",pro,"%)")
        nq<-rbind(nq,cbind(nl,rt))
      }}}

  else {
    dat2<-dat[!is.na(dat[,1]),][,-1]
    n.meas<-nrow(dat2)
    for (i in 1:ncol(dat2)){
      if (is.factor(dat2[,i])){
        vec<-dat2[,i]
        ns<-names(dat2)[i]
        for (r in 1:length(levels(vec))){
          vr<-levels(vec)[r]
          n<-as.numeric(length(vec[vec==vr&!is.na(vec)]))
          nall<-as.numeric(n.mn[i])
          nl<-paste0(ns,levels(vec)[r])
          pro<-round(n/nall*100,0)
          rt<-paste0(n," (",pro,"%)")
          nq<-rbind(nq,cbind(nl,rt))
        }}
      if (!is.factor(dat2[,i])){
        num<-dat2[,i]
        nl<-names(dat2)[i]
        n<-as.numeric(length(num[!is.na(num)]))
        nall<-as.numeric(n.meas)
        pro<-round(n/nall*100,0)
        rt<-paste0(n," (",pro,"%)")
        nq<-rbind(nq,cbind(nl,rt))
      }}}


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

  header<-data.frame(matrix("Adjusted",ncol = ncol(coll)),stringsAsFactors = F)
  names(header)<-names(coll)

  df<-data.frame(rbind(header,coll),stringsAsFactors = F)

  names(dfcr)[1]<-c("names")

  suppressWarnings(re<-left_join(df,dfcr,by="names"))

  ref<-data.frame(re[,1],re[,2],re[,5],re[,3])

  names(ref)<-c("Variable",paste0("N=",n.meas),"Crude OR (95 % CI)","Mutually adjusted OR (95 % CI)")

  ls<-list(tbl=ref,miss,n.meas,nrow(d))
  names(ls)<-c("Printable table","Deleted due to missingness in adjusted analysis","Number of outcome observations","Length of dataframe")

  return(ls)
}
