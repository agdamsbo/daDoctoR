#' Print ordinal logistic regression results according to STROBE
#'
#' Printable table of ordinal logistic regression with bivariate and multivariate analyses.
#' Table according to STROBE. Uses polr() funtion of the MASS-package.
#' Formula analysed is the most simple m~v1+v2+vn. The is no significance test. Results are point estimates with 95 percent CI.
#' @param meas outcome meassure variable name or response in data-data.frame as a string. Should be factor, preferably ordered.
#' @param vars variables to compare against. As vector of columnnames.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @param n.by.adj flag to indicate wether to count number of patients in adjusted model or overall for outcome meassure not NA.
#' @keywords olr
#' @export

<<<<<<< HEAD
print_olr<-function(meas,vars,data,dec=2,n.by.adj=FALSE){
=======
strobe_olr<-function(meas,vars,data,dec=2,n.by.adj=FALSE){
>>>>>>> d8ffa3dc7b67e43a846bbd58e055a141a010b304
## For calculation of p-value from t-value see rep_olr()

  require(MASS)
  require(dplyr)

  d<-data
  m<-d[,c(meas)]

  ads<-d[,c(vars)]

  if(!is.factor(m)){stop("'meas' should be a factor, preferably ordered.")}

  if(is.factor(m)){

    ## Crude ORs

    dfcr<-data.frame(matrix(NA,ncol = 2))
    names(dfcr)<-c("pred","or_ci")
    n.mn<-c()
    nref<-c()

    for(i in 1:ncol(ads)){
      dat<-data.frame(m=m,ads[,i])
      names(dat)<-c("m",names(ads)[i])
      mn<-polr(m ~ ., data = dat, Hess=TRUE)
      n.mn<-c(n.mn,nrow(mn$model))

      suppressMessages(ci<-matrix(exp(confint(mn)),ncol=2))
      l<-round(ci[,1],dec)
      u<-round(ci[,2],dec)
      or<-round(exp(coef(mn)),dec)
      or_ci<-paste0(or," (",l," to ",u,")")

      x1<-ads[,i]

      if (is.factor(x1)){
        pred<-paste0(names(ads)[i],levels(x1)[-1])
      }

      else {
        pred<-names(ads)[i]
      }

      dfcr<-rbind(dfcr,cbind(pred,or_ci))
    }


    ## Mutually adjusted  ORs

    dat<-data.frame(m=m,ads)
    ma <-polr(m ~ ., data = dat, Hess=TRUE)
    miss<-length(ma$na.action)

    aco<-round(exp(coef(ma)),dec)
    suppressMessages(aci<-round(exp(confint(ma)),dec))
    alo<-aci[,1]
    aup<-aci[,2]
    aor_ci<-paste0(aco," (",alo," to ",aup,")")

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
        }}
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
    rest<-data.frame(names=names(aco),aor_ci,stringsAsFactors = F)

    numb<-data.frame(names=nq[,c("nl")],N=nq[,c("rt")],stringsAsFactors = F)
    namt<-data.frame(names=rnames,stringsAsFactors = F)

    coll<-left_join(left_join(namt,numb,by="names"),rest,by="names")

    header<-data.frame(matrix(paste0("Chance of higher ",meas),ncol = ncol(coll)),stringsAsFactors = F)
    names(header)<-names(coll)

    df<-data.frame(rbind(header,coll),stringsAsFactors = F)

    names(dfcr)[1]<-c("names")

    suppressWarnings(re<-left_join(df,dfcr,by="names"))

    rona<-c()
    for (i in 1:length(ads)){
      if (is.factor(ads[,i])){
        rona<-c(rona,names(ads[i]),levels(ads[,i]))}
      if (!is.factor(ads[,i])){
        rona<-c(rona,names(ads[i]),"Per unit increase")
      }
    }

    ref<-data.frame(c(NA,rona),re[,2],re[,4],re[,3])

    names(ref)<-c("Variable",paste0("N=",n.meas),"Bivariate OLR (95 % CI)","Mutually adjusted OLR (95 % CI)")

    ls<-list(tbl=ref,miss,n.meas,nrow(d))
    names(ls)<-c("Printable table","Deleted due to missingness in adjusted analysis","Number of outcome observations","Length of dataframe")
  }


  return(ls)
}
