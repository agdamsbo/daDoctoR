#' Print regression results according to STROBE
#'
#' Printable table of linear regression analysis of group vs var for meas. By group.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param var exposure variable to compare against (active vs placebo). As string.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @keywords cpr
#' @export
#' @examples
#' strobe_olr()

strobe_olr<-function(meas,var,adj,data,dec=2){
  #' Ønskeliste:
  #'
  #' - Sum af alle, der indgår (Overall N)
  #' - Ryd op i kode, der der er overflødig %-regning, alternativt, så fiks at NA'er ikke skal regnes med.
  #'
  require(MASS)
  require(dplyr)

  d<-data
  m<-d[,c(meas)]
  v<-d[,c(var)]

  ads<-d[,c(adj)]
  dat<-data.frame(m,v,ads)
  df<-data.frame(matrix(ncol=4))

  mn <- polr(m ~ v, data = dat, Hess=TRUE)
  ma <- polr(m ~ ., data = dat, Hess=TRUE)

  ctable <- coef(summary(mn))
  pa <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
  pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
  pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
               ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))
  pv<-c("REF",pa[1:length(coef(mn))])

  co<-round(exp(coef(mn)),dec)
  ci<-confint(mn)
  lo<-round(exp(ci[,1]),dec)
  up<-round(exp(ci[,2]),dec)

  or_ci<-c("REF",paste0(co," (",lo," to ",up,")"))

  nr<-c()

  for (r in 1:length(levels(dat[,2]))){
    vr<-levels(dat[,2])[r]
    dr<-dat[dat[,2]==vr,]
    n<-as.numeric(nrow(dr))

    ## Af en eller anden grund bliver der talt for mange med.
    # nall<-as.numeric(nrow(dat[!is.na(dat[,2]),]))
    nl<-levels(m)[r]
    # pro<-round(n/nall*100,0)
    # rt<-paste0(n," (",pro,"%)")
    nr<-rbind(nr,cbind(nl,n))
  }

  mms<-data.frame(cbind(nr,or_ci,pv))
  header<-data.frame(matrix(var,ncol = ncol(mms)))
  names(header)<-names(mms)

  ls<-list(unadjusted=data.frame(rbind(header,mms)))

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

  dat2<-dat[,-1]
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
        # nall<-as.numeric(nrow(dat[!is.na(dat2[,c(ns)]),]))
        nl<-paste0(ns,levels(vec)[r])
        # pro<-round(n/nall*100,0)
        # rt<-paste0(n," (",pro,"%)")
        nq<-rbind(nq,cbind(nl,n))
      }
    }
    if (!is.factor(dat2[,i])){
      num<-dat2[,i]
      ns<-names(dat2)[i]
      nall<-as.numeric(nrow(dat[!is.na(dat2[,c(ns)]),]))
      nq<-rbind(nq,cbind(ns,nall))
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

  numb<-data.frame(names=nq[,c("nl")],N=nq[,c("n")],stringsAsFactors = F)
  namt<-data.frame(names=rnames,stringsAsFactors = F)

  coll<-left_join(left_join(namt,numb,by="names"),rest,by="names")

  header<-data.frame(matrix("Adjusted",ncol = ncol(coll)))
  names(header)<-names(coll)

  ls$adjusted<-data.frame(rbind(header,coll))

  fnames<-c("Variable","N","OR (95 % CI)","p value")

  names(ls$unadjusted)<-fnames
  names(ls$adjusted)<-fnames

  return(ls)
}
