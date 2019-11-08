#' Print regression results according to STROBE
#'
#' Printable table of regression analysis by group for meas. Detects wether to perform logistic or linear regression.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param groups groups to compare, as string.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @keywords strobe
#' @export

strobe_diff_twodim<-function(meas,group,adj,data,dec=2){
  ## meas: sdmt
  ## var: rtreat
  ## group: genotype
  ## for dichotome exposure variable (var)

  d<-data
  m<-d[,c(meas)]
  g<-d[,c(group)]

  ads<-d[,c(adj)]

  dat<-data.frame(m,g,ads)

  df<-data.frame(grp=c(group,as.character(levels(g))))

  if(!is.factor(m)){

    mod<-lm(m~g,data=dat)
    ci<-confint(mod)
    co<-round(coef(mod)[-1],dec)
    lo<-round(ci[-1,1],dec)
    up<-round(ci[-1,2],dec)

    or_ci<-c("REF",paste0(co," (",lo," to ",up,")"))

    amod<-lm(m~.,data=dat)
    aci<-confint(amod)
    aco<-round(coef(amod)[2:length(levels(g))],dec)
    alo<-round(aci[2:length(levels(g)),1],dec)
    aup<-round(aci[2:length(levels(g)),2],dec)

    aor_ci<-c("REF",paste0(aco," (",alo," to ",aup,")"))

    nr<-c()

    for (r in 1:length(levels(g))){
      vr<-levels(dat$g)[r]
      dr<-dat[dat$g==vr,]
      n<-as.numeric(nrow(dr[!is.na(dr$m),]))
      mean<-round(mean(dr$m,na.rm = TRUE),dec-1)
      sd<-round(sd(dr$m,na.rm = TRUE),dec-1)
      ms<-paste0(mean," (",sd,")")

      nr<-c(nr,n,ms)
    }
    irl<-rbind(matrix(NA,ncol=4),cbind(matrix(nr,ncol=2,byrow = TRUE),cbind(or_ci,aor_ci)))
    colnames(irl)<-c("N","Mean (SD)","Difference","Adjusted Difference")
    df<-cbind(df,irl)
    ls<-list(linear.regression=df)
  }

  if(is.factor(m)){
    di<-dat

    mod<-glm(m~g,family=binomial(),data=di)
    ci<-exp(confint(mod))
    co<-round(exp(coef(mod))[-1],dec)
    lo<-round(ci[-1,1],dec)
    up<-round(ci[-1,2],dec)

    or_ci<-c("REF",paste0(co," (",lo," to ",up,")"))

    amod<-glm(m~.,family=binomial(),data=di)
    aci<-exp(confint(amod))
    aco<-round(exp(coef(amod))[2:length(levels(g))],dec)
    alo<-round(aci[2:length(levels(g)),1],dec)
    aup<-round(aci[2:length(levels(g)),2],dec)

    aor_ci<-c("REF",paste0(aco," (",alo," to ",aup,")"))

    nr<-c()

    for (r in 1:length(levels(g))){
      vr<-levels(dat$g)[r]
      dr<-dat[dat$g==vr,]
      n<-as.numeric(nrow(dr[!is.na(dr$m),]))
      nl<-levels(m)[2]
      out<-nrow(dr[dr$m==nl&!is.na(dr$m),])
      pro<-round(out/n*100,0)
      rt<-paste0(out," (",pro,"%)")

      nr<-c(nr,n,rt)
    }
    irl<-rbind(matrix(NA,ncol=4),cbind(matrix(nr,ncol=2,byrow = TRUE),cbind(or_ci,aor_ci)))
    colnames(irl)<-c("N",paste0("N.",nl),"OR","Adjusted OR")
    df<-cbind(df,irl)
    ls<-list(logistic.regression=df)
  }

  ls$adjustments<-names(ads)
  return(ls)
}
