#' Print regression results according to STROBE
#'
#' Printable table of linear regression analysis of group vs var for meas.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param var exposure variable to compare against (active vs placebo). As string.
#' @param groups groups to compare, as string.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @keywords cpr
#' @export
#' @examples
#' strobe_diff1()

strobe_diff1<-function(meas,var,group,adj,data,dec=2){
  ## meas: sdmt
  ## var: rtreat
  ## group: genotype
  ## for dichotome exposure variable (var)

  d<-data
  m<-d[,c(meas)]
  v<-d[,c(var)]
  g<-d[,c(group)]

  ads<-d[,c(adj)]

  dat<-data.frame(m,v,g,ads)

  df<-data.frame(matrix(ncol=7))

  if(!is.factor(m)){

    for (i in 1:length(levels(g))){
      grp<-levels(dat$g)[i]
      di<-dat[dat$g==grp,][,-3]

      mod<-lm(m~v,data=di)
      co<-round(coef(mod)[-1],dec)
      lo<-round(confint(mod)[-1,1],dec)
      up<-round(confint(mod)[-1,2],dec)

      ci<-paste0(co," (",lo," to ",up,")")

      amod<-lm(m~.,data=di)
      aco<-round(coef(amod)[2],dec)
      alo<-round(confint(amod)[2,1],dec)
      aup<-round(confint(amod)[2,2],dec)

      aci<-paste0(aco," (",alo," to ",aup,")")

      nr<-c()

      for (r in 1:2){
        vr<-levels(di$v)[r]
        dr<-di[di$v==vr,]
        n<-as.numeric(nrow(dr[!is.na(dr$m),]))
        mean<-round(mean(dr$m,na.rm = TRUE),dec-1)
        sd<-round(sd(dr$m,na.rm = TRUE),dec-1)
        ms<-paste0(mean," (",sd,")")

        nr<-c(nr,n,ms)
      }
      irl<-c(grp,nr,ci,aci)
      df<-rbind(df,irl)
      names(df)<-c("grp",paste0("N.",substr(levels(v)[1],1,3)),paste0("M.",substr(levels(v)[1],1,3)),paste0("N.",substr(levels(v)[2],1,3)),paste0("M.",substr(levels(v)[2],1,3)),"diff","ad.diff")
    }}

  if(is.factor(m)){

    for (i in 1:length(levels(g))){
      grp<-levels(dat$g)[i]
      di<-dat[dat$g==grp,][,-3]

      mod<-glm(m~v,family=binomial(),data=di)
      co<-round(exp(coef(mod)[-1]),dec)
      lo<-round(exp(confint(mod)[-1,1]),dec)
      up<-round(exp(confint(mod)[-1,2]),dec)

      ci<-paste0(co," (",lo," to ",up,")")

      amod<-glm(m~.,family=binomial(),data=di)
      aco<-round(exp(coef(amod)[2]),dec)
      alo<-suppressMessages(round(exp(confint(amod)[2,1]),dec))
      aup<-suppressMessages(round(exp(confint(amod)[2,2]),dec))

      aci<-paste0(aco," (",alo," to ",aup,")")

      nr<-c()

      for (r in 1:2){
        vr<-levels(di$v)[r]
        dr<-di[di$v==vr,]
        n<-as.numeric(nrow(dr[!is.na(dr$m),]))
        nl<-levels(m)[2]
        out<-nrow(dr[dr$m==nl&!is.na(dr$m),])
        pro<-round(out/n*100,0)
        rt<-paste0(out," (",pro,"%)")

        nr<-c(nr,n,rt)
      }
      irl<-c(grp,nr,ci,aci)
      df<-rbind(df,irl)
      names(df)<-c("grp",paste0("N.",substr(levels(v)[1],1,3)),paste0(nl,".",substr(levels(v)[1],1,3)),paste0("N.",substr(levels(v)[2],1,3)),paste0(nl,".",substr(levels(v)[2],1,3)),"OR","ad.OR")
    }}

  return(df)
}
