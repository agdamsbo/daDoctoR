#' Print regression results according to STROBE
#'
#' Printable table of three dimensional regression analysis of group vs var for meas. By var.
#' @param meas outcome meassure variable name in data-data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param var binary exposure variable to compare against (active vs placebo). As string.
#' @param groups groups to compare, as string.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @keywords strobe
#' @export
#' @examples
#' strobe_diff_byvar()

strobe_diff_byvar<-function(meas,var,group,adj,data,dec=2){
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

  df<-data.frame(grp=c(NA,as.character(levels(g))))

  if(!is.factor(m)){

    for (i in 1:length(levels(v))){
      grp<-levels(dat$v)[i]
      di<-dat[dat$v==grp,][,-2]

      mod<-lm(m~g,data=di)
      co<-c("-",round(coef(mod)[-1],dec))
      lo<-c("-",round(confint(mod)[-1,1],dec))
      up<-c("-",round(confint(mod)[-1,2],dec))

      ci<-paste0(co," (",lo," to ",up,")")

      amod<-lm(m~.,data=di)
      aco<-c("-",round(coef(amod)[2:length(levels(g))],dec))
      alo<-c("-",round(confint(amod)[2:length(levels(g)),1],dec))
      aup<-c("-",round(confint(amod)[2:length(levels(g)),2],dec))

      aci<-paste0(aco," (",alo," to ",aup,")")

      nr<-c()

      for (r in 1:length(levels(g))){
        vr<-levels(di$g)[r]
        dr<-di[di$g==vr,]
        n<-as.numeric(nrow(dr[!is.na(dr$m),]))
        mean<-round(mean(dr$m,na.rm = TRUE),dec-1)
        sd<-round(sd(dr$m,na.rm = TRUE),dec-1)
        ms<-paste0(mean," (",sd,")")

        nr<-c(nr,n,ms)
      }
      irl<-rbind(matrix(grp,ncol=4),cbind(matrix(nr,ncol=2,byrow = TRUE),cbind(ci,aci)))
      colnames(irl)<-c("N","Mean (SD)","Difference","Adjusted Difference")
      df<-cbind(df,irl)
    }}

  if(is.factor(m)){

    for (i in 1:length(levels(v))){
      grp<-levels(dat$v)[i]
      di<-dat[dat$v==grp,][,-2]

      mod<-glm(m~g,family=binomial(),data=di)
      co<-c("-",round(exp(coef(mod)[-1]),dec))
      lo<-c("-",round(exp(confint(mod)[-1,1]),dec))
      up<-c("-",round(exp(confint(mod)[-1,2]),dec))

      ci<-paste0(co," (",lo," to ",up,")")

      amod<-glm(m~.,family=binomial(),data=di)
      aco<-c("-",suppressMessages(round(exp(coef(amod)[2:length(levels(g))]),dec)))
      alo<-c("-",suppressMessages(round(exp(confint(amod)[2:length(levels(g)),1]),dec)))
      aup<-c("-",suppressMessages(round(exp(confint(amod)[2:length(levels(g)),2]),dec)))

      aci<-paste0(aco," (",alo," to ",aup,")")

      nr<-c()

      for (r in 1:length(levels(g))){
        vr<-levels(di$g)[r]
        dr<-di[di$g==vr,]
        n<-as.numeric(nrow(dr[!is.na(dr$m),]))
        nl<-levels(m)[2]
        out<-nrow(dr[dr$m==nl&!is.na(dr$m),])
        pro<-round(out/n*100,0)
        rt<-paste0(out," (",pro,"%)")

        nr<-c(nr,n,rt)
      }
      irl<-rbind(matrix(grp,ncol=4),cbind(matrix(nr,ncol=2,byrow = TRUE),cbind(ci,aci)))
      colnames(irl)<-c("N",paste0("N.",nl),"OR","Adjusted OR")
      df<-cbind(df,irl)
    }}

  return(df)
}
