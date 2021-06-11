#' Regression model of predictors according to STROBE, bi- and multivariable. Printable result.
#'
#' Printable table of regression model according to STROBE for linear or binary outcome-variables.
#' Includes both bivariate and multivariate in the same table.
#' Output is a list, with the first item being the main "output" as a dataframe.
#' Automatically uses logistic regression model for dichotomous outcome variable and linear regression model for continuous outcome variable. Linear regression will give estimated adjusted true mean in list.
#' For logistic regression gives count of outcome variable pr variable level.
#' @param meas binary outcome measure variable, column name in data.frame as a string. Can be numeric or factor. Result is calculated accordingly.
#' @param adj variables to adjust for, as string.
#' @param data dataframe of data.
#' @param dec decimals for results, standard is set to 2. Mean and sd is dec-1.
#' @param n.by.adj flag to indicate whether to count number of patients in adjusted model or overall for outcome measure not NA.
#' @param p.val flag to include p-values in table, set to FALSE as standard.
#' @keywords logistic
#' @export

print_pred<-function(meas,adj,data,dec=2,n.by.adj=FALSE,p.val=FALSE){

## Wish list:
  ## - SPEED, maybe flags to include/exclude time consuming tasks
  ## - Include ANOVA in output list, flag to include

  require(dplyr)

  d<-data
  m<-d[,c(meas)]

  ads<-d[,c(adj)]

  if(is.factor(m)){

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
      l<-round(ci[-1,1],dec)
      u<-round(ci[-1,2],dec)
      or<-round(exp(coef(mn))[-1],dec)
      or_ci<-paste0(or," (",l," to ",u,")")
      pv<-round(tidy(mn)$p.value[-1],dec+1)
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
    nall<-length(!is.na(dat[,1]))

    if (n.by.adj==TRUE){
      dat2<-ma$model
      # nalt<-nrow(dat2)
      for (i in 2:ncol(dat2)) {
        if (is.factor(dat2[, i])) {
          vec <- dat2[, i]
          ns <- names(dat2)[i]
          for (r in 1:length(levels(vec))) {
            vr <- levels(vec)[r]
            ## Counting all included in analysis
            n <- length(vec[vec == vr & !is.na(vec)])
            rt <- paste0(n, " (", round(n/nall * 100, 0), "%)")
            ## Counting all included in analysis with outcome
            lvl<-levels(dat2[,1])[2]
            no <- length(vec[vec == vr & dat2[,1]==lvl & !is.na(vec)])
            ro <- paste0(no, " (", round(no/n * 100, 0), "%)")
            ## Combining
            nq <- rbind(nq, cbind(paste0(ns, levels(vec)[r]), rt,ro))
          }
        }
        if (!is.factor(dat2[, i])) {
          num <- dat2[, i]
          n <- length(num[!is.na(num)])
          rt <- paste0(n, " (", round(n/nall * 100, 0), "%)")
          nq <- rbind(nq, cbind(names(dat2)[i], rt,ro="-"))
        }
      }
    }

    else {
      dat2<-dat[!is.na(dat[,1]),]
      for (i in 2:ncol(dat2)) {
        if (is.factor(dat2[, i])) {
          vec <- dat2[, i]
          ns <- names(dat2)[i]
          for (r in 1:length(levels(vec))) {
            vr <- levels(vec)[r]
            ## Counting all included in analysis
            n <- length(vec[vec == vr & !is.na(vec)])
            rt <- paste0(n, " (", round(n/nall * 100, 0), "%)")
            ## Counting all included in analysis with outcome
            lvl<-levels(dat2[,1])[2]
            no <- length(vec[vec == vr & dat2[,1]==lvl & !is.na(vec)])
            ro <- paste0(no, " (", round(no/n * 100, 0), "%)")
            ## Combining
            nq <- rbind(nq, cbind(paste0(ns, levels(vec)[r]), rt,ro))
          }
        }
        if (!is.factor(dat2[, i])) {
          num <- dat2[, i]
          n <- length(num[!is.na(num)])
          rt <- paste0(n, " (", round(n/nall * 100, 0), "%)")
          nq <- rbind(nq, cbind(names(dat2)[i], rt,ro="-"))
        }
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

    numb<-data.frame(names=nq[,1],N=nq[,2],N.out=nq[,3],stringsAsFactors = F)
    namt<-data.frame(names=tail(rnames,-3),stringsAsFactors = F)

    coll<-left_join(left_join(namt,numb,by="names"),rest,by="names")

    header<-data.frame(matrix(paste0("Chance of ",meas," is ",levels(m)[2]),ncol = ncol(coll)),stringsAsFactors = F)
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

    if (p.val==TRUE){
      ref<-data.frame(c(NA,rona),re[,"N"],re[,"N.out"],re[,"or_ci"],re[,"pv"],re[,"aor_ci"],re[,"apv"])

      names(ref)<-c("Variable",paste0("N=",nall),paste0("N, ",meas," is ",levels(m)[2]),"Crude OR (95 % CI)","p-value","Mutually adjusted OR (95 % CI)","A p-value")
    }
    else{
      ref<-data.frame(c(NA,rona),re[,"N"],re[,"N.out"],re[,"or_ci"],re[,"aor_ci"])

      names(ref)<-c("Variable",paste0("N=",nall),paste0("N, ",meas," is ",levels(m)[2]),"Crude OR (95 % CI)","Mutually adjusted OR (95 % CI)")
    }

    ls<-list(tbl=ref,miss,nall,nrow(d))
    names(ls)<-c("Printable table","Deleted due to missingness in adjusted analysis","Number of outcome observations","Length of dataframe")
  }

  if(!is.factor(m)){

    dfcr<-data.frame(matrix(NA,ncol = 3))
    names(dfcr)<-c("pred","dif_ci","pv")
    n.mn<-c()

    nref<-c()

    for(i in 1:ncol(ads)){
      dat<-data.frame(m=m,ads[,i])
      names(dat)<-c("m",names(ads)[i])
      mn<-lm(m~.,data=dat)
      n.mn<-c(n.mn,nrow(mn$model))

      suppressMessages(ci<-confint(mn))
      l<-round(ci[-1,1],dec)
      u<-round(ci[-1,2],dec)
      dif<-round(coef(mn)[-1],dec)
      dif_ci<-paste0(dif," (",l," to ",u,")")
      pv<-round(tidy(mn)$p.value[-1],dec+1)
      pv<-ifelse(pv<0.001,"<0.001",round(pv,3))
      pv <- ifelse(pv<=0.05|pv=="<0.001",paste0("*",pv),
                   ifelse(pv>0.05&pv<=0.1,paste0(".",pv),pv))

      x1<-ads[,i]

      if (is.factor(x1)){
        pred<-paste0(names(ads)[i],levels(x1)[-1])
      }

      else {
        pred<-names(ads)[i]
      }

      dfcr<-rbind(dfcr,cbind(pred,dif_ci,pv))
    }

    ## Mutually adjusted  ORs

    dat<-data.frame(m=m,ads)
    ma <- lm(m ~ ., data = dat)
    miss<-length(ma$na.action)


    actable <- coef(summary(ma))
    pa <- actable[,4]
    pa<-ifelse(pa<0.001,"<0.001",round(pa,3))
    pa <- ifelse(pa<=0.05|pa=="<0.001",paste0("*",pa),
                 ifelse(pa>0.05&pa<=0.1,paste0(".",pa),pa))

    apv<-pa[1:length(coef(ma))]

    aco<-round(coef(ma),dec)
    suppressMessages(aci<-round(confint(ma),dec))
    alo<-aci[,1]
    aup<-aci[,2]
    amean_ci<-paste0(aco," (",alo," to ",aup,")")

    mean_est<-amean_ci[[1]]


    nq<-c()
    nall<-length(!is.na(dat[,1]))

    if (n.by.adj==TRUE){
      dat2<-ma$model[,-1]
      # nalt<-nrow(dat2)
      for (i in 1:ncol(dat2)){
        if (is.factor(dat2[,i])){
          vec<-dat2[,i]
          ns<-names(dat2)[i]
          for (r in 1:length(levels(vec))){
            vr<-levels(vec)[r]
            n<-length(vec[vec==vr&!is.na(vec)])
            rt<-paste0(n," (",round(n/nall*100,0),"%)")
            nq<-rbind(nq,cbind(paste0(ns,levels(vec)[r]),rt))
          }}
        if (!is.factor(dat2[,i])){
          num<-dat2[,i]
          n<-as.numeric(length(num[!is.na(num)]))
          rt<-paste0(n," (",round(n/nall*100,0),"%)")
          nq<-rbind(nq,cbind(names(dat2)[i],rt))
        }}
    }

    else {
      dat2<-dat[!is.na(dat[,1]),][,-1]
      for (i in 1:ncol(dat2)) {
        if (is.factor(dat2[, i])) {
          vec <- dat2[, i]
          ns <- names(dat2)[i]
          for (r in 1:length(levels(vec))) {
            vr <- levels(vec)[r]
            n <- length(vec[vec == vr & !is.na(vec)])
            rt <- paste0(n, " (", round(n/nall * 100, 0), "%)")
            nq <- rbind(nq, cbind(paste0(ns, levels(vec)[r]), rt))
          }
        }
        if (!is.factor(dat2[, i])) {
          num <- dat2[, i]
          n <- length(num[!is.na(num)])
          rt <- paste0(n, " (", round(n/nall * 100, 0), "%)")
          nq <- rbind(nq, cbind(names(dat2)[i], rt))
        }
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
    res<-cbind(amean_ci,apv)
    rest<-data.frame(names=row.names(res),res,stringsAsFactors = F)

    numb<-data.frame(names=nq[,1],N=nq[,2],stringsAsFactors = F)
    namt<-data.frame(names=rnames,stringsAsFactors = F)

    coll<-left_join(left_join(namt,numb,by="names"),rest,by="names")

    header<-data.frame(matrix("Adjusted",ncol = ncol(coll)),stringsAsFactors = F)
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

    if (p.val==TRUE){
      ref<-data.frame(c(NA,rona),re[,2],re[,5],re[,6],re[,3],re[,4])

      names(ref)<-c("Variable",paste0("N=",nall),"Difference (95 % CI)","p-value","Mutually adjusted difference (95 % CI)","A p-value")
    }
    else{
      ref<-data.frame(c(NA,rona),re[,2],re[,5],re[,3])

      names(ref)<-c("Variable",paste0("N=",nall),"Difference (95 % CI)","Mutually adjusted difference (95 % CI)")
    }

    ls<-list(tbl=ref,miss,nall,nrow(d),mean_est)
    names(ls)<-c("Printable table","Deleted due to missingness in adjusted analysis","Number of outcome observations","Length of dataframe","Estimated true mean (95 % CI) in adjusted analysis")

  }

  return(ls)
}
