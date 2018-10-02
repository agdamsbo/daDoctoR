#' HWE calculation by alleles
#'
#' For easy calculation.
#' @param x Allele 1.
#' @param y Allele 2.
#' @keywords hardy-weinberg-equllibrium
#' @export
#' @examples
#' hwe_allele()

hwe_allele<-function(x,y)
{
## Witten by Andreas Gammelgaard Damsbo, agdamsbo@pm.me, based on a non-working
## applet at from http://www.husdyr.kvl.dk/htm/kc/popgen/genetik/applets/kitest.htm
  
  all<-pmax(length(levels(factor(x))),length(levels(factor(y))))
  
  if(all==2){
    df=1
    ## Biallellic system, df=1
    al1<-factor(x,labels=c("p","q"))
    al2<-factor(y,labels=c("p","q"))
    
    snp<-paste(al1,al2,sep = "_")
    snp[snp=="q_p"]<-"p_q"
    
    snp_f<-factor(snp,levels=c("p_p", "p_q", "q_q"))
    
    a<-length(snp)*2
    b<-length(snp)
    p<-(length(al1[al1=="p"])+length(al2[al2=="p"]))/a
    q<-(length(al1[al1=="q"])+length(al2[al2=="q"]))/a
    
    al_dist<-round(rbind(cbind(p*a,q*a),cbind(p*100,q*100)),1)
    al_names<-levels(factor(x))
    
    p_p<-round(p^2*b,3)
    p_q<-round(2*p*q*b,3)
    q_q<-round(q^2*b,3)
    
    hwe<-data.frame(obs=summary(snp_f),exp=rbind(p_p, p_q, q_q))
    
    hwe$dev<-hwe$obs-hwe$exp
    hwe$chi<-hwe$dev^2/hwe$exp
    
    snp_obs<-round(rbind(summary(snp_f),summary(snp_f)/length(snp_f)*100),1)
    
    snp_exp<-round(rbind(hwe$exp,hwe$exp/b*100),1)
    
    gen_names<-c(
      paste(levels(factor(x))[1],levels(factor(x))[1],sep="_"),
      paste(levels(factor(x))[1],levels(factor(x))[2],sep="_"),
      paste(levels(factor(x))[2],levels(factor(x))[2],sep="_"))
    
    chi<-sum(hwe$chi,na.rm = TRUE)
    p_v<-pchisq(chi, df=df, lower.tail=FALSE)
  }
  else if(all==3){
    df=3
    ## Triallellic system, df=3
    al1<-factor(x,labels=c("p","q","r"))
    al2<-factor(y,labels=c("p","q","r"))
    
    snp<-paste(al1,al2,sep = "_")
    snp[snp=="r_p"]<-"p_r"
    snp[snp=="r_q"]<-"q_r"
    snp[snp=="q_p"]<-"p_q"
    
    snp_f<-factor(snp,levels=c("p_p", "p_q", "q_q","p_r","q_r", "r_r"))
    
    a<-length(snp)*2
    b<-length(snp)
    p<-(length(al1[al1=="p"])+length(al2[al2=="p"]))/a
    q<-(length(al1[al1=="q"])+length(al2[al2=="q"]))/a
    r<-(length(al1[al1=="r"])+length(al2[al2=="r"]))/a
    
    al_dist<-round(rbind(cbind(p*a,q*a,r*a),cbind(p*100,q*100,r*100)),1)
    al_names<-levels(factor(x))
    
    p_p<-round(p^2*b,3)
    p_q<-round(2*p*q*b,3)
    q_q<-round(q^2*b,3)
    p_r<-round(2*r*p*b,3)
    q_r<-round(2*q*r*b,3)
    r_r<-round(r^2*b,3)
    
    hwe<-data.frame(obs=summary(snp_f),exp=rbind(p_p, p_q, q_q, p_r, q_r, r_r))
    
    hwe$dev<-hwe$obs-hwe$exp
    hwe$chi<-hwe$dev^2/hwe$exp
    
    snp_obs<-round(rbind(summary(snp_f),summary(snp_f)/length(snp_f)*100),1)
    
    snp_exp<-round(rbind(hwe$exp,hwe$exp/b*100),1)
    
    gen_names<-c(
      paste(levels(factor(x))[1],levels(factor(x))[1],sep="_"),
      paste(levels(factor(x))[1],levels(factor(x))[2],sep="_"),
      paste(levels(factor(x))[2],levels(factor(x))[2],sep="_"),
      paste(levels(factor(x))[1],levels(factor(x))[3],sep="_"),
      paste(levels(factor(x))[2],levels(factor(x))[3],sep="_"),
      paste(levels(factor(x))[3],levels(factor(x))[3],sep="_"))
    
    chi<-sum(hwe$chi,na.rm = TRUE)
    p_v<-pchisq(chi, df=df, lower.tail=FALSE)
    
  }
  else if (!any(all==c(2,3))){stop("This formula only works for bi- or triallellic systems")}
    
  else {stop("There was an unknown error")}
  
  colnames(al_dist)<-al_names
  colnames(snp_obs)<-gen_names
  colnames(snp_exp)<-gen_names
  
  rn<-c("N","%")
  
  rownames(al_dist)<-rn
  rownames(snp_obs)<-rn
  rownames(snp_exp)<-rn
  
  int<-ifelse(p_v<=0.05,"The null-hypothesis of difference from the HWE can be confirmed","The null-hypothesis of difference from the HWE can be rejected")
  
  t1<-"Chi-square test for Hardy-Weinberg equillibrium for a bi- or triallellic system. Read more: http://www.husdyr.kvl.dk/htm/kc/popgen/genetics/2/2.htm"
  
  list<-list(info=t1,n.total=b,allele.dist=al_dist,observed.dist=snp_obs,expected.dist=snp_exp,chi.value=chi,p.value=p_v,df=df,interpretation=int)
  
  return(list)
}
