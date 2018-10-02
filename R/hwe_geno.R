#' HWE calculation by genotypes
#'
#' For easy calculation.
#' @param mm First count of genotype.
#' @keywords hardy-weinberg-equllibrium
#' @export
#' @examples
#' hwe_geno()

hwe_geno<-function(mm,mn,nn,mo,no,oo,alleles=2)
{
  ## x is the number of common homozygote, y the heterozygote and z the rare homozygote.
  
  if (alleles==2){
    ## Biallelic has three degrees of freedom
    df=1
    
    a<-sum(mm,mn,nn)*2
    b<-sum(mm,mn,nn)
    p<-(2*mm+mn)/a
    q<-(2*nn+mn)/a
    
    al_dist<-round(rbind(cbind(p*a,q*a),cbind(p*100,q*100)),1)
    al_names<-c("m","n")
    
    p_p<-round(p^2*b,3)
    p_q<-round(2*p*q*b,3)
    q_q<-round(q^2*b,3)
    
    obs=rbind(mm,mn,nn)
    exp=rbind(p_p, p_q, q_q)
    
    hwe<-data.frame(obs,exp)
    
    hwe$dev<-hwe$obs-hwe$exp
    hwe$chi<-hwe$dev^2/hwe$exp
    
    snp_obs<-round(rbind(hwe$obs,hwe$obs/sum(hwe$obs)*100),1)
    
    snp_exp<-round(rbind(hwe$exp,hwe$exp/b*100),1)
    
    gen_names<-c("mm","mn","nn")
    
    chi<-sum(hwe$chi,na.rm = TRUE)
    
    p_v<-pchisq(chi, df=df, lower.tail=FALSE)
  }
  if(alleles==3){
    ## Triallelic has three degrees of freedom
    df=3
    
    a<-sum(mm,mn,nn,mo,no,oo)*2
    b<-sum(mm,mn,nn,mo,no,oo)
    p<-(2*mm+mn+mo)/a
    q<-(2*nn+mn+no)/a
    r<-(2*oo+no+mo)/a
    
    al_dist<-round(rbind(cbind(p*a,q*a,r*a),cbind(p*100,q*100,r*100)),1)
    al_names<-c("m","n","o")
    
    p_p<-round(p^2*b,3)
    p_q<-round(2*p*q*b,3)
    q_q<-round(q^2*b,3)
    p_r<-round(2*r*p*b,3)
    q_r<-round(2*q*r*b,3)
    r_r<-round(r^2*b,3)
    
    obs=rbind(mm,mn,nn,mo,no,oo)
    exp=rbind(p_p, p_q, q_q, p_r, q_r, r_r)
    
    hwe<-data.frame(obs,exp)
    
    hwe$dev<-hwe$obs-hwe$exp
    hwe$chi<-hwe$dev^2/hwe$exp
    
    snp_obs<-round(rbind(hwe$obs,hwe$obs/sum(hwe$obs)*100),1)
    
    snp_exp<-round(rbind(hwe$exp,hwe$exp/b*100),1)
    
    gen_names<-c("mm","mn","nn", "mo", "no", "oo")
    
    chi<-sum(hwe$chi,na.rm = TRUE)
    
    p_v<-pchisq(chi, df=df, lower.tail=FALSE)    
  }
  
  
  colnames(al_dist)<-al_names
  colnames(snp_obs)<-gen_names
  colnames(snp_exp)<-gen_names
  
  rownames(al_dist)<-c("N","%")
  rownames(snp_obs)<-c("N","%")
  rownames(snp_exp)<-c("N","%")
  
  int<-ifelse(p_v<=0.05,"The null-hypothesis of difference from the HWE can be confirmed","The null-hypothesis of difference from the HWE can be rejected")
  
  t1<-"Chi-square test for Hardy-Weinberg equillibrium for a bi- or triallellic system. Theory: http://www.husdyr.kvl.dk/htm/kc/popgen/genetics/2/2.htm"
  
  list<-list(info=t1,n.total=b,allele.dist=al_dist,observed.dist=snp_obs,expected.dist=snp_exp,chi.value=chi,p.value=p_v,df=df,interpretation=int)
  
  return(list)
}