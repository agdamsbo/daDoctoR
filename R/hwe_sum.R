#' Summarising function of HWE calculation
#'
#' For easy printing.
#' @param a1 Allele 1.
#' @param a2 Allele 2.
#' @param f factor for grouping.
#' @keywords hardy-weinberg-equllibrium
#' @export
#' @examples
#' hwe_sum()

hwe_sum<-function(a1,a2,f){
## HWE summarising function, for several groups defined by factor f. Alleles are provided as vectors, a1 and a2.

source("https://raw.githubusercontent.com/agdamsbo/research/master/hwe_allele.R")
lst<-list()
df<-data.frame(cbind(a1,a2))
  for (i in 1:length(ls<-split(df,f))){
  
  grp<-names(ls)[i]
  
  obs<-data.frame(hwe_allele(ls[[i]][,1],ls[[i]][,2])[[c("observed.dist")]])
  
  pval<-round(hwe_allele(ls[[i]][,1],ls[[i]][,2])[[c("p.value")]],3)
  
  prnt<-paste0(obs[1,]," (",obs[2,],")")
  
  names(prnt)<-names(obs)
  
  lst<-list(lst,grp,obs.dist=obs,print=prnt,hwe.pv=pval)
  }
  
return(lst)
}
