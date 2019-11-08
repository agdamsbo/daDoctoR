#' Summarising function of HWE calculation
#'
#' For easy printing.
#' @param a1 Allele 1.
#' @param a2 Allele 2.
#' @param f factor for grouping.
#' @keywords hardy-weinberg-equllibrium
#' @export

hwe_sum<-function (a1, a2, f) {
  require(daDoctoR)
  lst <- list()
  df <- data.frame(a1, a2)

  for (i in 1:length(ls <- split(df, f))) {
    grp <- names(ls)[i]
    obs <- data.frame(hwe_allele(ls[[i]][, 1], ls[[i]][,2])[[c("observed.dist")]])
    pval <- round(hwe_allele(ls[[i]][, 1], ls[[i]][, 2])[[c("p.value")]],3)
    prnt <- paste0(obs[1, ], " (", obs[2, ], ")")
    names(prnt) <- names(obs)
    lst <- list(lst, grp, obs.dist = obs, print = prnt,hwe.pv = pval)
  }
  return(lst)
}
