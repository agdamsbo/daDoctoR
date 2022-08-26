#' Easy function for splitting numeric variable in quantiles
#'
#' Using base/stats functions cut() and quantile().
#' @param x Variable to cut.
#' @param groups Number of groups.
#' @param y alternative vector to draw quantile cuts from. Limits has to be within x. Default is NULL.
#' @param na.rm Remove NA's. Default is TRUE.
#' @param group.names Names of groups to split to. Default is NULL, giving intervals as names.
#' @param ordered.f Set resulting vector as ordered. Default is FALSE.
#' @param inc.outs Flag to include min(x) and max(x) as boarders in case of y!=NULL.
#' @keywords quantile
#' @export
#' @examples
#' aa <- as.numeric(sample(1:1000,2000,replace = TRUE))
#' x <- 1:450
#' y <- 6:750
#' summary(quantile_cut(aa,groups=4)) ## Cuts quartiles

quantile_cut<-function (x, groups,y=NULL, na.rm = TRUE, group.names = NULL, ordered.f = FALSE, inc.outs=FALSE,detail.lst=TRUE)
{
  if (!is.null(y)){
    q<-quantile(y, probs = seq(0, 1, 1/groups), na.rm = na.rm, names = TRUE, type = 7)
    if (inc.outs){ # Setting cut boardes to include outliers in x compared to y.
      q[1]<-min(x,na.rm = TRUE)
      q[length(q)]<-max(x,na.rm = TRUE)
    }
  }
  if (is.null(y)){
    q<-quantile(x, probs = seq(0, 1, 1/groups), na.rm = na.rm, names = TRUE, type = 7)

  }
  d<-cut(x, q, include.lowest = TRUE, labels = group.names,
         ordered_result = ordered.f)
  if (detail.lst){return(list(d,q))} else {return(d)}

}
