#' Formatting multiple columns as factor
#'
#' Depending on dply's contains()-function.
#' @param string Columnnames containg strings.
#' @param data Dataframe
#' @param levels Levels for all selected columns
#' @param labels labels for all selected columns
#' @keywords factor
#' @export
#' @examples
#' col_fact()


col_fact<-function(string,data,levels=NULL,labels=NULL){
## Defining factors for columns containing string (can be vector of multiple strings), based on dplyr.
## Factoring several columns with same levels or labels, these can be provided.

  require(dplyr)
  d<-data
  s<-string
  n<-c()

  for(i in 1:length(s)){
  n<-c(n,names(dplyr::select(d,dplyr::contains(s[i]))))
  }

  if (!is.null(levels)){
    for(i in 1:length(n)) {
    d[,n[i]]<-factor(d[,n[i]],levels=levels)}}

    if (!is.null(labels)){
      for(i in 1:length(n)) {
        d[,n[i]]<-factor(d[,n[i]],labels=labels)
  }}
      else
        for(i in 1:length(n)) {
          d[,n[i]]<-factor(d[,n[i]])}

  return(d)
}
