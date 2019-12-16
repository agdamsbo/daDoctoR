#' Substring n last characters
#'
#'
#' @param x vector
#' @param n number of string elements to
#' @keywords substr
#' @export
#' @examples
#'   ##Kim Larsen
#'   cpr<-"231045-0637"
#'   substrRight(cpr,4)

substrRight <- function(x, n){
  r<-nchar(x)
  substr(x, r-n+1, r)
}
