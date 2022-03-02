#' Replacing specific characters or string elements with gsub()
#'
#' Using gsub() to make repeated character string element replacements.
#' @param char.vec Vector of elements with to replace.
#' @param to.be.rep Vector of characters to replace.
#' @param replacement Vector of characters to replace with.
#' @keywords replace
#' @export
#' @examples
#'   n <- c("Se, en fraek raev", "Oesterland", "Aalborg", "Soester Aase", "Soendergaard")
#'   p <- c('Ae', 'ae', 'Oe', 'oe','Aa','aa')
#'   r <- c('Æ', 'æ', 'Ø', 'ø','Å','å')
#'   replace_char(n,p,r)

replace_char<-function(char.vec,to.be.rep,replacement){

  if (length(to.be.rep)!=length(replacement)) {stop("Replacement vectors are not the same length")}
  if (!is.vector(char.vec)) {stop("The provided data should be a vector")}

  n<-char.vec
  for(i in seq_along(n)) {
    s<-n[i]
    for (j in seq_along(to.be.rep)){
      s <- gsub(to.be.rep[j],replacement[j], s)
    }
    n[i]<-s
  }
  return(n)
}
