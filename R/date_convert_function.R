#' Converting data format
#'
#' Input format as dd/mm/yyyy, output is standard yyyy-mm-dd
#' @param x data as as dd/mm/yyyy.
#' @keywords date
#' @export

date_convert<-function(x)
  ## Input format as dd/mm/yyyy, output is standard yyyy-mm-dd
  {
      result<-as.Date(x, format="%d/%m/%Y")
print(result)
}
