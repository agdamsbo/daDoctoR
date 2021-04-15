#' Easily format csv-files for instrument opload to REDCap
#'
#' Both Numbers and Excel lacks detailed formatting options for csv-export.
#' This function formats csv-files for ReDCap instruments after export from said programs.
#' @param folder Path to desired folder.
#' @param name Desired file name.
#' @keywords redcap
#' @export
#' @examples
#' csv_redcap_cleaner("/Users/user/REDCap_conversion")

redcap_clean_csv<-function(folder,name="instrument"){
  f<-folder
  fn<-name
  pt<-paste0(name,".csv")

  p<-list.files(f, pattern=pt, full.names=TRUE)
  d<-read.csv(p, header=FALSE, sep=";")
  nc<-unlist(strsplit(p, "[.]"))
  n<-paste0(f,"/done/",fn,".csv")
  colnames(d)<-NULL
  write.csv(d,n,na="",row.names = FALSE)
}
