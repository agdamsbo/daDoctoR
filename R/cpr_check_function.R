#' CPR check
#'
#' Checking validity of cpr number
#' @param cpr cpr-numbers as ddmmyy[-.]xxxx or ddmmyyxxxx. Also mixed formatting.
#' @keywords cpr
#' @export
#' @examples
#' cpr_check("231045-0637")

cpr_check<-function(cpr){
  # Check validity of CPR number, format ddmmyy-xxxx
  # Build upon data from this document: https://cpr.dk/media/167692/personnummeret%20i%20cpr.pdf
  # example vector: fsd<-c("2310450637", "010115-4000", "0101896000","010189-3000","300450-1030","010150-4021")

  v <- c()

  for (x in cpr){
  if (!substr(x,7,7)%in%c("-",".")){ # Added check to take p8 if ddmmyy[-.]xxxx,
    x<-paste(substr(x,1,6),substr(x,7,10),collapse="-")
  }

  p1<-as.integer(substr(x,1,1))
  p2<-as.integer(substr(x,2,2))
  p3<-as.integer(substr(x,3,3))
  p4<-as.integer(substr(x,4,4))
  p5<-as.integer(substr(x,5,5))
  p6<-as.integer(substr(x,6,6))
  p7<-as.integer(substr(x,8,8))
  p8<-as.integer(substr(x,9,9))
  p9<-as.integer(substr(x,10,10))
  p10<-as.integer(substr(x,11,11))

  v<-c(v,
       ifelse((p1*4+p2*3+p3*2+p4*7+p5*6+p6*5+p7*4+p8*3+p9*2+p10) %% 11 == 0,"valid","invalid")
       )
  }

  return(v)
}
