#' Extracting date of birth from CPR
#'
#' For easy calculation.
#' @param cpr cpr-numbers as ddmmyy[-.]xxxx or ddmmyyxxxx. Also mixed formatting.
#' @keywords cpr
#' @export
#' @examples
#' dob_extract_cpr("231045-0637")


dob_extract_cpr<-function(cpr)
## Input as cpr-numbers in format ddmmyy-xxxx
## Build upon data from this document: https://cpr.dk/media/167692/personnummeret%20i%20cpr.pdf
## example vector: fsd<-c("010190-2000", "010115-4000", "0101896000","010189-3000","300450-1030","010150-4021")
## cpr <- "231045-0637"
## cpr <- "2310450637"
  {

  dobs<-c()

  a00<-as.numeric(c(0:99))
  a36<-as.numeric(c(0:36))
  a57<-as.numeric(c(0:57))
  b00<-as.numeric(c(0,1,2,3))
  b36<-as.numeric(c(4,9))
  b57<-as.numeric(c(5,6,7,8))

  for (x in cpr)
  {
  p56<-as.numeric(substr(x,5,6))

  if (substr(x,7,7)%in%c("-",".")){
    p8<-as.numeric(substr(x,8,8))           # Added check to take p8 if ddmmyy[-.]xxxx,
  } else {p8<-as.numeric(substr(x,7,7))}    # or p7 if ddmmyyxxxx

  birth<-as.Date(substr(x,1,6),format="%d%m%y")


  if (((p56%in%a00)&&(p8%in%b00)))
    {
    dob<-as.Date(format(birth, format="19%y%m%d"), format="%Y%m%d")
    }
  else if (((p56%in%a36)&&(p8%in%b36)))
    {
    dob<-as.Date(format(birth, format="20%y%m%d"), format="%Y%m%d")
    }
  else if ((!(p56%in%a36)&&(p8%in%b36)))
    {
    dob<-as.Date(format(birth, format="19%y%m%d"), format="%Y%m%d")
    }
  else if (((p56%in%a57)&&(p8%in%b57)))
    {
    dob<-as.Date(format(birth, format="20%y%m%d"), format="%Y%m%d")
    }
  else if ((!(p56%in%a57)&&(p8%in%b57)))
    {
    dob<-as.Date(format(birth, format="18%y%m%d"), format="%Y%m%d")
    }
  else {print("Input contains data in wrong format") # test if position 5,6 or 8 contains letters as is the case for temporary cpr-numbers
    }
  dobs<-append(dobs,dob)

  }

  return(dobs)

}
