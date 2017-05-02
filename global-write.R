#IVA 20.4.17
#install.packages("pheno")
#install.packages('pheno', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages("modiscloud")
#install.packages("StreamMetabolism")
require(pheno)
require(lubridate)
require(modiscloud)
require(StreamMetabolism)

  as.data.frame.ymd <- function (Y, trace.ymd = FALSE) {
    stopifnot(is.numeric(Y), Y > 1800, Y < 2018)
    if (leapyear(Y)==TRUE)  jul <- 1:366
    else jul <- 1:365 
    M <- matrix(rep(1, length(jul)*3), ncol = 3)
    for (i in jul){ # Converts Julian date to integers day, month, year
      ymd <- jul2date2(i, Y)
      M[i, 1] <- ymd$day; M[i, 2] <- ymd$month; M[i, 3] <- ymd$year
      if (trace.ymd)
        cat(i, ymd$day, ymd$month, ymd$year, "\n")
    }  
    return(as.data.frame(M))
  }

if (FALSE) {
  summary(as.data.frame.ymd(2016, trace.ymd = F))
  Y <- 1904
  if (leapyear(Y)==TRUE) {
    jul <- 1:366
  } else { jul <- 1:365 } 
  
  for (i in jul){
    # Converts Julian date to integers day,month,year
    ymd <- jul2date2(i,Y)
    jul2 <- yearmonthday_to_julianday(ymd$year, ymd$month, ymd$day)
    cat(i, ymd$day, ymd$month, ymd$year, jul2, "\n")
  }  
}

if (FALSE) {
  #https://www.rdocumentation.org/packages/StreamMetabolism/versions/1.1.2/topics/sunrise.set
  #https://rdrr.io/cran/RAtmosphere/man/suncalc.html
  #https://rdrr.io/cran/rwunderground/man/astronomy.html
  
  #This is for Atlanta Georgia 
  #(Only so that you can compare it to the NOAA 
  #website that is given above)
  #sunrise.set(33.43, -84.22, "2008/01/01", timezone="UTC+5") 	
  
  #Same As above but look at Time Zone Specification
  sunrise.set(33.43, -84.22, "2008/01/01", timezone="America/New_York")
  
  S<-sunrise.set(56.01839, 92.86717, "2017/04/21", timezone = "Asia/Krasnoyarsk")
  D<-S$sunset-S$sunrise
  
}
