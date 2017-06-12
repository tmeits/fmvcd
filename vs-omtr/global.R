# 8.6.17 IVA
is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}
number.days.year <- function (year){
  if (is.leapyear(year)) 
    return(rep.int(0, 366))
  else 
    return(rep.int(0, 365)) 
}
ymd2date <- function(cDate) {
  sD <- paste(cDate[1], "-", cDate[2], "-", cDate[3], sep = "") #; print(sD) 
  return(as.Date(sD)) # The wrong date throws an exception
}
dmy2date <- function(cDate) {
  sD <- paste(cDate[3], "-", cDate[2], "-", cDate[1], sep = "") #; print(sD) 
  return(as.Date(sD)) # The wrong date throws an exception
}
