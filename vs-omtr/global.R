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