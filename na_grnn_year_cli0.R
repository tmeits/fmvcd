# 22.3.17 IVA
# http://stackoverflow.com/questions/18695335/replacing-all-nas-with-smoothing-spline
# install.packages('grnn', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('grt', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('neuralnet', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('caret', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('lubridate', dependencies=TRUE, repos='http://cran.rstudio.com/')
require(zoo)
require(grnn)
require(grt)
require(lubridate)
require(foreach)

is.leapyear=function(year){
  #http://en.wikipedia.org/wiki/Leap_year
  #https://www.r-bloggers.com/leap-years/
  #https://github.com/hadley/lubridate/blob/master/R/leap-years.r
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
}

as.zero.negative <- function(vec){
  return(sapply(vec, setZeroNegativeNumber))
}

createCliDF <- function(fileName){
  cli_dataset <-read.table(fileName, header=FALSE, sep="")
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  cli_dataset[cli_dataset$prec == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4] 
  cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]
  days <- c(1:dim(cli_dataset)[1])
  return(data.frame(days=days, prec=prec_vector, temp=temp_vector))
}

# Replacement numbers-9999 to missing value NA
replacementNumbersMinus9999.NA <- function(cli_dataset){
  for(i in 1:nrow(cli_dataset)){
    if(!is.na(cli_dataset[i,2])){
      if(cli_dataset[i,2]==-9999) cli_dataset[i,2]<-NA
    }
    if(!is.na(cli_dataset[i,3])) {
      if(cli_dataset[i,3]==-9999) cli_dataset[i,3]<-NA
    }
  }
  return(cli_dataset)
}

plotTempFilled <- function(naDF, filledDF, fileName){
  plot(filledDF[,3], main=fileName, ylab="Temp(c)*10", xlab="Days", col="red", pch=19); 
  points(naDF[,3], col="blue", pch=19)
  grid (10,10, lty = 6, col = "cornsilk2")
  points(filledDF[,3], col="red", pch=19); 
  points(naDF[,3], col="blue", pch=19)
  # Add a legend
  legend("topleft", legend=c("Guess CLI", "Real CLI"),  col=c("red", "blue"), pch=c(19,19), cex=0.8)
}

plotPrecFilled <- function(naDF, filledDF, fileName){
  plot(filledDF[,2], main=fileName, ylab="Prec(mm)*10", xlab="Days", col="red", pch=19); 
  points(naDF[,2], col="blue", pch=19)
  grid (10,10, lty = 6, col = "cornsilk2")
  points(filledDF[,2], col="red", pch=19); 
  points(naDF[,2], col="blue", pch=19)
  # Add a legend
  legend("topleft", legend=c("Guess CLI", "Real CLI"),  col=c("red", "blue"), pch=c(19,19), cex=0.8)
}

na.grnn <- function(vec.cli, setTrain, setSigma, searchSigma, trace, seePlot){
  # PRE-PROCESSING DATA
  vec.na<-vec.cli; vec.na.scale <- grt::scale(vec.na);  
  vec.na.scale.min<-min(vec.na.scale, na.rm = T); vec.na.scale.max<-max(vec.na.scale, na.rm = T); 
  vec.na.index <- which(is.na(vec.na)) #
  vec.na.scale.index <- which(is.na(vec.na.scale)) # 
  vec.na.scale.na.omit <- na.omit(vec.na.scale);
  days <- 1:length(vec.na); days.scale <- grt::scale(days) # для прогноза
  days.scale.na.omit <- days.scale[-vec.na.scale.index]; # base::scale(days)
  XY <- data.frame(days.scale.na.omit, vec.na.scale.na.omit)
  if(searchSigma == TRUE){
## SPLIT DATA SAMPLES  
    #seed.init<-2017; set.seed(seed.init)
    num.test<-setTrain; 
    rows <- sample(1:nrow(XY), nrow(XY) - num.test)
    trainXY <- XY[rows, ]; testXY <- XY[-rows, ]
    s.seq <- seq(0.01, 0.99, 0.05)
    cv <- foreach(s = s.seq) %do% {
      L <- grnn::learn(trainXY, variable.column = ncol(trainXY))
      grnn <- grnn::smooth(L, sigma = s)
      guessY <- testXY[, 2]
      for(i in 1:length(testXY[, 1])){
        guessY[i] <- grnn::guess(grnn, testXY[i, 1])
      }
      test.sse <- round(sum((guessY-testXY[, 2]))^2, 6)
      if(trace == TRUE){
        cat("Sigma= ", round(s,3), "SSE= ", test.sse, "\n")
      }
      data.frame(s, sse = test.sse)
    # http://r-train.ru/r-%D0%BA%D0%B2%D0%B0%D0%B4%D1%80%D0%B0%D1%82-rse-%D0%B8-rmse/
    }
    idx<-1; val<-cv[[1]][1,2]
    for(i in 2: length(s.seq)){
      if(cv[[i]][1,2] < val){
        idx <- i
        val <- cv[[i]][1,2]
      }
    }
    idx; val;
  ## predicting missing values with the calculated optimum Sigma GRNN
    s<-cv[[idx]][1,1]
  } else s <- setSigma  
  L <- grnn::learn(XY, variable.column = ncol(XY))
  grnn <- grnn::smooth(L, sigma = s)
  for(i in vec.na.index){
    #todo поставить проверку выхода за min max
    vec.na.scale[i] <- grnn::guess(grnn, days.scale[i, 1])
    #cat(i, vec.na.scale[i],"\n")
  }
    
  vec.na.unscale <- grt::unscale(vec.na.scale)
  if (seePlot == TRUE){
    plot(vec.na.unscale, col="red"); points(vec.na, col="blue"); 
  }
  return(vec.na.unscale)
} #end na.grnn

na.grnn.cli <- function(cli_dataset, setTrain, setSigma,  searchSigma, trace, seePlot){
  cli_dataset<-replacementNumbersMinus9999.NA(cli_dataset) 
  prec_vector <- cli_dataset[, 2]
  temp_vector <- cli_dataset[, 3]
  #days <- length(cli_dataset[,1])
  days <- c(1:dim(cli_dataset)[1])
  if(sum(is.na(prec_vector)) != 0){
    prec_filled <- sapply(as.integer(na.grnn(prec_vector, setTrain, setSigma,  searchSigma, trace, seePlot)), setZeroNegativeNumber)
  } else prec_filled <- prec_vector
  if(sum(is.na(temp_vector)) != 0){
    #temp_filled <- as.integer(na.spline(temp_vector))
    temp_filled <- as.integer(na.grnn(temp_vector, setTrain, setSigma,  searchSigma, trace, seePlot))
  } else temp_filled <- temp_vector
  print(length(cli_dataset[, 1]))
  print(length(prec_filled))
  print(length(temp_filled))
  new_cli <- data.frame(day = cli_dataset[, 1], prec=prec_filled, temp=temp_filled)
  return(new_cli)
}

na.grnn.cli.test <- function(fileName, setTrain, setSigma,  searchSigma, trace, seePlot){
  filled_path <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year"
  #filled_path <- "/home/larisa/Dropbox/Apps/na_grnn_year"
  setwd(filled_path); getwd(); file_name <- fileName
  #station.df<-read.csv("master-location-identifier-database-20130801.csv")
  #kara.turek <- read.table("SCH31.txt", header = FALSE, sep=";")
  #plot(kara.turek[, 8], col="red")
  # http://koldunov.net/?p=920
  #
  cli_dataset <- createCliDF(file_name) 
  summary(cli_dataset)
  #system.time(na.na<-na.grnn(cli_dataset[, 3]))
  #user  system elapsed Atom home
  #23.52    2.03   32.03
  #пользователь      система       прошло 
  #3.43         0.00         3.81
  cli_dataset_filled <- na.grnn.cli(cli_dataset, setTrain, setSigma,  searchSigma, trace, seePlot)
  plotTempFilled(cli_dataset, cli_dataset_filled, file_name)
  #plotPrecFilled(cli_dataset, cli_dataset_filled, file_name)
}

kara_turek.convert <- function(){
   filled_path <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year"
   #filled_path <- "/home/larisa/Dropbox/Apps/na_grnn_year"   
   setwd(filled_path); getwd();
   # Read datasets
   # Work with weather data <http://aisori.meteo.ru/ClimateR> [R]
   kara_turek.cli <- read.csv(paste0(filled_path, "/cli/36442_kara-turek/SCH31.txt"), header = FALSE, sep = ";", dec = ".")  # read Climate
   kara_turek.cli <- kara_turek.cli[-c(5, 7, 9, 11, 13, 14)]  # we delete excess columns
   kara_turek.cli <- setNames(kara_turek.cli, c("Station", "Year", "Month", "Day", "TMIN", "TMEAN", "TMAX", "PRECIP")) # Assign columns names
}
kara_turek_cli <- kara_turek.convert()

# Reading climatic data in one year
get_one_year <- function(years, now) {
    one_year <- years[years$Year == now, ]
    days <- c(1:dim(one_year)[1])
    return(data.frame(days=days, 
      prec=as.integer(one_year$PRECIP*10), 
      temp=as.integer(one_year$TMEAN*10)))
}

#Reading climatic data in one year. Format VSO
get_one_year_vso <- function(years, now) {
  one_year <- years[years$Year == now, ]
  cli_dataset <- data.frame(
    one_year[,4], one_year[,3], one_year[,2], as.integer(one_year$PRECIP*10), as.integer(one_year$TMEAN*10)
  )
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  return(cli_dataset)
}
 
#Add vectors with filled gaps in temperature and precipitation
vectors.filled.replace <- function(year.na, year.filled) {
  cli_dataset <- data.frame(
    year.na[,1], year.na[,2], year.na[,3], year.filled$prec, year.filled$temp
  )
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  return(cli_dataset)
}
#Record one year climate data in the format model Vaganova Shashkina
write.table.vso <- function(year.cli, filled_path, file_name){
  write.table(year.cli, file = paste(filled_path,file_name, sep=""),
              row.names=FALSE, col.names=FALSE, sep="\t")
}

#
kara_turek_cli <- kara_turek.convert()
one.year.1983 <- get_one_year(kara_turek_cli, 1983) # 12
plot(one.year.1983)
str(one.year.1983)
head(one.year.1983); tail(one.year.1983)
summary(one.year.1983)
one.year.1983.new <- na.grnn.cli(one.year.1983, 157, 0.01, TRUE, TRUE, FALSE)
plotPrecFilled(one.year.1983, one.year.1983.new, "36442, kara-turek, 1983 year")
plotTempFilled(one.year.1983, one.year.1983.new, "36442, kara-turek, 1983 year")
one.year.vso <- get_one_year_vso(kara_turek_cli, 1983)
str(one.year.vso)
summary(one.year.vso)
one.year.vso <- vectors.filled.replace(one.year.vso, one.year.1983.new)
summary(one.year.vso)
write.table.vso(one.year.vso, filled_path, "/1983.cli")

#
filled_path <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year"
#filled_path <- "/home/larisa/Dropbox/Apps/na_grnn_year"   
setwd(filled_path); getwd();
one.year.1986 <- get_one_year(kara_turek_cli, 1986) # 12
plot(one.year.1986)
str(one.year.1986)
head(one.year.1986); tail(one.year.1986,40)
print(summary(one.year.1986))
one.year.1986.new <- na.grnn.cli(one.year.1986, 67, 0.01, TRUE, TRUE, FALSE)
print(summary(one.year.1986.new))
plotPrecFilled(one.year.1986, one.year.1986.new, "36442, kara-turek, 1986 year")
plotTempFilled(one.year.1986, one.year.1986.new, "36442, kara-turek, 1986 year")

one.year.vso <- get_one_year_vso(kara_turek_cli, 1986)
str(one.year.vso)
summary(one.year.vso)
one.year.vso <- vectors.filled.replace(one.year.vso, one.year.1986.new)
summary(one.year.vso)
write.table.vso(one.year.vso, filled_path, "/1986.cli")

#ivan 28.3.17
# http://meteoinfo.ru/klimatgorod
read.cvs.aisori <- function(file.name){
  # Work with weather data <http://aisori.meteo.ru/ClimateR> [R]
  df.cli <- read.csv(file.name, header = FALSE, 
                     # sep=";", #sep = " ", # sep = "\t"
                     dec = ".") 
  df.cli <- df.cli[-c(5, 7, 9, 11, 13, 14)]  # we delete excess columns
  df.cli <- setNames(df.cli, c("Station", "Year", "Month", "Day", "TMIN", "TMEAN", "TMAX", "PRECIP")) # Assign columns names
  return(df.cli)
}
filled_path <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year"
#filled_path <- "/home/larisa/Dropbox/Apps/na_grnn_year" 
file.path <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year/cli/24343_Zhigansk_66-46N_123-24E/"
setwd(file.path); getwd();
file.name <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year/cli/24343_Zhigansk_66-46N_123-24E/Zhigansk.txt"
df.cli.Zhigansk <- read.cvs.aisori(file.name)
str(df.cli.Zhigansk)
summary(df.cli.Zhigansk)
# Unique values of the vector
unique(df.cli.Zhigansk$Year) == c(min(df.cli.Zhigansk$Year):max(df.cli.Zhigansk$Year))

file.name <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year/cli/21921_Kjusjur_70-41N_127-24E/Kjusjur.txt"
df.cli.Kjusjur <- read.cvs.aisori(file.name)
str(df.cli.Kjusjur)
summary(df.cli.Kjusjur)
unique(df.cli.Kjusjur$Year) == c(min(df.cli.Kjusjur$Year):max(df.cli.Kjusjur$Year))
# Unique values of the vector


file.name.Khatyryk <- "C:/Users/IVA/Dropbox/Apps/na_grnn_year/cli/24643_Khatyryk-Khoma_63N_124E/24643_Khatyryk-Khoma.1957-2015.aisori.csv"
df.cli.Khatyryk <-  read.csv(file.name.Khatyryk, header = TRUE, sep = ";", dec = ",")  # read Climate
str(df.cli.Khatyryk)
summary(df.cli.Khatyryk)
df.cli.Khatyryk.period <- df.cli.Khatyryk[df.cli.Khatyryk$Year > 1957, ]
summary(df.cli.Khatyryk.period)
df.cli.Khatyryk.prec <- df.cli.Khatyryk.period$PRECIP
plot(df.cli.Khatyryk.prec , col="blue")
cli.Khatyryk.prec.na <- which(is.na(df.cli.Khatyryk.prec)) # index in vector
cli.Khatyryk.prec.zoo <- as.zero.negative(zoo::na.spline(df.cli.Khatyryk.prec))
summary(cli.Khatyryk.prec.zoo)

for(i in 1:length(cli.Khatyryk.prec.na)){
  points(cli.Khatyryk.prec.na[i], cli.Khatyryk.prec.zoo[cli.Khatyryk.prec.na[i]], col="red", pch=19)
}

cli.Khatyryk.temp <- df.cli.Khatyryk.period$TMEAN
plot(cli.Khatyryk.temp, col="blue")
cli.Khatyryk.temp.na <- which(is.na(cli.Khatyryk.temp))
cli.Khatyryk.temp.zoo <- zoo::na.spline(cli.Khatyryk.temp)
summary(cli.Khatyryk.temp.zoo)

for(i in 1:length(cli.Khatyryk.temp.na)){
  points(cli.Khatyryk.temp.na[i], cli.Khatyryk.temp.zoo[cli.Khatyryk.temp.na[i]], col="red", pch=19)
}

cli.Khatyryk.Ivan <- df.cli.Khatyryk.period[, c(1,2,3,4)]
str(cli.Khatyryk.Ivan)
cli.Khatyryk.Ivan <- data.frame(df.cli.Khatyryk.period[, c(1,2,3,4)], 
                                Temp= round(cli.Khatyryk.temp.zoo,2),
                                Prec= round(cli.Khatyryk.prec.zoo,2))
#install.packages('pacman', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('openxlsx', dependencies=TRUE, repos='http://cran.rstudio.com/')
require(openxlsx)
Sys.setenv(R_ZIPCMD = paste0("C:/Users/IVA/Dropbox/Apps/na_grnn_year", "/bin/zip.exe"))  ## path to zip.exe
openxlsx::write.xlsx(cli.Khatyryk.Ivan, 
                     file = "C:/Users/IVA/Dropbox/Apps/na_grnn_year/cli/24643_Khatyryk-Khoma_63N_124E/24643_Khatyryk-Khoma.1957-2015.aisori.xlsx")


# creating a DataSet with a missing year
creatingDataSetMissingYear <- function(data.set.cli){
  
}

### TEST SCRIPT
if(FALSE){
   save(one.year.1986.new ,file="one.year.1986-1.new.Rdata")
   one.year.1986.new.pre <- one.year.1986.new
   save(one.year.1986.new.pre ,file="one.year.1986-pre.new.Rdata")
   # https://gist.githubusercontent.com/anonymous/75b0aa2a79091549247e6596f989712e/raw/def3b6967dc8c1a527a0472012cf57a44b1b75ab/1967.cli
   na.grnn.cli.test("1967na.cli", 67, 0.01, TRUE, TRUE)
   na.grnn.cli.test("1967na.cli", 67, 0.01, TRUE, FALSE)
}


















