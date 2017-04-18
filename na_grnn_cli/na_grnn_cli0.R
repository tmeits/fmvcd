# 21.3.17 IVA
# http://stackoverflow.com/questions/18695335/replacing-all-nas-with-smoothing-spline
# install.packages('grnn', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('grt', dependencies=TRUE, repos='http://cran.rstudio.com/')
require(zoo)
require(grnn)
require(grt)
require(foreach)

setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
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
  legend("topleft", legend=c("Real CLI", "Guess CLI"),  col=c("red", "blue"), pch=c(19,19), cex=0.8)
}
plotPrecFilled <- function(naDF, filledDF, fileName){
  plot(filledDF[,2], main=fileName, ylab="Prec(mm)*10", xlab="Days", col="red", pch=19); 
  points(naDF[,2], col="blue", pch=19)
  grid (10,10, lty = 6, col = "cornsilk2")
  points(filledDF[,2], col="red", pch=19); 
  points(naDF[,2], col="blue", pch=19)
  # Add a legend
  legend("topleft", legend=c("Real CLI", "Guess CLI"),  col=c("red", "blue"), pch=c(19,19), cex=0.8)
}

if(FALSE){
#filled_path <- "C:/Users/IVA/Dropbox/Apps/na_grnn"
  filled_path <- "/home/larisa/Dropbox/Apps/na_grnn"
  setwd(filled_path); getwd(); file_name <- "1969.cli"
  cli_dataset <- createCliDF(file_name) 
  summary(cli_dataset)
# PRE-PROCESSING DATA
  vec.na<-cli_dataset$temp; vec.na.scale <- grt::scale(vec.na);  
  vec.na.scale.min<-min(vec.na.scale, na.rm = T); vec.na.scale.max<-max(vec.na.scale, na.rm = T); 
  vec.na.index <- which(is.na(vec.na)) #
  vec.na.scale.index <- which(is.na(vec.na.scale)) # 
  vec.na.scale.na.omit <- na.omit(vec.na.scale);
  days <- 1:length(vec.na); days.scale <- grt::scale(days) # для прогноза
  days.scale.na.omit <- days.scale[-vec.na.scale.index]; # base::scale(days)
  XY <- data.frame(days.scale.na.omit, vec.na.scale.na.omit)
## SPLIT DATA SAMPLES  
  seed.init<-2017; num.test<-41; set.seed(seed.init)
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
    data.frame(s, sse = test.sse)
  # http://r-train.ru/r-%D0%BA%D0%B2%D0%B0%D0%B4%D1%80%D0%B0%D1%82-rse-%D0%B8-rmse/
  #cat("Sigma= ", round(s,3), "SSE= ", test.sse, "\n")
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
  L <- grnn::learn(XY, variable.column = ncol(XY))
  grnn <- grnn::smooth(L, sigma = s)
  for(i in vec.na.index){
    vec.na.scale[i] <- grnn::guess(grnn, days.scale[i, 1])
    cat(i, vec.na.scale[i],"\n")
  }
    
  vec.na.unscale <- grt::unscale(vec.na.scale)
  plot(vec.na.unscale, col="red"); points(vec.na, col="blue");
  
  plotTempFilled(cli_dataset, cli_dataset_filled, file_name)
  plotPrecFilled(cli_dataset, cli_dataset_filled, file_name)
}



# require(stats)
# x <- matrix(1:10, ncol=2)
# x
# z <- scale(x)
# z
# unscale(z)
# 
# #maybe useful for truncating
# trunc <- 1
# z[abs(z) > trunc] <- sign(z[abs(z) > trunc])*trunc
# unscale(z)
# http://knigi.link/istoriya-arheologiya/sinhronizatsiya-krivyih-formirovanie-37534.html
# https://github.com/johnmcdonnell/General-Recognition-Theory-R/blob/master/R/utils.R


















