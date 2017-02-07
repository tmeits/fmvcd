
# https://datascienceplus.com/fitting-neural-network-in-r/
# http://aakinshin.net/ru/blog/r/functions/
# http://zoonek2.free.fr/UNIX/48_R/15.html
# ! http://dkhramov.dp.ua/Comp/R#shiny
# http://mpoctok.narod.ru/r/intro.htm
# https://www.datacamp.com/courses/machine-learning-toolbox

# /home/larisa/anaconda3/bin/jupyter notebook
require(nnet)
print(Sys.getenv('NUMBER_OF_PROCESSORS'))
rm(list=ls())
filled_path <- "C:/Users/IVA/Dropbox/Apps/R/"
#filled_path <- "/home/larisa/Dropbox/Apps/"
setwd(filled_path); getwd()
cli_dataset <-read.table("1967.cli", header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$prec == -9999, 4] <- NA
cli_dataset[cli_dataset$temp == -9999, 5] <- NA

idx_temp_na <- which(is.na(cli_dataset$temp))
idx_prec_na <- which(is.na(cli_dataset$prec))

# First we need to check that no datapoint is missing, otherwise we need to fix the dataset.
prec_vector <- cli_dataset[, 4]; temp_vector <- cli_dataset[, 5]
print(summary(prec_vector)); print(summary(temp_vector))
apply(data.frame(prec_vector, temp_vector), 2, function(x) sum(is.na(x)))

# Preparing to fit the neural network
# I chose to use the min-max method and scale the data in the interval [0,1]
data <- na.omit(data.frame(time= (1: length(prec_vector)), prec=prec_vector))

prec_div2 <- function(x, sh=40){
  if(x > 100) return(x-sh)
  else if(x > 170) return(x-sh-sh) 
  else return(x)
}
prec_div_vector <- sapply(data[,2], prec_div2)
#data <- data.frame(time= (1: length(prec_div_vector)), prec=prec_div_vector)
index <- sample(1:nrow(data),round(0.75*nrow(data)))
summary(data)
str(index)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
#!!! Note that scale returns a matrix that needs to be coerced into a data.frame
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]
# https://www.r-bloggers.com/what-a-nice-looking-scatterplot/
par(mfrow=c(2,2))
plot(train_, frame.plot=FALSE, col="darkblue", ylim=c(0,1), xlim=c(0,1))
plot(test_, frame.plot=FALSE, col="blue", ylim=c(0,1), xlim=c(0,1))
plot(train_, frame.plot=FALSE, col="darkred", ylim=c(0,1), xlim=c(0,1))
plot(test_, frame.plot=FALSE, col="orange", ylim=c(0,1), xlim=c(0,1))

nnet.fit <- nnet(prec~ ., data=train_, size=11, linout=TRUE, maxit=450, trace=F) #, 
                 #skip=TRUE, MaxNWts=10000, abstol = 1.0e-4, reltol = 1.0e-8)
# summarize the fit
print(summary(nnet.fit))

# make predictions
x <- data.frame(time=test_[, 1])
predictions <- predict(nnet.fit, x, type="raw")
summary(predictions)
# summarize accuracy
y <- test_[, 2]
mse <- mean((y - predictions)^2, na.rm = T)
cat("MSE = ", mse, "\n")

abs_predict <- function(x){
  if(x < 0) return(0)
  else return(x)
}
pred_src<-data.frame(prec_predict = sapply(predictions[, 1], abs_predict))

#plot.new()
par(mfrow=c(1,1))
plot(test_, col = "darkred", main=paste("MSE = ", round(mse,4), "\n"), frame.plot=FALSE,
     ylim=c(0,1), xlim=c(0,1))
res_src <- data.frame(time=test_[, 1], prec=pred_src[, 1])
points(res_src, col="darkblue")

# http://r-train.ru/%D1%84%D1%83%D0%BD%D0%BA%D1%86%D0%B8%D1%8F-train/
# http://r-train.ru/%D0%BF%D1%80%D0%B5%D0%BE%D0%B1%D1%80%D0%B0%D0%B7%D0%BE%D0%B2%D0%B0%D0%BD%D0%B8%D1%8F-%D0%B4%D0%B0%D0%BD%D0%BD%D1%8B%D1%85-%D0%BF%D1%80%D0%B8-%D0%BF%D0%BE%D0%BC%D0%BE%D1%89%D0%B8-preprocess/
