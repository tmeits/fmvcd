
# https://datascienceplus.com/fitting-neural-network-in-r/
# /home/larisa/anaconda3/bin/jupyter notebook
require(nnet)

rm(list=ls())
filled_path <- "C:/Users/IVA/Apps/R/"
filled_path <- "/home/larisa/Dropbox/Apps/"
setwd(filled_path); getwd()
cli_dataset <-read.table("1967.cli", header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$prec == -9999, 4] <- NA
cli_dataset[cli_dataset$temp == -9999, 5] <- NA

# First we need to check that no datapoint is missing, otherwise we need to fix the dataset.
prec_vector <- cli_dataset[, 4]; temp_vector <- cli_dataset[, 5]
summary(prec_vector); summary(temp_vector)
apply(data.frame(prec_vector, temp_vector), 2, function(x) sum(is.na(x)))


# Preparing to fit the neural network
# I chose to use the min-max method and scale the data in the interval [0,1]
data <- na.omit(data.frame(time= (1: length(prec_vector)), prec=prec_vector))
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

nnet.fit <- nnet(prec~ ., data=train_, size=7, linout=TRUE, maxit=450, trace=F, 
                 skip=TRUE, MaxNWts=10000, abstol = 1.0e-4, reltol = 1.0e-8)
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
par(mfrow=c(2,1))
plot(test_, col = "darkred", main=paste("MSE = ", round(mse,4), "\n"), frame.plot=FALSE)
res_src <- data.frame(time=test_[, 1], prec=pred_src[, 1])
points(res_src, col="darkblue")
