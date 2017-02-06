# 
require(nnet)

rm(list=ls())
filled_path <- "C:/Users/IVA/Apps/R/"
setwd(filled_path); getwd()
cli_dataset <-read.table("1967.cli", header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$prec == -9999, 4] <- NA
cli_dataset[cli_dataset$temp == -9999, 5] <- NA
print(summary(cli_dataset[, c(4)])); print(summary(cli_dataset[, c(5)]))

prec_vector <- cli_dataset[, 4]; temp_vector <- cli_dataset[, 5]
# 
train_src <- data.frame(time= (1: length(prec_vector)), prec=prec_vector)
train <- data.frame(time= (1: length(prec_vector))/370., prec=prec_vector/350.)
#
nnet.fit <- nnet(prec~ ., data=train, size=7, na.action = "na.omit",
                 linout=TRUE, skip=TRUE, MaxNWts=10000, trace=TRUE, maxit=50,
                 abstol = 1.0e-4, reltol = 1.0e-8)
# summarize the fit
print(summary(nnet.fit))
# make predictions
x <- data.frame(time=train[, 1])
predictions <- predict(nnet.fit, x, type="raw")

# summarize accuracy
y <- train[, 2]
mse <- mean((y - predictions)^2, na.rm = T)
cat("MSE = ", mse, "\n")
#
abs_predict <- function(x){
  if(x < 0) return(0)
  else return(as.integer(x*350.))
}
pred_src<-data.frame(prec_predict = sapply(predictions[, 1], abs_predict))

plot.new()
plot(train_src, col = "red")
res_src <- data.frame(time=train_src[, 1], prec=pred_src[, 1])
points(res_src, col="blue")










