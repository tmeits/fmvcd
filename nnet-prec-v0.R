require(nnet)

rm(list=ls())
filled_path <- "C:/Users/teipolykom/git/fmvcd/krest/"
#filled_path <- "/home/larisa/Dropbox/git/fmvcd/krest/"
setwd(filled_path); getwd()
# load data
cli_dataset <-read.table("1967.cli", header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$prec == -9999, 4] <- NA
print(summary(cli_dataset[, c(4)]))

prec_vector <- cli_dataset[, 4]
train <- data.frame(time= (1: length(prec_vector)), prec=prec_vector/350.)
train <- na.omit(train)
plot(train, col = "red")

nnet.fit <- nnet(prec~ ., data=train, size=121, 
                 linout=TRUE, skip=TRUE, MaxNWts=10000, trace=TRUE, maxit=5500)
# summarize the fit
print(summary(nnet.fit))
# make predictions
x <- data.frame(time=train[, 1])
predictions <- predict(nnet.fit, x, type="raw")

# summarize accuracy
y <- train[, 2]
mse <- mean((y - predictions)^2)
cat("MSE = ", mse, "\n")

# https://www.r-bloggers.com/selecting-the-number-of-neurons-in-the-hidden-layer-of-a-neural-network/
# https://www.r-bloggers.com/visualizing-neural-networks-from-the-nnet-package/


