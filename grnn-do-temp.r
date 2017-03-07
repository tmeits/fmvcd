# IVA 7.3.17
install.packages('grnn', dependencies=TRUE, repos='http://cran.rstudio.com/')
# https://forum.exploit.in/index.php?showtopic=112038&mode=threaded&pid=695337
# ftp://cran.r-project.org/pub/R/web/packages/foreach/vignettes/foreach.pdf

require(grnn); require(foreach)
setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
}
#filled_path <- "/home/larisa/Dropbox/Apps/16.3.17"
filled_path <- "C:/Users/IVA/Dropbox/Apps/16.3.17"
setwd(filled_path); getwd(); file_name <- '2014.cli'
cli_dataset <-read.table(file_name, header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]; 
print(str(temp_vector)); print(summary(temp_vector))
days <- c(1:dim(cli_dataset)[1])

# PRE-PROCESSING DATA 
X <- days
st.X <- scale(X)
st.Y <- scale(temp_vector)
XY <- data.frame(st.X, st.Y)

### SPLIT DATA SAMPLES CLI
#set.seed(2011) # http://r-analytics.blogspot.ru/2012/05/blog-post.html#.WL0LEjvyiMo
rows <- sample(1:nrow(XY), nrow(XY) - 41)
trainXY <- XY[rows, ]; testXY <- XY[-rows, ]
plot(trainXY, col="red", main="SPLIT DATA SAMPLES TEMP2014.CLI", pch=19);  
points(testXY, col="blue", pch=18); 
legend("topleft", legend=c("Train CLI", "Test CLI"), col=c("red", "blue"), lty=1:2, cex=0.8)
print(rows); nrow(trainXY); nrow(testXY); print(nrow(trainXY) + nrow(testXY))

idx_temp_na <- which(is.na(cli_dataset$temp))
idx_temp_na
sigmaPlot <- seq(0.01, 1., 0.05)
ssePlot <- 1: length(sigmaPlot)
cv <- foreach(s = seq(0.01, 1., 0.05)) %do% {
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
cat("\n### SSE FROM VALIDATIONS ###\n")
print(cv)
# http://statsoft.ru/home/textbook/modules/stnonlin.html
# error sum of squares a?? SSE
for(i in 1: length(sigmaPlot)){
  ssePlot[i] <- cv[[i]][1,2]
}
plot(sigmaPlot, ssePlot, main="SSE FROM VALIDATIONS - TEMP2014.CLI", 
     xlab = "sigma", ylab = "error sum of squares - SSE", col="red", pch=19, type="b")
grid(lty=3, col="gray")

s<-min(ssePlot) # SCORE THE WHOLE DATASET WITH GRNN
L <- grnn::learn(trainXY, variable.column = ncol(trainXY))
grnn <- grnn::smooth(L, sigma = s)
newY <- st.Y
for(i in 1:length(st.X)){
  newY[i] <- grnn::guess(grnn, st.X[i])
}
cat("Sigma= ", round(s,3), "SSE= ", round(sum(newY-st.Y)^2, 6), "\n")
plot(XY, main = paste0("Sigma= ", round(s,3), ", SSE= ", round(sum(newY-st.Y)^2, 6), "\n"),
     col="red", type = 'b', pch=19)
lines(st.X, newY, col="blue", type = 'b', pch=18, lty=2);
# Add a legend
legend("topleft", legend=c("Real CLI", "Guess CLI"),  col=c("red", "blue"), lty=1:2, cex=0.8)

s<-min(ssePlot) # The measured and theoretical values
L <- grnn::learn(trainXY, variable.column = ncol(trainXY))
grnn <- grnn::smooth(L, sigma = s)
guessY <- testXY[, 2]
  for(i in 1:length(testXY[, 1])){
    guessY[i] <- grnn::guess(grnn, testXY[i, 1])
  }
cat("Sigma= ", round(s,3), "SSE= ", round(sum(guessY-testXY[, 2])^2, 6), "\n")
  
plot(testXY, main = paste0("Sigma= ", round(s,3), ", SSE= ", round(sum(guessY-testXY[, 2])^2, 6), "\n"),  col="red", type = 'b', pch=19)
lines(testXY[, 1], guessY, col="blue", type = 'b', pch=18, lty=2);
# Add a legend
legend("topleft", legend=c("Real CLI", "Guess CLI"),  col=c("red", "blue"), lty=1:2, cex=0.8)

M.lm<-summary(lm(guess~test, data.frame(guess=guessY, test=testXY[,2])))
summary(M.lm)
M.lm$adj.r.squared
M.lm$r.squared
plot(data.frame(guess=newY, train=XY[,2])); abline(M.lm,  col="blue")

# https://susanejohnston.wordpress.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
library(ggplot2)

M<-data.frame(guess=newY, train=XY[,2])
ggplot(M, aes(x = train, y = guess)) + 
         geom_point() + 
        stat_smooth(method = "lm", col = "red")





