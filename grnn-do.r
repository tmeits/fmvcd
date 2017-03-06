# IVA 2.3.17
#install.packages('caret', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('dplyr', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('grnn', dependencies=TRUE, repos='http://cran.rstudio.com/')
install.packages('DMwR', dependencies=TRUE, repos='http://cran.rstudio.com/')
#install.packages('doParallel', dependencies=TRUE, repos='http://cran.rstudio.com/')
pkgs <- c('MASS', 'doParallel', 'foreach', 'grnn', 'zoo')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 1)
# https://forum.exploit.in/index.php?showtopic=112038&mode=threaded&pid=695337
# ftp://cran.r-project.org/pub/R/web/packages/foreach/vignettes/foreach.pdf
require(zoo); require(grnn); require(foreach)
setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
}

#filled_path <- "/home/larisa/Dropbox/Apps/16.3.17"
filled_path <- "C:/Users/IVA/Dropbox/Apps/16.3.17"
setwd(filled_path); getwd()
file_name <- '1967.cli'

cli_dataset <-read.table(file_name, header=FALSE, sep="")
names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
cli_dataset[cli_dataset$prec == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4];
cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]; 
print(str(prec_vector)); print(str(temp_vector))
print(summary(prec_vector)); print(summary(temp_vector))

days <- c(1:dim(cli_dataset)[1])
prec_zoo <- zoo(prec_vector, days)
temp_zoo <- zoo(temp_vector, days)
prec_filled <- sapply(as.integer(na.spline(prec_zoo)), setZeroNegativeNumber)
temp_filled <- as.integer(na.spline(temp_zoo))
print(str(prec_filled)); print(str(temp_filled))
print(summary(prec_filled)); print(summary(temp_filled))

# PRE-PROCESSING DATA 
X <- days
st.X <- scale(X)
st.Y <- scale(temp_filled)
st.Y <- scale(prec_filled)
XY <- data.frame(st.X, st.Y)
cat(seq(0., 1., 0.025), "\n")
cv <- foreach(s = seq(0., 1., 0.025)) %do% {
  L <- grnn::learn(XY, variable.column = ncol(XY))
  grnn <- grnn::smooth(L, sigma = s)
  newY <- st.Y
  for(i in 1:length(st.X)){
    newY[i] <- grnn::guess(grnn, st.X[i])
  }
  cat("Sigma= ", round(s,3), "SSE= ", round(sum(newY-st.Y)^2, 6), "\n") # 
}
# e the sum of squared errors (SSE)

s<-0.05 
#s<-0.01
#s<-0.0025
#s<-0.1
L <- grnn::learn(XY, variable.column = ncol(XY))
grnn <- grnn::smooth(L, sigma = s)
newY <- st.Y
for(i in 1:length(st.X)){
  newY[i] <- grnn::guess(grnn, st.X[i])
}
cat("Sigma= ", round(s,3), "SSE= ", round(sum(newY-st.Y)^2, 6), "\n")

# Sum squared error performance function http://cens.ioc.ee/local/man/matlab/toolbox/nnet/sse.html
# http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way
plot(XY, main = paste0("Sigma= ", round(s,3), ", SSE= ", round(sum(newY-st.Y)^2, 6), "\n"),
     col="red", type = 'b', pch=19)
lines(st.X, newY, col="blue", type = 'b', pch=18, lty=2);
# Add a legend
legend("topleft", legend=c("Real CLI", "Guess CLI"),  col=c("red", "blue"), lty=1:2, cex=0.8)


###
### SPLIT DATA SAMPLES CLI

#set.seed(2011) # http://r-analytics.blogspot.ru/2012/05/blog-post.html#.WL0LEjvyiMo
rows <- sample(1:nrow(XY), nrow(XY) - 100)
trainXY <- XY[rows, ]; testXY <- XY[-rows, ]
plot(trainXY, col="red", main="SPLIT DATA SAMPLES CLI", pch=19);  points(testXY, col="blue", pch=18); 
legend("topleft", legend=c("Train CLI", "Test CLI"),  col=c("red", "blue"), lty=1:2, cex=0.8)
print(rows)
nrow(trainXY); nrow(testXY); print(nrow(trainXY) + nrow(testXY))

idx_temp_na <- which(is.na(cli_dataset$temp))
idx_prec_na <- which(is.na(cli_dataset$prec))

cv <- foreach(s = seq(0., 1., 0.025)) %do% {
  L <- grnn::learn(trainXY, variable.column = ncol(XY))
  grnn <- grnn::smooth(L, sigma = s)
  newY <- st.Y
  for(i in 1:length(st.X)){
    newY[i] <- grnn::guess(grnn, st.X[i])
  }
  cat("Sigma= ", round(s,3), "SSE= ", round(sum(newY-st.Y)^2, 6), "\n") # 
}



# DEFINE A FUNCTION TO SCORE GRNN
pred_grnn <- function(x, nn){
  xlst <- split(x, 1:nrow(x))
  pred <- foreach(i = xlst, .combine = rbind) %do% {
    data.frame(pred = grnn::guess(nn, as.matrix(i)), i, row.names = NULL)
  }
}

# SEARCH FOR THE OPTIMAL VALUE OF SIGMA BY THE VALIDATION SAMPLE
cv <- foreach(s = seq(0.2, 1, 0.05), .combine = rbind) %do% {
  L <- grnn::learn(set1, variable.column = ncol(set1))
  grnn <- grnn::smooth(L, sigma = s)
  pred <- pred_grnn(set2[, -ncol(set2)], grnn)
  test.sse <- sum((set2[, ncol(set2)] - pred$pred)^2)
  data.frame(s, sse = test.sse)
}

cat("\n### SSE FROM VALIDATIONS ###\n")
print(cv)
jpeg('grnn_cv.jpeg', width = 800, height = 400, quality = 100)
with(cv, plot(s, sse, type = 'b'))

cat("\n### BEST SIGMA WITH THE LOWEST SSE ###\n")
print(best.s <- cv[cv$sse == min(cv$sse), 1])


# SCORE THE WHOLE DATASET WITH GRNN
final_grnn <- grnn::smooth(learn(set1, variable.column = ncol(set1)), sigma = best.s)
pred_all <- pred_grnn(boston[, -ncol(set2)], final_grnn)
jpeg('grnn_fit.jpeg', width = 800, height = 400, quality = 100)
plot(pred_all$pred, boston$medv) 
dev.off()

# https://www.r-bloggers.com/fitting-generalized-regression-neural-network-with-python/
# http://logic.pdmi.ras.ru/~sergey/teaching/mlau12/13-neural.pdf
# https://www.researchgate.net/publication/280235021_RESENIE_ZADAC_OPTIMIZACII_S_POMOSU_NEJRONNOJ_SETI_HOPFILDA_SOLVING_OPTIMIZATION_PROBLEMS_USING_HOPFIELD_NEURAL_NETWORK
# http://www.stgau.ru/company/personal/user/7684/files/lib/%D0%98%D0%BD%D1%82%D0%B5%D0%BB%D0%BB%D0%B5%D0%BA%D1%82%D1%83%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5%20%D1%81%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D1%8B%20%D0%B8%20%D1%82%D0%B5%D1%85%D0%BD%D0%BE%D0%BB%D0%BE%D0%B3%D0%B8%D0%B8/%D0%9D%D0%B5%D0%B9%D1%80%D0%BE%D1%81%D0%B5%D1%82%D0%B5%D0%B2%D1%8B%D0%B5%20%D1%82%D0%B5%D1%85%D0%BD%D0%BE%D0%BB%D0%BE%D0%B3%D0%B8%D0%B8%20%D0%BE%D0%B1%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B8%20%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%86%D0%B8%D0%B8/5%20%D0%93%D0%BB%D0%B0%D0%B2%D0%B0%202.pdf
# http://studopedia.org/11-90359.html
# https://www.hse.ru/data/2010/04/02/1218415543/dborusyak%20BA.pdf
# https://eoncosurg.com/nejronnaya-set-v-prognozirovanii-otd
# http://foibg.com/ibs_isc/ibs-19/ibs-19-p21.pdf
# http://www.dissercat.com/content/geoekologicheskaya-otsenka-i-prognozirovanie-opasnykh-prirodno-tekhnogennykh-protsessov-na-v


General Regression Neural Network with R

Подобно нейронной сети обратного распространения, общая регрессия нейронная сеть (GRNN) 
является хорошим инструментом для аппроксимации функции (модели). 
Specht in 1991, GRNN имеет преимущества мгновенного обучения и легкой настройки. 
GRNN будет сформирована  с помощью  обучения за 1 проход. 
Единственное нужно настроить гипер-параметр SIGMA, который регулирует гладкость GRNN.

neuralnet: Training of Neural Networks
PRE-PROCESSING DATA
























