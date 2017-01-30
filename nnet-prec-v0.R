# It’s all about hidden layers
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
plot(train, col = "red")

#nnet(x, y, weights, size, Wts, mask, linout = FALSE, entropy = FALSE, softmax = FALSE, 
#     censored = FALSE, skip = FALSE, rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
#     trace = TRUE, MaxNWts = 1000, abstol = 1.0e-4, reltol = 1.0e-8, ...)
nnet.fit <- nnet(prec~ ., data=train, size=7, na.action = "na.omit",
                 linout=TRUE, skip=TRUE, MaxNWts=10000, trace=TRUE, maxit=500,
                 abstol = 1.0e-4, reltol = 1.0e-8)
# summarize the fit
#print(summary(nnet.fit))
# make predictions
x <- data.frame(time=train[, 1])
predictions <- predict(nnet.fit, x, type="raw")

# summarize accuracy
y <- train[, 2]
mse <- mean((y - predictions)^2, na.rm = T)
cat("MSE = ", mse, "\n")
plot(train[, 2], predictions)


#import function from Github
require(RCurl)

root.url <- 'https://gist.github.com/fawda123'
raw.fun <- paste(root.url, 
               '5086859/raw/17fd6d2adec4dbcf5ce750cbd1f3e0f4be9d8b19/nnet_plot_fun.r', 
               sep='/')

# install.packages("scales")
# install.packages("foreach")
library(foreach)
source("nnet_plot.R")
par(mar=numeric(4),mfrow=c(1,2),family='serif')
plot.nnet(nnet.fit, nid=F)
plot.nnet(nnet.fit)


# https://www.r-bloggers.com/selecting-the-number-of-neurons-in-the-hidden-layer-of-a-neural-network/
# https://www.r-bloggers.com/visualizing-neural-networks-from-the-nnet-package/
# https://www.r-bloggers.com/animating-neural-networks-from-the-nnet-package/
# https://www.r-bloggers.com/sensitivity-analysis-for-neural-networks/
# https://www.r-bloggers.com/variable-importance-in-neural-networks/
# https://www.r-bloggers.com/imputing-missing-data-with-expectation-maximization/
# https://www.r-bloggers.com/what-are-the-best-machine-learning-packages-in-r/
# http://portal.stats.ox.ac.uk/userdata/ripley/Photos/CorkKerry2015/photos/DSCF0666.jpg
# https://xakep.ru/2017/01/20/loops-in-r-v2/
# https://www.rdocumentation.org/packages/nnet/versions/7.3-12/topics/nnet
# https://xakep.ru/2015/04/20/195-learning-r-programming-language/
# https://habrahabr.ru/company/infopulse/blog/305692/
# https://habrahabr.ru/company/infopulse/blog/307242/
# https://habrahabr.ru/company/infopulse/blog/307708/
# https://habrahabr.ru/post/310108/
# https://habrahabr.ru/company/infopulse/blog/309052/
# https://habrahabr.ru/post/310472/
# https://habrahabr.ru/post/317130/
# https://habrahabr.ru/post/317314/
# https://habrahabr.ru/post/301176/
# http://www.codeinstinct.pro/2015/11/azure-ml-hackathon.html
# http://www.codeinstinct.pro/2015/03/azure-machine-learning-for-data-scientist.html
# http://www.codeinstinct.pro/2016/10/gpu-in-cloud.html
# 
# !!! https://habrahabr.ru/company/infopulse/blog/281400/
# https://github.com/ropensci/lawn
# http://tryr.codeschool.com/levels/1/challenges/3
# https://stepik.org/course/%D0%9E%D1%81%D0%BD%D0%BE%D0%B2%D1%8B-%D1%81%D1%82%D0%B0%D1%82%D0%B8%D1%81%D1%82%D0%B8%D0%BA%D0%B8-76/syllabus
# https://habrahabr.ru/company/stepic/blog/250527/
# https://habrahabr.ru/post/207750/

# http://www.dissercat.com/content/metod-evolyutsionnogo-nakopleniya-priznakov-dlya-avtomaticheskogo-postroeniya-neironnykh-set
# http://statsoft.ru/home/textbook/modules/stdatmin.html
# https://vk.com/page-77692614_48888430
# https://vk.com/wall-74058720_1808
# https://vk.com/doc926644_402483592?hash=c4a031c96c3aef7977&dl=b92980d0245b7c5291
# http://cyberleninka.ru/article/n/modelirovanie-iskusstvennyh-neyronnyh-setey-s-pomoschyu-graficheskogo-adaptera-obschego-naznacheniya
# https://www.ibm.com/support/knowledgecenter/ru/SSLVMB_24.0.0/spss/neural_network/intro_neuralnet_procedures.html




