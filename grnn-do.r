# https://www.r-bloggers.com/fitting-generalized-regression-neural-network-with-python/
# http://logic.pdmi.ras.ru/~sergey/teaching/mlau12/13-neural.pdf
# https://www.researchgate.net/publication/280235021_RESENIE_ZADAC_OPTIMIZACII_S_POMOSU_NEJRONNOJ_SETI_HOPFILDA_SOLVING_OPTIMIZATION_PROBLEMS_USING_HOPFIELD_NEURAL_NETWORK
pkgs <- c('MASS', 'doParallel', 'foreach', 'grnn')
lapply(pkgs, require, character.only = T)
registerDoParallel(cores = 4)
# https://forum.exploit.in/index.php?showtopic=112038&mode=threaded&pid=695337
# ftp://cran.r-project.org/pub/R/web/packages/foreach/vignettes/foreach.pdf
data(Boston); str(Boston)
# PRE-PROCESSING DATA 
X <- Boston[-14]
st.X <- scale(X)
Y <- Boston[14]
boston <- data.frame(st.X, Y)

# SPLIT DATA SAMPLES
set.seed(2013)
rows <- sample(1:nrow(boston), nrow(boston) - 200)
set1 <- boston[rows, ]
set2 <- boston[-rows, ]


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
# http://www.stgau.ru/company/personal/user/7684/files/lib/%D0%98%D0%BD%D1%82%D0%B5%D0%BB%D0%BB%D0%B5%D0%BA%D1%82%D1%83%D0%B0%D0%BB%D1%8C%D0%BD%D1%8B%D0%B5%20%D1%81%D0%B8%D1%81%D1%82%D0%B5%D0%BC%D1%8B%20%D0%B8%20%D1%82%D0%B5%D1%85%D0%BD%D0%BE%D0%BB%D0%BE%D0%B3%D0%B8%D0%B8/%D0%9D%D0%B5%D0%B9%D1%80%D0%BE%D1%81%D0%B5%D1%82%D0%B5%D0%B2%D1%8B%D0%B5%20%D1%82%D0%B5%D1%85%D0%BD%D0%BE%D0%BB%D0%BE%D0%B3%D0%B8%D0%B8%20%D0%BE%D0%B1%D1%80%D0%B0%D0%B1%D0%BE%D1%82%D0%BA%D0%B8%20%D0%B8%D0%BD%D1%84%D0%BE%D1%80%D0%BC%D0%B0%D1%86%D0%B8%D0%B8/5%20%D0%93%D0%BB%D0%B0%D0%B2%D0%B0%202.pdf
# http://studopedia.org/11-90359.html
# https://www.hse.ru/data/2010/04/02/1218415543/dborusyak%20BA.pdf
# https://eoncosurg.com/nejronnaya-set-v-prognozirovanii-otd
# http://foibg.com/ibs_isc/ibs-19/ibs-19-p21.pdf
# http://www.dissercat.com/content/geoekologicheskaya-otsenka-i-prognozirovanie-opasnykh-prirodno-tekhnogennykh-protsessov-na-v






























