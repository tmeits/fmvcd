# Renjin is a JVM-based interpreter for the R language for statistical computing. http://www.renjin.org/
# http://www.inp.nsk.su/~baldin/DataAnalysis/R/R-10-hpc.pdf
# http://www.tandfonline.com/doi/pdf/10.1623/hysj.51.6.1092
# https://arxiv.org/pdf/1503.00855.pdf
# http://dirk.eddelbuettel.com/papers/ismNov2009introHPCwithR.pdf
# https://www.r-bloggers.com/deploying-desktop-apps-with-r/
# http://r.psylab.info/blog/2015/05/09/code-profiling/
# http://www.burns-stat.com/pages/Tutor/R_inferno.pdf
require(compiler)
enableJIT(3)

ds <- function(Xa, X) {
  value <- (X - Xa) %*% t(X - Xa)
  return(as.numeric(value))
}

pattern <- function(Xa, X, sigma) {
  res <- exp( - ds(Xa, X) / (2 * sigma ^ 2) )
  return(as.numeric(res))
}

patterns <- function(Xa, X, sigma)
  apply(Xa, 1, pattern, X, sigma)

K <- function(Xa, Ya, X, sigma) {
  patterns1 <- patterns(Xa, X, sigma)
  f <- sum(Ya * patterns1) / sum(patterns1)
  return(f)
}

sim <- function(Xa, Ya, Ga, sigma){
  len <- length(Ga[,1])
  res <- as.matrix(1:len)
  for(i in 1:len) {
    res[i] <- K(Xa, Ya, Ga[i,], sigma)
  }
  return(res)
}
if(FALSE) {
  n <- 400; set.seed(123456)
  x <- as.matrix(runif(n, -2, 2))
  x<- as.matrix(data.frame(x1=runif(n, -2, 2),x2=runif(n, -2, 2)))
  y <- as.matrix(x^3 + rnorm(n, 0, .1))
  plot(x,y, col="blue")
  x.sample <- as.matrix(sample(x, 50))
  
  x<-matrix(runif(n*2, -1, 1), ncol=2)
  y<-as.matrix(x^2 + rnorm(n, 0, .1))[,1]
  sim(x, y, x, 0.01)
  
  system.time(sim(x, y, x.sample, 0.01)) 
  #пользователь      система       прошло 
  #18.98             0.00          18.99
}



