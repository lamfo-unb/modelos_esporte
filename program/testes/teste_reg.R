library(DirichletReg)
library(sirt)
library(maxLik)

rm(list = ls())
gc()


nr <- 100
beta0 <- c(.5,1.2,2.6)
xr <- matrix(1,nrow = nr)
parmreg <- matrix(beta0,nrow=length(beta0),byrow = F)
ai <- xr%*%t(parmreg)
air <- exp(ai)
yr <-rdirichlet(nr,air)
vecs <- dirichlet.mle(yr)
log(vecs$alpha)
yrt <- DR_data(yr)
lake1 <- DirichReg(yrt ~ 1)
lake1$coefficients
### est ok


## FUNCIONANDO
logDirichregregt <- function(parm){
  ai <- exp(parm)
  ai <- matrix(ai,nrow=nrow(yr),ncol=length(parm),byrow = T)
  Ai <- apply(ai,1,sum)
  v0 <- (ai-1)*log(yr)-log(gamma(ai))
  v1 <- apply(v0,1,sum)
  veroi <- sum(log(gamma(Ai)) + v1)
  return(veroi)
}


## RESTRIÇÕES
A <- diag(1,length(beta0))
B <- matrix(rep(0,length(beta0)),ncol=1)


res4 <- maxLik(logDirichregregt,start = c(1.1,1.1,1.1),
               method = "BFGS")
res4$estimate

## TESTANDO REGRESSOR ---------------
beta0 <- c(.25,.6,.8,-1.2,.35,1.2)

xr <- cbind(1,matrix(rnorm(nr),nrow = nr))
parmreg <- matrix(beta0,nrow=3,byrow = F)
ai <- xr%*%t(parmreg)
air <- exp(ai)
yr<-rdirichlet(nr,air)

## regressão dados
yrt <- DR_data(yr)
dxr <- data.frame(xr)



logDirichregregt <- function(parm){
  parmreg <- matrix(parm,nrow=ncol(yr),byrow = F)
  ai <- xr %*% t(parmreg)
  ai <- exp(ai)
  Ai <- apply(ai,1,sum)
  v0 <- ((ai-1)*log(yr)-log(gamma(ai)))
  v1 <- apply(v0,1,sum)
  veroi <- sum(log(gamma(Ai)) + v1)
  return(veroi)
}

## função gradiente

gradients <- function(parm){
  g <- parm
  logYY <- log(yr)
  for(i in 1:ncol(xr)){
    for(j in 1:ncol(yr)){
      g[(i-1)*ncol(yr)+j] <-  sum((digamma(apply(ai,1,sum)) -  digamma(ai[,j]) + logYY[,j])*xr[,i]*ai[,j])
    }
  }
  return(g)
}

res4 <- maxLik(logDirichregregt,start = c(1.1,1.1,
                                          1.1,1.1,
                                          1.1,1.1),
               grad = gradients,
               method = "SANN")

## parametros 
matrix(res4$estimate,nrow=ncol(yr),byrow = F)

## regressão mdoelo

yrt <- DR_data(yr)
dxr <- data.frame(xr)
lake1 <- DirichReg(yrt ~ X2, data = dxr,model = "common")

matrix(lake1$coefficients,nrow=ncol(yr),byrow = T)

## valores verdadeiros
matrix(c(.25,.6,.8,-1.2,.35,1.2),nrow=ncol(yr),byrow = F)



