library(DEoptim)
library(maxLik)
library(sirt)



dirichlet.ll <- function(param) {
  eps <- 0.01
  logYY <- YY
  # transform observations x into (0,1)
  logYY <- (logYY + eps ) / ( 1 + 2 * eps )
  # N <- nrow(YY)
  # K <- ncol(YY)
  # v1 <- N * log(gamma(sum(param)))
  # v2 <- N * sum(log(gamma(parm)))
  # v3 <- 0 
  # for(i in 1:N){
  #   for(j in 1:ncol(YY)){
  #     v3 <- v3 + (param[j]-1)*log(logYY[i,j])
  #   }
  # }

  ll <- sum(ddirichlet(logYY,param))
  ll
}


dirichlet.mle(YY)
res <- maxLik(dirichlet.ll, start=c(.5,.5,.5)) # use 'wrong' start values

res <-DEoptim(dirichlet.ll,lower = c(0,0,0),
              upper =  c(2,2,2))












param <- c(6,.5)

vbetas <- matrix(0.5,nrow = 2 ,ncol = 3)


loglik <- function(param){
  vbetas <- matrix(param,ncol = 3,byrow = T)
  XX  <- cbind(1,base_result_jogadores_modelo$`Passe curto`)
  YY <- cbind(as.numeric(base_result_jogadores_modelo$resultado == "VA"),
              as.numeric(base_result_jogadores_modelo$resultado == "Emp"),
              as.numeric(base_result_jogadores_modelo$resultado == "VB"))
  ai <- XX%*%vbetas
  Ai <- apply(ai,1,sum)
  psAi <- digamma(Ai)
  psAi <- matrix(ifelse(psAi<0|is.na(psAi),0,psAi),ncol=ncol(YY),
                 nrow = length(psAi))
  logy <- YY
  psai <- digamma(ai)
  for(i in 1:3){
    psai[,i] <- ifelse(psai[,i]<0|is.na(psai[,i]),0,psai[,i])
    
    # transform observations x into (0,1)
    eps <- .01
    logy[,i] <- ( logy[,i] + eps ) / ( 1 + 2 * eps )
  }

  logy <- log(logy)
  
  loglikelihood <- t(XX) %*% (psAi - psai + logy) 
  return(loglikelihood)
}


res <- maxLik(loglik, start=rep(0.05,6)) # use 'wrong' start values
summary( res )

round(res$estimate,2)


dirichlet.ll <- function(param) {
  eps <- 0.01
  logYY <- YY
  for(i in 1:ncol(YY)){
    # transform observations x into (0,1)
    logYY[,i] <- ( YY[,i] + eps ) / ( 1 + 2 * eps )
  }
  
  ll <- sum(log( ddirichlet( logYY , param ) ) )
  ll
}

library(sirt)
dirichlet.mle(YY)
res <- maxLik(dirichlet.ll, start=c(.33,.33,.33)) # use 'wrong' start values

res <-DEoptim(dirichlet.ll,lower = c(.01,.01,.01),
        upper =  c(.5,.5,.5))
res$optim

# transform observations x into (0,1)
eps <- .01
x <- ( YY + eps ) / ( 1 + 2 * eps )
x <- x[sample(2000,100),]
# define likelihood function
dirichlet.ll <- function(param) {
  ll <- sum( log( ddirichlet(x,param ) ) )
  ll
}
mod1a <- sirt::dirichlet.mle( x )
mod1a
# estimation in maxLik
mod1b <- maxLik(dirichlet.ll, start=c(.13,.12,.12)) 
print(mod1b)
coef(mod1b)

