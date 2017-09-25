varsmodel <- c("Passe curto","Dribles","Cruzamento")
names(base_result_jogadores_modelo)
varsmodely <- c("VA","Emp","VB")
XX  <- data.matrix(cbind(1,base_result_jogadores_modelo %>% select(varsmodel)))
YY <-  data.matrix(cbind(as.numeric(base_result_jogadores_modelo$resultado == "VA"),
            as.numeric(base_result_jogadores_modelo$resultado == "Emp"),
            as.numeric(base_result_jogadores_modelo$resultado == "VB")))

base_result_jogadores_modelo %>%
  group_by(resultado) %>%
  summarise(valor = mean(`Passe curto`))

digamma1 <- function(x,h=.001){
  ( digamma(x+h) - digamma(x-h) ) / (2*h)
}




mletest <- function (YY,XX, weights = NULL, eps = 10^(-2), convcrit = 1e-05, 
                     maxit = 100, oldfac = 0.3, progress = FALSE) {
  N <- nrow(x)
  K <- ncol(x)
  YY <- (YY + eps)/(1 + 2 * eps)
  YY <- YY/rowSums(YY)
  N <- nrow(YY)
  if (is.null(weights)) {
    weights <- rep(1, N)
  }
  weights <- N * weights/sum(weights)
  conv <- 1
  iter <- 1
  
  ### Chute inicial para beta seguindo Hijazi and Jernigan apud Camargo, Stern e Correa
  
  r <- 1000
  m <- 50
  A <- NULL
  W <- NULL
  i <- 1
  set.seed(17092017)
  for(i in 1:r){
    amostra <- sample(nrow(YY),m,replace = T)
    YYsample <- matrix(YY[amostra,],ncol = ncol(YY),nrow=m,byrow = F)
    XXsample <- matrix(XX[amostra,],ncol = ncol(XX),nrow=m,byrow = F)
    A_t <- apply(YYsample,2,sum)
    W_t <- apply(XXsample,2,mean)
    
    A <- rbind(A,A_t)
    W <- rbind(W,W_t)
  }
  betas <- matrix(NA,nrow=ncol(A),ncol = ncol(W))
  lambda <- 1
  i <- 1
  for(i in 1:ncol(A)){
    YYcolum <- matrix(YYsample[,i],ncol= 1 )
    ## RIDGE
    lambdas <- rep(lambda,ncol(XXsample))
    betas[i,] <- t(solve(t(XXsample)%*%XXsample + diag(lambdas))%*%(t(XXsample)%*%YYcolum))
  }
  betass <- NA
  for(i in 1:ncol(XX)){
    betass <- c(betass,betas[,i])
  }
  betas <- betass[-1]
  rm(betass)
  
  
  while ((conv > convcrit) & (iter < maxit)) {
    beta0 <- betas
    betasM <- matrix(betas,nrow=ncol(YY),byrow = F)
    alphai <-  XX%*%t(betasM)
    alphai[alphai<0|is.na(alphai)] <- 10^(-5)
    
    # alpha0 <- alpha
    logYY <- log(YY)
    ## gradiente
    
    g <- betas
    for(i in 1:ncol(betasM)){
      for(j in 1:nrow(betasM)){
        g[(i-1)*nrow(betasM)+j] <-  sum((digamma(apply(alphai,1,sum)) -  digamma(alphai[,j]) + logYY[,j])*XX[,i])
      }
    }
    
    i <- 1
    j <- 1
    round(g,2)
    ## HESSIAN
    H <- matrix(0,ncol=length(betas),nrow=length(betas))
    for(i in 1:ncol(betasM)){
      for(j in 1:nrow(betasM)){
        for(ii in 1:ncol(betasM)){
          for(jj in 1:nrow(betasM)){
            linhah <- (i-1)*nrow(betasM)+j
            colunah <- (ii-1)*nrow(betasM)+jj
            # print(paste0(cont, ": linha=",linhah," e coluna=",colunah))
            # print(paste0("--->aj=",jj," e xk=",ii," e xk=",i))
            if(linhah==colunah){
              H[linhah,colunah] <- sum((digamma1(apply(alphai,1,sum)) - digamma1(alphai[,jj]))*XX[,ii]*XX[,i])
            }else{
              H[linhah,colunah] <- sum((digamma1(apply(alphai,1,sum)))*XX[,ii]*XX[,i])
            }
          }
        }
      }
    }
    
    betas <- beta0 - solve(H, g)
    betas <- beta0 + oldfac * (betas - beta0)
    conv <- max(abs(beta0 - betas))
    if (progress) {
      print(paste(iter, paste0(betas,collapse = "|"), round(conv,2)))
    }
    iter <- iter + 1
    utils::flush.console()
  }
  res <- list(beta = betas)
  return(res)
}

### apenas intercepto (caso particular)
XCOVAR <- matrix(XX[,1],nrow = nrow(XX))
modelo_proposto <- mletest(YY,XCOVAR,progress = F,eps = .01)
modelo_padrao <- dirichlet.mle(YY,progress = F,eps = .01)

modelo_padrao$alpha
modelo_proposto$beta
### regressor (caso geral)

set.seed(17092017)
basebetas <- NULL
i <- 2015
for(i in 2010:2015){
  amostra <- which(base_result_jogadores_modelo$season==i)
  modelo <- mletest(YY[amostra,],XX[amostra,],progress = T,
                    eps = .001,convcrit = .005)
  
  basebetas_temp <- modelo$beta
  basebetas_temp <- c(basebetas_temp , i)
  betamatrix <- matrix(modelo$beta,nrow=3)
  
  
  YYr <- DR_data(YY[amostra,])
  base_result_jogadores_modeloREG <- base_result_jogadores_modelo %>% filter(season==2015)
  plot(YYr, cex = 0.5, a2d = list(colored = FALSE, c.grid = FALSE))
  lake1 <- DirichReg(YYr ~ 1, base_result_jogadores_modeloREG)
  
  betamatrix2 <- matrix(lake1$coefficients,nrow=3,byrow = T)
  
  
  YYhat <- XX[amostra,] %*% t(betamatrix)
  Yhat <- apply(YYhat,1,which.max)
  Yt <- apply(YY[amostra,],1,which.max)
  cross <- table(Yt,Yhat)
  
  basebetas_temp <- c(basebetas_temp,sum(diag(cross)) / sum(cross))
  YYhat <- XX[amostra,] %*% t(betamatrix2)
  Yhat <- apply(YYhat,1,which.max)
  Yt <- apply(YY[amostra,],1,which.max)
  cross <- table(Yt,Yhat)
  basebetas_temp <- c(basebetas_temp,sum(diag(cross)) / sum(cross))
  basebetas <- rbind(basebetas,basebetas_temp)
  res$estimate
  print(i)  
}


library(nmet)
install.packages("nmet")

### validação
gradients <- function(parm){
  ai <- matrix(parm,nrow= nrow(YYr),byrow=T,ncol = ncol(YYr)) 
  Ai <- apply(ai,1,sum)
  veroi <- sum(apply(digamma(Ai) + digamma(ai),2,sum))
  return(veroi)
  
}


gradients2 <- function(parm){
  g <- parm
  logYY <- log(YYr)
  for(i in 1:ncol(XXr)){
    for(j in 1:ncol(YYr)){
      g[(i-1)*nrow(betasM)+j] <-  sum((digamma(apply(ai,1,sum)) -  digamma(ai[,j]) + logYY[,j])*XXr[,i])
    }
  }
  
}




amostra <- which((base_result_jogadores_modelo  %>% select(season))==2015)
YYr <- DR_data(YY[amostra,])
XXr <- matrix(XX[amostra,],ncol = ncol(XX))

logDirichreg <- function(parm){
  aiM <- matrix(parm,nrow=ncol(YYr),byrow = F)
  ai <-  XXr%*%t(aiM)
  
  ai <- matrix(parm,nrow= nrow(YYr),byrow=T,ncol = ncol(YYr))
  Ai <- apply(ai,1,sum)
  
  v1 <- apply((ai-1)*log(YYr)-log(gamma(ai)),1,sum)
  veroi <- sum(-gamma(Ai) + v1)
  return(veroi)
  
}

logDirichregreg <- function(parm){
  betas <- matrix(parm,nrow=ncol(YYr),byrow = F)
  ai <-  exp(XXr%*%t(betas))
  ai <- matrix(parm,nrow= nrow(YYr),byrow=T,ncol = ncol(YYr))
  Ai <- apply(ai,1,sum)
  v0 <- (ai-1)*log(YYr)-log(gamma(ai))
  v1 <- apply(v0,1,sum)
  veroi <- sum(-gamma(Ai) + v1) + lambda * sum(parm^2)
  return(veroi)
  
}


A <- diag(rep(1,ncol(XXr)))
B <- matrix(rep(0,ncol(XXr)),ncol = 1)



lambda <- 10
res1 <- maxLik(logDirichreg,start = c(4,4,4),
       method = "SANN")
lambda <- 10
res2 <- maxLik(logDirichregreg,start = c(4,4,4,4,4,4,4,4,4,4,4,4),
              method = "SANN")
lambda <- 100
res3 <- maxLik(logDirichregreg,start = c(4,4,4,4,4,4,4,4,4,4,4,4),
               method = "SANN")

res1$estimate
res2$estimate
res3$estimate

# 
# ,
# constraints = list(ineqA = A,
#                    ineqB = B)
res$estimate
dirichlet.mle(YYr)


## teste modelo

set.seed(111)
nr <- 100
err <- matrix(rnorm(3*nr),ncol=3)
xr  <- matrix(rnorm(3*nr),ncol=3)
ai <- 0.75 + xr * .22
air <- exp(ai)
yr<-rdirichlet(nr,air)

res4 <- maxLik(logDirichregregt,start = c(4,4,4,4,4,4,4,4,4),
               method = "SANN")
res4$estimate
logDirichregregt <- function(parm){
  betas <- matrix(parm,nrow=ncol(yr),byrow = F)
  ai <-  xr%*%t(betas)
  ai <- matrix(parm,nrow= nrow(yr),byrow=T,ncol = ncol(yr))
  Ai <- apply(ai,1,sum)
  v0 <- (ai-1)*log(yr)-log(gamma(ai))
  v1 <- apply(v0,1,sum)
  veroi <- sum(-gamma(Ai) + v1) + lambda * sum(parm^2)
  return(veroi)
}