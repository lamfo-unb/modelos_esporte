XX  <- cbind(1,base_result_jogadores_modelo$`Passe curto`)
YY <- cbind(as.numeric(base_result_jogadores_modelo$resultado == "VA"),
            as.numeric(base_result_jogadores_modelo$resultado == "Emp"),
            as.numeric(base_result_jogadores_modelo$resultado == "VB"))


digamma1 <- function(x,h=.001){
  ( digamma(x+h) - digamma(x-h) ) / (2*h)
}


mletest <- function (YY,XX, weights = NULL, eps = 10^(-5), convcrit = 1e-05, 
          maxit = 1000, oldfac = 0.3, progress = FALSE) {
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
  
  ###chute inicial

  lambda <- 1
  ## RIDGE
  lambdas <- rep(lambda,ncol(XX))
  betas <- t(solve(t(XX)%*%XX + diag(lambdas))%*%(t(XX)%*%YY))
  
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
    alphai[alphai<0] <- 10^(-10)
    
    # alpha0 <- alpha
    logYY <- log(YY)
    ## gradiente
    M1 <- matrix(digamma(apply(alphai,1,sum)),ncol=ncol(YY),nrow = nrow(YY))
    g <-  t(M1 - digamma(alphai) + logYY)%*%XX
    gvector <- NA
    for(i in 1:ncol(XX)){
      gvector <- c(gvector,g[,i])
    }
    gvector<-gvector[-1]
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

    betas <- beta0 - solve(H, gvector)
    betas <- beta0 + oldfac * (betas - beta0)
    conv <- max(abs(beta0 - betas))
    if (progress) {
      print(paste(iter, paste0(round(betas,2),collapse = "|"), round(conv,2)))
    }
    iter <- iter + 1
    utils::flush.console()
  }
  res <- list(beta = betas)
  return(res)
}

modelo <- mletest(YY,matrix(XX[,1],ncol=1),progress = T)

dirichlet.mle(YY,progress = T)
