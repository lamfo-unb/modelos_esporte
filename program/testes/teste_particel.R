

timesfor <- unique((base_result_jogadores_modelo %>% filter(season != temporada))$Casa)
pars_total <- NULL

for(j in timesfor){
base_teste <- base_result_jogadores_modelo %>% filter(Casa == j & season != temporada)
  r <- 100
  m <- 25
  A <- NULL
  W <- NULL
  set.seed(17092017)
  for(i in 1:r){
    amostra <- base_teste[sample(nrow(base_teste),m,replace = T),]
    A_t <- table(factor(amostra$resultado,levels = unique(base_teste$resultado)))
    covars <- setdiff(names(amostra),c("Casa","Fora","season","resultado"))
    W_t <- apply(amostra %>% select(covars),2,"mean")
    
    A <- rbind(A,A_t)
    W <- rbind(W,W_t)
  }
  i <- 1
  pars <- matrix(NA,nrow=ncol(A),ncol = ncol(W) + 1)
  for(i in 1:ncol(A)){
    lambda <- 1
    XX <- cbind(1,W)
    Y <- matrix(A[,i],ncol= 1 )
    ## RIDGE
    lambdas <- rep(lambda,ncol(XX))
    pars[i,] <- t(solve(t(XX)%*%XX + diag(lambdas))%*%(t(XX)%*%Y))
  }
  
  pars <- data.table(pars)
  setnames(pars,names(pars),c("Int",covars))
  pars$Casa <- j
  pars$Evento <- colnames(A)
  pars_total <- rbind(pars_total,pars)
  print(j)
}


covars <- setdiff(names(base_result_jogadores_modelo),c("Casa","Fora","season","resultado"))
varsmodel <- covars[22]

  base_teste <- base_result_jogadores_modelo %>% filter( season != temporada)
  r <- 1000
  m <- 50
  A <- NULL
  W <- NULL
  i <- 1
  set.seed(17092017)
  for(i in 1:r){
    amostra <- base_teste[sample(nrow(base_teste),m,replace = T),]
    A_t <- table(factor(amostra$resultado,levels = unique(base_teste$resultado)))
    W_t <- apply(amostra %>% select(varsmodel),2,"mean")
    
    A <- rbind(A,A_t)
    W <- rbind(W,W_t)
  }
  i <- 1
  pars <- matrix(NA,nrow=ncol(A),ncol = ncol(W) + 1)
  for(i in 1:ncol(A)){
    lambda <- 1
    XX <- cbind(1,W)
    Y <- matrix(A[,i],ncol= 1 )
    ## RIDGE
    lambdas <- rep(lambda,ncol(XX))
    pars[i,] <- t(solve(t(XX)%*%XX + diag(lambdas))%*%(t(XX)%*%Y))
  }
  
  pars <- data.table(pars)
  setnames(pars,names(pars),c("Int",varsmodel))
  pars$Evento <- colnames(A)

  pars_total <- pars
