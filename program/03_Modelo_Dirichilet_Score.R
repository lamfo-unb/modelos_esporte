rm(list = ls())
library(dplyr)
library(data.table)
library(stringr)
library(maxLik)
base <- readRDS("data/result/base_modelo_dirichilet_score.rds")
base <- data.table(base)

## Funções ----

logDirichregregt <- function(parm){
  yr <- (yr*(nrow(yr) - 1) + 1/ncol(yr))/(nrow(yr))
  parmreg <- matrix(parm,nrow=ncol(yr),byrow = F)
  
  ai <- xr %*% t(parmreg)
  ai[,2] <- (cbind(xr[,1],gama*exp(-sigma*(xr[,-1])^2)) %*% t(parmreg))[,2]
  ai <- exp(ai)
  Ai <- apply(ai,1,sum)
  v0 <- ((ai-1)*log(yr)-log(gamma(ai)))
  v1 <- apply(v0,1,sum)
  veroi <- sum(log(gamma(Ai)) + v1)
  ## regularização ridge&lasso
  veroi <- veroi - k*(alpha_regula*sum((parm^2)) +
                        (1-alpha_regula)*sum(abs(parm)))
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

## hessian 

hessian <- function(parm){
  parms_temp <- matrix(parm,nrow=ncol(yr),byrow = F)
  ai <- exp(xr %*% t(parms_temp))
  H <- matrix(0,ncol=length(parm),nrow=length(parm))
  parmreg <- matrix(parm,nrow=ncol(yr),byrow = F)
  for(i in 1:ncol(parmreg)){                # variando categoria 
    for(j in 1:nrow(parmreg)){              # variando covariáveis
      for(ii in 1:ncol(parmreg)){           # variando categoria 1
        for(jj in 1:nrow(parmreg)){
          linhah <- (i-1)*nrow(parmreg)+j
          colunah <- (ii-1)*nrow(parmreg)+jj# variando categoria 1
          if(linhah==colunah){
            H[linhah,colunah] <- sum(
              (xr[,i]*xr[,i]*ai[,jj])*(1 - digamma(ai[,jj]) - ai[,jj]*digamma1(ai[,jj]) + 
                                         digamma(apply(ai,1,sum)) + ai[,jj]*digamma1(apply(ai,1,sum))))
          }else{
            H[linhah,colunah] <- sum((digamma1(apply(ai,1,sum)))*xr[,ii]*xr[,i]*ai[,jj]*ai[,j])
          }
        }
      }
    }
  }
  return(H)
}

## digamma1 
digamma1 <- function(x,h=.001){
  ( digamma(x+h) - digamma(x-h) ) / (2*h)
}




scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

i <- 1
j <- 1
s <- 2013:2014
s_out <- 2015
s_t <- 2016


## Variáveis auxiliares
varsmodelo <- setdiff(names(base),c("season","Casa","Fora","score_casa",
                                  "score_fora"))
varsmodelo <- c("Aceleração","Perna boa","Posicion. GL","Perna ruim","Duração Do Contrato")

logref <- -Inf
acurraciaref <- 0
is_geral <- NULL
base_temp_j <- NULL

## Parâmetros estimate via grid search
ks <- seq(0,3,by = .5);k <- 0.5
alpha_regulas <- seq(0,1,by = .1);alpha_regula <- 0.5
sigmas <- seq(0.1,1,by= .1);sigma <- 1
gamas  <- seq(1,3,by= .5); gama <- 1

a <- expand.grid(ks,alpha_regulas,sigmas,gamas)
resultado_foward <- NULL
base$resultado <- base$score_casa-base$score_fora

for(gama in gamas){
  for(sigma in sigmas){
    for(k in ks[1]){
      if(k==0){
        alpha_regulasf <- 0
      }else{
        alpha_regulasf <- alpha_regulas
      }
      for(alpha_regula in alpha_regulasf){
        logref <- -Inf
        acurraciaref <- 0
          varsmodely <- c("VA","Emp","VB")
          xr  <- cbind(1,base %>% filter(season %in% s) %>% select(varsmodelo))
          xr <- data.matrix(xr)
          yr <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s) %>%
                                                     select(resultado) > 0),
                                        as.numeric(base %>% filter(season%in% s) %>%
                                                     select(resultado) == 0),
                                        as.numeric(base %>% filter(season%in% s) %>%
                                                     select(resultado) < 0)))
          
          inicial_pars <- rep(0.01,ncol(xr)*ncol(yr))
    
          res_temp <- maxLik(logDirichregregt,start = inicial_pars ,
                             method = "BFGS",hess = hessian)
          
          
    
          theta_temp <-res_temp$estimate
          etheta_temp <-diag(-solve(res_temp$hessian))^(.5)
          varsmodel_temp <- c("Int",varsmodelo)
          nometheta_temp_generico <- rep(paste0("b",str_pad(1:ncol(xr)-1,"000")),
                                         each=ncol(yr))
          nometheta_temp_Y <- rep(varsmodely,ncol(xr))
          nometheta_temp_X <- rep(varsmodel_temp,
                                  each=ncol(yr))
          nometheta_temp <- paste(nometheta_temp_Y,
                                  nometheta_temp_X,sep="_")
          
          
          parms_temp <- matrix(theta_temp,nrow=ncol(yr),byrow = F)
          ahat <- xr %*% t(parms_temp)
          ahat[,2] <- (cbind(xr[,1],gama*exp(-sigma*(xr[,-1])^2)) %*% t(parms_temp))[,2]
          ahat <- exp(ahat)
          yrhat <- factor(apply(ahat,1,which.max),levels = 1:ncol(yr))
          yrt <- factor(apply(yr,1,which.max),levels = 1:ncol(yr))
          cross <- table(yrt,yrhat)
          acurracia <- sum(diag(cross)) / sum(cross)
          acurracias <- data.table(t(diag(cross/apply(cross,1,sum))))
          setnames(acurracias,names(acurracias),
                   paste0(varsmodely,"_acuracia"))
          cross <- data.frame(cross)
          verossimilhanca <- res_temp$maximum
          
            
            
            ## erro out ----
            
            xrout  <- cbind(1,base %>% filter(season %in% s_out) %>% select(varsmodelo))
            xrout <- data.matrix(xrout)
            yrout <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s_out) %>%
                                                     select(resultado) >0),
                                        as.numeric(base %>% filter(season%in% s_out) %>%
                                                     select(resultado) == 0),
                                        as.numeric(base %>% filter(season%in% s_out) %>%
                                                     select(resultado) < 0)))
            
            
            parms_temp <- matrix(theta_temp,nrow=ncol(yr),byrow = F)
            ahat_out <- xrout %*% t(parms_temp)
            ahat_out[,2] <- (cbind(xrout[,1],gama*exp(-sigma*(xrout[,-1])^2)) %*% t(parms_temp))[,2]
            ahat_out <- exp(ahat_out)
            yrhat_out <- factor(apply(ahat_out,1,which.max),levels = 1:ncol(yrout))
            yrt_out <- factor(apply(yrout,1,which.max),levels = 1:ncol(yrout))
            cross_out <- table(yrt_out,yrhat_out)
            acurracia_out <- sum(diag(cross_out)) / sum(cross_out)
            acurracias_out <- data.table(t(diag(cross_out/apply(cross_out,1,sum))))
            setnames(acurracias_out,names(acurracias_out),
                     paste0(varsmodely,"_acuracia_out"))
            cross_out <- data.frame(cross_out)
            
            base_temp_f <- cbind(data.table(k = k,
                                            alpha = alpha_regula,
                                            varadd = paste0(varsmodelo,collapse = ";"),
                                            variaveis = paste0(varsmodelo,collapse = ";"),
                                            logvero = verossimilhanca,
                                            logveroref =logref, 
                                            acurracia = acurracia,
                                            acurraciaref =acurraciaref,
                                            nome_theta_est = nometheta_temp,
                                            theta_est= theta_temp,
                                            etheta_est = etheta_temp,
                                            acurracia_out = acurracia_out),
                                 acurracias,
                                 acurracias_out)
            file_name <- paste0("T_K",k*10,"_A",alpha_regula*10,"_S",sigma*10,"_G",gama*10,"_dissimilaridade.rds")
            saveRDS(base_temp_f,file_name)
            resultado_foward <- rbind(resultado_foward,
                                  base_temp_f)
        print(paste0("----Fim alpha=",alpha_regula," em k=",k))
      }
      print(paste0("---Fim alpha=",alpha_regula))
    }
    print(paste0("---Fim sigma=",sigma))
  }
  print(paste0("---Fim gamma=",gama))
}