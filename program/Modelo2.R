rm(list = ls())
# install.packages("maxLik")
library(dplyr)
library(data.table)
library(stringr)
library(maxLik)
base <- readRDS("data/base_modelo_bayes01.rds")
base <- data.table(base)

## Funções ----

logDirichregregt <- function(parm){
  yr <- (yr*(nrow(yr) - 1) + 1/ncol(yr))/(nrow(yr))


  # parm[indzero] <- 0 

  # nrealx <- (ncol(xr)-1)/2
  # indzero <- ncol(yr) +c((0:(nrealx-1))*ncol(yr) + 2, ##zerando difereça empate
  #                        nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + 1, ##zerando similaridade casa
  #                        nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + ncol(yr)) ##zerando similaridade fora
  # 
  
  parmreg <- matrix(parm,nrow=ncol(yr),byrow = F)
  
  ai <- xr %*% t(parmreg)
  # ai[,2] <- (cbind(xr[,1],exp(-sigma*(xr[,-1])^2)) %*% t(parmreg))[,2]
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

### rodando modelo


# base[,ultimo_result :=c(NA, resultado[-.N]),by = c("season","Casa")]
# base[,ultimo_result :=ifelse(is.na(ultimo_result)|ultimo_result=="Emp",0,
#                              ifelse(ultimo_result=="VA",1,-1))]
# 
# base[,ultimo_result_va :=as.numeric(ultimo_result==1)]
# base[,ultimo_result_vb :=as.numeric(ultimo_result==-1)]
# base[,ultimo_result_emp :=as.numeric(ultimo_result==0)]
# base[,ultimo_result:=NULL]


varsmodelo <- setdiff(names(base),c("season","Casa","Fora","resultado","Perna ruim",
                                    "Duração Do Contrato"))



scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}
# base <- base[,(varsmodelo) := lapply(.SD, scale_this), .SDcols = varsmodelo] 


i <- 1
j <- 1
s <- 2010:2012 ## mudar para 2008:2012
s_out <- 2013:2014


### Encontrando sigma de cada variável (teste)




## ordem de seleção
logref <- -Inf
acurraciaref <- 0
is_geral <- NULL
varsmodelorest <- varsmodelo

base_temp_j <- NULL
k <- .5
alpha_regula <- .5
sigma <- 1
 i <- 1
for(i in 1:length(varsmodelorest)){
  is_temp <- c(is_geral,i)
  varadd <- varsmodelorest[i]
  varsmodel <- varsmodelo[is_temp]
  varsmodely <- c("VA","Emp","VB")
  xr  <- cbind(1,base %>% filter(season %in% s) %>% select(varsmodel))
  # xr <- data.matrix(cbind(xr,exp(-sigma*(xr[,(1:length(varadd))+1]^2))))
  xr <- data.matrix(xr)
  yr <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s) %>%
                                        select(resultado) == "VA"),
                           as.numeric(base %>% filter(season%in% s) %>%
                                        select(resultado) == "Emp"),
                           as.numeric(base %>% filter(season%in% s) %>%
                                        select(resultado) == "VB")))
  
  
  # nrealx <- (ncol(xr)-1)/2
  # indzero <- ncol(yr) +c((0:(nrealx-1))*ncol(yr) + 2, ##zerando difereça empate
  #                        nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + 1, ##zerando similaridade casa
  #                        nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + ncol(yr)) ##zerando similaridade fora
  # 
  # 
  # vec1 <- rep(0,ncol(xr)*ncol(yr))
  # vec1[indzero] <- 1
  # A <- diag(vec1)
  # B <- matrix(0,nrow = ncol(xr)*ncol(yr),ncol = 1)
  # A <- A[indzero,]
  # B <- B[indzero,]

  inicial_pars <- rep(.01,ncol(xr)*ncol(yr))
  # inicial_pars[indzero] <- 0
  
  res_temp <- maxLik(logDirichregregt,start = inicial_pars,
                     method = "SANN")
  
  theta_temp <-res_temp$estimate
  etheta_temp <-diag(-solve(res_temp$hessian))^(.5)
  varsmodel_temp <- c("Int",varsmodel)
  nometheta_temp_generico <- rep(paste0("b",str_pad(1:ncol(xr)-1,"000")),
                                 each=ncol(yr))
  nometheta_temp_Y <- rep(varsmodely,ncol(xr))
  nometheta_temp_X <- rep(varsmodel_temp,
                          each=ncol(yr))
  nometheta_temp <- paste(nometheta_temp_Y,
                          nometheta_temp_X,sep="_")
  
  
  parms_temp <- matrix(theta_temp,nrow=ncol(yr),byrow = F)
  ahat <- exp(xr %*% t(parms_temp))
  yrhat <- factor(apply(ahat,1,which.max),levels = 1:ncol(yr))
  yrt <- factor(apply(yr,1,which.max),levels = 1:ncol(yr))
  cross <- table(yrt,yrhat)
  acurracia <- sum(diag(cross)) / sum(cross)
  acurracias <- data.table(t(diag(cross)/apply(cross,1,sum)))
  setnames(acurracias,names(acurracias),
           paste0(varsmodely,"_acuracia"))
  cross <- data.frame(cross)
  verossimilhanca <- res_temp$maximum
  base_temp <- cbind(data.table(rodada = j,
                                ind = i,
                                k = k,
                                alpha = alpha_regula,
                                varadd = varadd,
                                variaveis = paste0(varsmodel,collapse = ";"),
                                logvero = verossimilhanca,
                                logveroref =logref, 
                                acurracia = acurracia,
                                acurraciaref =acurraciaref),
                     acurracias)
  
  base_temp_j <- rbind(base_temp_j,base_temp) 
  
  print(paste0("----fim variável ",i," (ordenando)"))
}

# base_temp_j_espcial <- base_temp_j
ks <- seq(0,2,by = .1)
alpha_regulas <- seq(0,1,by = .1)

resultado_foward <- NULL
k <- 0.1
alpha_regula <- 0

# for(sigma in sigmas){
  for(k in ks){
    if(k==0){
      alpha_regulasf <- 0
    }else{
      alpha_regulasf <- alpha_regulas
    }
    for(alpha_regula in alpha_regulasf){

      
      
      logref <- -Inf
      acurraciaref <- 0
      varsmodelorest <- varsmodelo
      
      
      base_temp_j <- base_temp_j %>%
        arrange(desc(acurracia))
        j <- 1:27
        ### rodando modelo com variável adicionada
        varadd <- base_temp_j$varadd[j]
        indadd <- base_temp_j$ind[j]
        is_temp <- c(is_geral,indadd)
        
        varsmodel <- varsmodelo[is_temp]
        varsmodely <- c("VA","Emp","VB")
        xr  <- cbind(1,base %>% filter(season %in% s) %>% select(varsmodel))
        # xr <- data.matrix(cbind(xr,exp(-sigma*(xr[,(1:length(varadd))+1]^2))))
        xr <- data.matrix(xr)
        yr <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s) %>%
                                              select(resultado) == "VA"),
                                 as.numeric(base %>% filter(season%in% s) %>%
                                              select(resultado) == "Emp"),
                                 as.numeric(base %>% filter(season%in% s) %>%
                                              select(resultado) == "VB")))
        
        # nrealx <- (ncol(xr)-1)/2
        # indzero <- ncol(yr) +c((0:(nrealx-1))*ncol(yr) + 2, ##zerando difereça empate
        #                        nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + 1, ##zerando similaridade casa
        #                        nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + ncol(yr)) ##zerando similaridade fora
        # 
        # 
        # vec1 <- rep(0,ncol(xr)*ncol(yr))
        # vec1[indzero] <- 1
        # A <- diag(vec1)
        # B <- matrix(0,nrow = ncol(xr)*ncol(yr),ncol = 1)
        # A <- A[indzero,]
        # B <- B[indzero,]

        inicial_pars <- rep(0.01,ncol(xr)*ncol(yr))

        # inicial_pars[indzero] <- 0
        
        res_temp <- maxLik(logDirichregregt,start = inicial_pars ,
                           # constraints  = list( eqA = A, eqB = B ),
                           method = "BFGS",hess = hessian)

        theta_temp <-res_temp$estimate
        etheta_temp <-diag(-solve(res_temp$hessian))^(.5)
        varsmodel_temp <- c("Int",varsmodel)
        nometheta_temp_generico <- rep(paste0("b",str_pad(1:ncol(xr)-1,"000")),
                                       each=ncol(yr))
        nometheta_temp_Y <- rep(varsmodely,ncol(xr))
        nometheta_temp_X <- rep(varsmodel_temp,
                                each=ncol(yr))
        nometheta_temp <- paste(nometheta_temp_Y,
                                nometheta_temp_X,sep="_")
        
        
        parms_temp <- matrix(theta_temp,nrow=ncol(yr),byrow = F)
        ahat <- exp(xr %*% t(parms_temp))
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
          
          xrout  <- cbind(1,base %>% filter(season %in% s_out) %>% select(varsmodel))
          # xrout <- data.matrix(cbind(xrout,exp(-sigma*(xrout[,(1:length(varadd))+1]^2))))
           xrout <- data.matrix(xrout)
        
          yrout <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s_out) %>%
                                                   select(resultado) == "VA"),
                                      as.numeric(base %>% filter(season%in% s_out) %>%
                                                   select(resultado) == "Emp"),
                                      as.numeric(base %>% filter(season%in% s_out) %>%
                                                   select(resultado) == "VB")))
          
          
          parms_temp <- matrix(theta_temp,nrow=ncol(yr),byrow = F)
          ahat_out <- exp(xrout %*% t(parms_temp))
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
                                          varadd = paste0(varadd,collapse = ";"),
                                          variaveis = paste0(varsmodel,collapse = ";"),
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
          file_name <- paste0("T_K",k*10,"_A",alpha_regula*10,".rds")
          saveRDS(base_temp_f,file_name)
          resultado_foward <- rbind(resultado_foward,
                                base_temp_f)
      print(paste0("----Fim alpha=",alpha_regula," em k=",k))
    }
    print(paste0("---Fim alpha=",alpha_regula))
  }
#   print(paste0("---Fim sigma=",s))
# }
