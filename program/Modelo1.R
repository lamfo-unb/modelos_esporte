rm(list = ls())
library(dplyr)
library(data.table)
base <- readRDS("data/result/base_modelo_bayes01.rds")


## Modelo teste, passe curto 

temporada <- 2014


base %>%
  group_by(resultado) %>% filter(season==temporada) %>%
  summarise(valor = mean(`Passe curto`))

varsmodel <- c("Passe curto")
varsmodely <- c("VA","Emp","VB")
xr  <- data.matrix(cbind(1,base %>% filter(season==temporada) %>% select(varsmodel)))
yr <-  data.matrix(cbind(as.numeric(base %>% filter(season==temporada) %>%
                                      select(resultado) == "VA"),
                         as.numeric(base %>% filter(season==temporada) %>%
                                      select(resultado) == "Emp"),
                         as.numeric(base %>% filter(season==temporada) %>%
                                      select(resultado) == "VB")))
yrt <- DR_data(yr)
dxr <- data.frame(xr)

## Funções 


logDirichregregt <- function(parm){
  yr <- (yr*(nrow(yr) - 1) + 1/ncol(yr))/(nrow(yr))
  parmreg <- matrix(parm,nrow=ncol(yr),byrow = F)
  ai <- xr %*% t(parmreg)
  ai <- exp(ai)
  Ai <- apply(ai,1,sum)
  v0 <- ((ai-1)*log(yr)-log(gamma(ai)))
  v1 <- apply(v0,1,sum)
  veroi <- sum(log(gamma(Ai)) + v1)
  ## regularização ridge&lasso
  veroi <- veroi - k*(alpha_regulas*sum((parm^2)) + (1-alpha_regulas)*sum((parm^2)))
  return(veroi)
}

## função gradiente ----

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

## hessian ----

hessian <- function(parm){
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



## digamma1 ----


digamma1 <- function(x,h=.001){
  ( digamma(x+h) - digamma(x-h) ) / (2*h)
}



# 
# ## modelo tradicional ----
# res1_0 <- DirichReg(yrt ~ Passe.curto,data = dxr,model = "common")
# matrix(res1_0$coefficients,nrow=ncol(yr),byrow = T)
# res1_0$hessian
# 
# 
# ## modelo proposto
# res4 <- maxLik(logDirichregregt,start = c(0.1,0.1,
#                                           0.1,0.1,
#                                           0.1,0.1),
# 
#                method = "SANN")
# res4$estimate
# sigmasest <- diag(solve(res4$hessian))
# 
# ## parametros 
# matrix(res4$estimate,nrow=ncol(yr),byrow = F)


tabela_geral_temp <- NULL


### rodando modelo
ks <- seq(0,1,by = .1)
alpha_regulas <- seq(0,1,by = .1)


temps <- 2014

base <- data.table(base)
base[,ultimo_result :=c(NA, resultado[-.N]),by = c("season","Casa")]
base[,ultimo_result :=ifelse(is.na(ultimo_result)|ultimo_result=="Emp",0,
                             ifelse(ultimo_result=="VA",1,-1))]

base[,ultimo_result_va :=as.numeric(ultimo_result==1)]
base[,ultimo_result_vb :=as.numeric(ultimo_result==-1)]
base[,ultimo_result_emp :=as.numeric(ultimo_result==0)]
base[,ultimo_result:=NULL]

resultado_foward <- NULL
varsmodelo <- setdiff(names(base),c("season","Casa","Fora","resultado","Perna ruim","ultimo_result_emp"))
is <- NULL
i <- 1
s <- 2015
for(s in 2010:2015){
  logref <- -Inf
  is <- NULL
  for(i in 1:33){
  is_temp <- c(is,i)
  varsmodel <- varsmodelo[is_temp]
  varsmodely <- c("VA","Emp","VB")
  xr  <- data.matrix(cbind(1,base %>% filter(season==s) %>% select(varsmodel)))
  yr <-  data.matrix(cbind(as.numeric(base %>% filter(season==s) %>%
                                        select(resultado) == "VA"),
                           as.numeric(base %>% filter(season==s) %>%
                                        select(resultado) == "Emp"),
                           as.numeric(base %>% filter(season==s) %>%
                                        select(resultado) == "VB")))
    
    res_temp <- maxLik(logDirichregregt,start =rep(.01,ncol(xr)*ncol(yr)) ,
                   method = "SANN")
    
    
    parms_temp <- matrix(res_temp$estimate,nrow=3,byrow = F)
    ahat <- exp(xr %*% t(parms_temp))
    yrhat <- factor(apply(ahat,1,which.max),levels = 1:3)
    yrt <- factor(apply(yr,1,which.max),levels = 1:3)
    cross <- table(yrt,yrhat)
    tabela_temp <- sum(diag(cross)) / sum(cross)
  
    verossimilhanca <- res_temp$maximum

    resultado_foward <- rbind(resultado_foward,
                              data.table(temporada = s,
                                         variaveis = paste0(varsmodel,collapse = ";"),
                                         logvero = verossimilhanca,
                                         diagonal = tabela_temp))
    if(logref<verossimilhanca){
      logref <- verossimilhanca
      is <- is_temp
      
      
    }
    print(paste0("Iteração ",i," e temporada ",s))
  }
}


  # plot(DR_data(ahat), cex=.5,
  #       a2d=list(colored=FALSE,
  #                  c.grid=FALSE),col = yrt )
  

  
## as
dxr <- data.frame(xr)
res1_0 <- DirichReg(DR_data(yr) ~ Passe.curto,data = dxr,model = "common")
anova(res1_0)
res1_0$logLik
factor(apply(res1_0$fitted.values$mu,1,which.max),levels = 1:3)


