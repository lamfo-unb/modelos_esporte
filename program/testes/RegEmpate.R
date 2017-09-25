logref <- -Inf
acurraciaref <- 0
is_geral <- NULL
varsmodelorest <- varsmodelo
sigma <- .5
base_temp_j_emp <- NULL
k <- .5
alpha_regula <- .5
sigma <- .5
i <- c(22,1,5)
for(i in 1:length(varsmodelorest)){
  is_temp <- c(is_geral,i)
  varadd <- varsmodelorest[i]
  varsmodel <- varsmodelo[is_temp]
  varsmodely <- c("VA","Emp","VB")
 
  yr <-  data.matrix(cbind(as.numeric(base %>% filter(season%in% s) %>%
                                        select(resultado) == "VA"),
                           as.numeric(base %>% filter(season%in% s) %>%
                                        select(resultado) == "Emp"),
                           as.numeric(base %>% filter(season%in% s) %>%
                                        select(resultado) == "VB")))
  xr  <- cbind(1,
                           base %>% filter(season %in% s) %>% select(varsmodel))
  xr <- data.matrix(cbind(xr,exp(-sigma*(xr[,(1:length(varadd))+1]^2))))
  xr <- data.matrix(xr)
  
  
  nrealx <- (ncol(xr)-1)/2
  indzero <- ncol(yr) +c((0:(nrealx-1))*ncol(yr) + 2, ##zerando difereça empate
                         nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + 1, ##zerando similaridade casa
                         nrealx*ncol(yr)+ (0:(nrealx-1))*ncol(yr) + ncol(yr)) ##zerando similaridade fora


  vec1 <- rep(0,ncol(xr)*ncol(yr))
  vec1[indzero] <- 1
  A <- diag(vec1)
  B <- matrix(0,nrow = ncol(xr)*ncol(yr),ncol = 1)
  A <- A[indzero,]
  B <- B[indzero,]
  
  res_temp <- maxLik(logDirichregregt,start = rep(.1,ncol(xr)*ncol(yr)) ,
                     constraints  = list( eqA = A, eqB = B ),
                     method = "SANN")
  # method = "SANN",
  
  # library(DEoptim)
  # ?DEoptim
  # low <- rep(-1,ncol(xr)*ncol(yr))
  # upp <- rep(+1,ncol(xr)*ncol(yr))
  # low[indzero] <- 0
  # upp[indzero] <- 0
  # 
  # res_temp <- DEoptim(logDirichregregt,
  #                     lower = low,
  #                     upper = upp,
  #                     DEoptim.control())
  
  theta_temp <- rep(0,ncol(yr)*ncol(xr))
  theta_temp[-indzero] <-res_temp$estimate
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
  ahat <- xr %*% t(parms_temp)
  # ahat_emp <- cbind(xr[,1],
  #                   exp(-sigma*(xr[,-1])^2)) %*% t(parms_temp)
  # ahat[,2] <- ahat_emp[,2]
  ahat <- exp(ahat)
  
  yrhat <- factor(apply(ahat,1,which.max),levels = 1:ncol(yr))
  yrt <- factor(apply(yr,1,which.max),levels = 1:ncol(yr))
  cross <- table(yrt,yrhat)
  acurracia <- sum(diag(cross) / sum(cross))
  acurracias <- data.table(t(diag(cross/apply(cross,1,sum))))
  setnames(acurracias,names(acurracias),
           paste0(varsmodely,"_acuracia"))
  cross <- data.frame(cross)
  verossimilhanca <- res_temp$maximum
  base_temp <- cbind(data.table(ind = i,
                                k = k,
                                alpha = alpha_regula,
                                varadd = varadd,
                                variaveis = paste0(varsmodel,collapse = ";"),
                                logvero = verossimilhanca,
                                logveroref =logref, 
                                acurracia = acurracia,
                                acurraciaref =acurraciaref),
                     acurracias)
  
  base_temp_j_emp <- rbind(base_temp_j_emp,base_temp) 
  
  print(paste0("----fim variável ",i," (ordenando)"))
}


base_temp_j_emp <- base_temp_j_emp %>%
  arrange(desc(logvero))
