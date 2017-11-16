rm(list = ls())
gc()
library(dplyr)
library(tidyverse)
library(data.table)

#### pegando resultados
resultados_modelos <- readRDS("data/result/modelo_est_similaridade.rds")
gama <- unique(resultados_modelos$gama) 
sigma<- unique(resultados_modelos$sigma) 

resultados_modelos <- resultados_modelos %>%
  mutate(par_x = gsub(".*_(.*)","\\1",nome_theta_est),
         par_y = gsub("(.*)_.*","\\1",nome_theta_est))

resultados_modelos_pars <-  resultados_modelos %>%
  select(par_x,par_y,theta_est)

parm_matrix <- spread(resultados_modelos_pars, par_y, theta_est)
s <- 2012:2014
s_out <- 2015
s_t <- 2016

varsmodelo <- setdiff(parm_matrix$par_x,"Int")
rownames(parm_matrix) <- parm_matrix$par_x
parm_matrix <- parm_matrix %>% select(-par_x)
parm_matrix <- data.matrix(parm_matrix)



## baixando tabela com jogos de 2017

temporada <- 2017

link <- paste0("https://www.transfermarkt.com/premier-league/gesamtspielplan/wettbewerb/GB1?saison_id=",temporada,
               "&spieltagVon=1&spieltagBis=38")

library(RCurl)
library(data.table)
library(gsubfn)
library(stringr)
library(purrr)
library(rvest) # the new package, version 0.3.0
library(RCurl)
library(XML)
library(dplyr)

name_file <- paste0("S",temporada,".rds")
pathout <- "data/games"
all_match <- readRDS(file.path(pathout,
                               name_file))


## Simulando primeiro confronto
#rodada1

base_forecast <- readRDS("data/static/dados_media_time.rds")
base_forecast <- data.table(base_forecast)
base_forecast <- base_forecast[,max_season:=max(season),by = time_match3] %>% 
  filter(season == max_season ) %>%
  select(time_match3,variavel,valor)

## Ajustando nomes time ----


all_match$`Home team` <- ifelse(all_match$`Home team`=="Blackburn Rvrs","Blackburn Rovers",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Blackburn Rovers","Blackburn",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Birm. City" | all_match$`Home team`=="Birmingham City","Birmingham",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Manchester City","Man City",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Manchester United" | all_match$`Home team`=="Manchester Utd." | all_match$`Home team`=="Manchester Utd","Man Utd",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Newcastle United" | all_match$`Home team`=="Newcastle Utd","Newcastle",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Tottenham Hotspur" | all_match$`Home team`=="Spurs","Tottenham",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="West Bromwich","West Brom",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="West Ham United","West Ham",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Wolverhampton Wanderers" | all_match$`Home team`=="Wolverhampton","Wolverhampton Wanderers",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Wigan Athletic","Wigan",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Swansea City","Swansea",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Norwich City","Norwich",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Leicester City","Leicester",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Cardiff City","Cardiff",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Bolton Wanderers","Bolton",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="QPR","Queens Park Rangers",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Arsenal FC","Arsenal",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Chelsea FC","Chelsea",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Reading FC","Reading",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Portsmouth FC","Portsmouth",all_match$`Home team`)
all_match$`Home team` <- ifelse(all_match$`Home team`=="Charlton Athletic","Charlton",all_match$`Home team`)



all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Blackburn Rvrs","Blackburn Rovers",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Blackburn Rovers","Blackburn",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Birm. City" | all_match$`Visiting team`=="Birmingham City","Birmingham",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Manchester City","Man City",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Manchester United" | all_match$`Visiting team`=="Manchester Utd." | all_match$`Visiting team`=="Manchester Utd","Man Utd",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Newcastle United" | all_match$`Visiting team`=="Newcastle Utd","Newcastle",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Tottenham Hotspur" | all_match$`Visiting team`=="Spurs","Tottenham",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="West Bromwich","West Brom",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="West Ham United","West Ham",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Wolverhampton Wanderers" | all_match$`Visiting team`=="Wolverhampton","Wolverhampton Wanderers",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Wigan Athletic","Wigan",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Swansea City","Swansea",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Norwich City","Norwich",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Leicester City","Leicester",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Cardiff City","Cardiff",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Bolton Wanderers","Bolton",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="QPR","Queens Park Rangers",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Arsenal FC","Arsenal",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Chelsea FC","Chelsea",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Reading FC","Reading",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Portsmouth FC","Portsmouth",all_match$`Visiting team`)
all_match$`Visiting team` <- ifelse(all_match$`Visiting team`=="Charlton Athletic","Charlton",all_match$`Visiting team`)





base_simula <- data.table()
set.seed(11112017)
k <- 1
i <- 1
for(k in 1:100){

  classificacao_times <- data.table(time = unique(all_match$`Home team`),
                                    pt = 0)
  
  
  classificacao_times[,match:=0]
  classificacao_times[,resultado:=NA]
  
  classificacao_times[,pos:=rank(pt,ties.method = "min"),by=match]
  i <- 1
  for(i in 1:38){
    rodada1 <- all_match[match==i,]
    rodada1 <- rodada1 %>% left_join(base_forecast,
                                     by = c("Home team"="time_match3"))
    
    setnames(rodada1,"valor","valor_casa")
    
    rodada1 <- rodada1 %>% left_join(base_forecast,
                                     by = c("Visiting team"="time_match3",
                                            "variavel"="variavel"))
    setnames(rodada1,"valor","valor_fora")
    
    rodada1 <- rodada1 %>%
      mutate(valor = valor_casa-valor_fora)
    
    
    setnames(rodada1,c("Home team","Visiting team"),
             c("Casa","Fora"))
    
    
    
  
    classificacao_times_prox <- classificacao_times %>% filter(match==(i-1)) %>%
      mutate(match = i)
    classificacao_times_prox <- data.table(classificacao_times_prox)
    # classificacao_times_prox[,pos:=rank(pt,ties.method = "min"),by=match]
    classificacao_times_prox[,resultado:=NULL]
    
    base <- spread(rodada1 %>% 
                     select(season,match,Date,Casa,Fora,variavel,valor),
                   key = variavel,value = valor)
    rm(rodada1)
    base <- base %>% left_join(classificacao_times_prox %>% select(-pt),
                               by=c("match"="match",
                                    "Casa"="time"))
    setnames(base,"pos","pos_casa")
  
    
    base <- base %>% left_join(classificacao_times_prox %>% select(-pt),
                               by=c("match"="match",
                                    "Fora"="time"))
    setnames(base,"pos","pos_fora")
    
    base <- base %>%
      mutate(pos = pos_casa - pos_fora)
    
    
    parm_matrix <- spread(resultados_modelos_pars, par_y, theta_est)
    
    varsmodelo <- parm_matrix$par_x
    xrt  <- (cbind("Int"=1,base) %>% select(varsmodelo))
    ## colocando na mesma ordem dos parâmetros
    xrt <- data.matrix(xrt)
    
    rownames(parm_matrix) <- varsmodelo
    parm_matrix <- parm_matrix %>% select(-par_x)
    parm_matrix <- data.matrix(parm_matrix)
    
    yrt_est <- rep(NA,10)
    for(j in 1:10){
      
      xrt_empate <- gama*exp(-sigma*xrt^2)
      xrt_empate[,"Int"] <- 1
      res_temp <- rmultinom(1,1,prob=c(exp(xrt_empate %*% parm_matrix)[j,1],
                                       exp(xrt %*% parm_matrix)[j,2],
                                       exp(xrt %*% parm_matrix)[j,3]))
      yrt_est[j] <- (c("Emp","VA","VB"))[which(res_temp==1)]
    }
    
  
    
    base$resultado <- yrt_est
    
    classificacao_times_prox <- classificacao_times_prox %>%
      left_join(base %>% select(Casa,match,resultado),
                by=c("match"="match",
                     "time"="Casa"))
    
    classificacao_times_prox <- classificacao_times_prox %>%
      left_join(base %>% select(Fora,match,resultado),
                by=c("match"="match",
                     "time"="Fora"))
    
    classificacao_times_prox <- classificacao_times_prox %>% 
      mutate(pt =ifelse(is.na(resultado.x),pt,
                        ifelse(resultado.x=="VA",pt+3,
                               ifelse(resultado.x == "Emp",pt+1,pt))))
    
    classificacao_times_prox <- classificacao_times_prox %>% 
      mutate(pt =ifelse(is.na(resultado.y),pt,
                        ifelse(resultado.y=="VB",pt+3,
                               ifelse(resultado.y == "Emp",pt+1,pt))))
    
    classificacao_times_prox <- classificacao_times_prox %>% 
      mutate(resultado =ifelse(is.na(resultado.x),resultado.y,
                               resultado.x)) %>%
      select(-resultado.x,-resultado.y)
    classificacao_times_prox <- data.table(classificacao_times_prox)
    classificacao_times_prox[,pos:=21-rank(pt,ties.method = "min")]
    classificacao_times <- rbind(classificacao_times,classificacao_times_prox)
  }
  
  base_simula_temp <- data.table(classificacao_times %>% filter(match==38) %>% select(-resultado))
  base_simula_temp[,pos:=21-rank(pt,ties.method = "first")]
  base_simula_temp$simula <- k
  
  base_simula <- rbind(base_simula,base_simula_temp)
  print(k)
}

saveRDS(base_simula,"data/result/tabela_est_similaridade.rds")

prob_champions <- function(x){
  return(sum(x[1:4])/sum(x))
}
prob_campeao <- function(x){
  return(x[1]/sum(x))
}
prob_europa <- function(x){
  return(sum(x[5:6])/sum(x))
}
prob_rebaixa <- function(x){
  return(sum(x[18:20])/sum(x))
}

resultado_simula <- table(base_simula$time,base_simula$pos)

tabela_final <- cbind(campeao=apply(resultado_simula,1,prob_campeao)*100,
      champions=apply(resultado_simula,1,prob_champions)*100,
      europa=apply(resultado_simula,1,prob_europa)*100,
      rebaixa=apply(resultado_simula,1,prob_rebaixa)*100)

nomes_times <- rownames(tabela_final)
tabela_final <- cbind(data.table(time=nomes_times),tabela_final)


library(xtable)
print(xtable(tabela_final[rev(order(campeao))],digits = 2),include.rownames = F)

