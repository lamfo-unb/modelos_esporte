rm(list = ls())
gc()
library(dplyr)
library(tidyverse)
library(data.table)

#### pegando resultados

arquivos_modelo <- list.files("data/result","T_.*_full.rds",full.names = T)
base_modelos_treino <- rbindlist(lapply(arquivos_modelo,readRDS))

resultados_modelos <- base_modelos_treino %>%
  dplyr::group_by(k,alpha) %>%
  dplyr::summarise(acurracia_out = mean(acurracia_out))
resultados_modelos <- data.table(resultados_modelos)


kd <- resultados_modelos %>% spread(value = acurracia_out,key =alpha)
xnames <- as.numeric(as.character(as.vector(kd[-1,1]$k)))
ynames <- as.numeric(as.character(as.numeric(names(kd[-1,-1]))))
kd <- as.matrix(kd[-1,-1])



kd <- list(x = xnames,
           y = ynames,
           z = kd)

p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()

image(kd$x, kd$y, kd$z)
contour(kd$x, kd$y, kd$z, col = "pink", add = TRUE, method = "edge",
        vfont = c("sans serif", "plain"))



## selecionando modelo
resultados_modelos <- resultados_modelos[acurracia_out==max(acurracia_out),]

# SAMPLE
set.seed(2609)
ind_mod <- sample(nrow(resultados_modelos),1)
resultados_modelos <- resultados_modelos[ind_mod,]

resultados_modelos <- resultados_modelos %>%
  left_join(base_modelos_treino,by = c("k","alpha"))

resultados_modelos <- resultados_modelos %>%
  mutate(par_x = gsub(".*_(.*)","\\1",nome_theta_est),
         par_y = gsub("(.*)_.*","\\1",nome_theta_est))

resultados_modelos_pars <-  resultados_modelos %>%
  select(par_x,par_y,theta_est)
### analise parâmetros


parm_matrix <- spread(resultados_modelos_pars, par_y, theta_est)
signmax <- function(x){
  return(sum(x[which.max(abs(x))]>0))
  
}


which.max.1 <- function(x){
  return(order(x,decreasing = T)[1])
}
which.max.2 <- function(x){
  return(order(x,decreasing = T)[2])
}
which.max.3 <- function(x){
  return(order(x,decreasing = T)[3])
}

which.min.1 <- function(x){
  return(order(x)[1])
}
which.min.2 <- function(x){
  return(order(x)[2])
}
which.min.3 <- function(x){
  return(order(x)[3])
}

a <- cbind(parm_matrix[,1],parm_matrix[,-1] %>% round(5))
vec_max1 <- (parm_matrix[parm_matrix$par_x!="Int",1])[apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,which.max.1)]
vec_max2 <- (parm_matrix[parm_matrix$par_x!="Int",1])[apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,which.max.2)]
vec_max3 <- (parm_matrix[parm_matrix$par_x!="Int",1])[apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,which.max.3)]

max_vars <- rbind(vec_max1,vec_max2,vec_max3)

vec_min1 <- (parm_matrix[parm_matrix$par_x!="Int",1])[apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,which.min.1)]
vec_min2 <- (parm_matrix[parm_matrix$par_x!="Int",1])[apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,which.min.2)]
vec_min3 <- (parm_matrix[parm_matrix$par_x!="Int",1])[apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,which.min.3)]

min_vars <- rbind(vec_min1,vec_min2,vec_min3)




valor_min <- apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,min)
valor_max <- apply(parm_matrix[parm_matrix$par_x!="Int",-1],2,max)



estatis_vars <- rbind(paste0(vec_max1," (",round(valor_max,5),")"),
                      paste0(vec_min1," (",round(valor_min,5),")"))
colnames(estatis_vars) <- c("Emp","VA","VB")


print(xtable(estatis_vars),include.rownames = F)

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

# html1 <- read_html(link, encoding = "UTF-8")
# 
# tables <- html_table(html1, fill = TRUE)
# all_match <- data.table()
# i <- 38
# for(i in 1:38){
#   ### JOGO 1
#   playoff <- data.table(tables[[i+3]][c(1,2,3,5,7)])
#   playoff <- playoff[!grepl(paste0("PM|AM|",temporada,"|",temporada+1),`Home team`),]
#   vecs <- c("Date","Time")
#   ## replicando datas e horários
#   playoff <- playoff[,c(paste0(vecs,"_")) := lapply(.SD,function(x) cumsum(x!="")),.SDcols = vecs]
#   for(j in vecs){
#     stage <- unique(playoff[eval(parse(text=paste0(j,"!=''"))),
#                             c(j,paste0(j,"_")),
#                             with=FALSE])
#     playoff[,(j):=NULL]
#     playoff <- stage[playoff,on=(paste0(j,"_"))]
#     playoff[,(paste0(j,"_")):=NULL]
#     rm(stage)
#   }
#   
#   
#   setnames(playoff,c("Home","Result"),
#            c("Result","Visiting team"))
#   
#   playoff[,pos_home:= gsub(".*\\((.*)\\..*","\\1",`Home team`)]
#   playoff[,pos_visit:= gsub(".*\\((.*)\\..*","\\1",`Visiting team`)]
#   playoff[,`Home team`:= gsub(".*\\(.*\\)\\s\\s(.*)","\\1",`Home team`, perl=T)]
#   playoff[,`Visiting team`:= gsub("(.*)\\s\\s\\(.*","\\1",`Visiting team`, perl=T)]
#   playoff[,score_home:= gsub("(.*)\\:.*","\\1",Result)]
#   playoff[,score_visit:= gsub(".*\\:(.*)","\\1",Result)]
#   playoff[,match:=i]
#   playoff[,season:=temporada]
#   
#   all_match <- rbind(all_match,playoff)
# }
name_file <- paste0("S",temporada,".rds")
pathout <- "data/games"
# saveRDS(all_match,
#         file.path(pathout,
#                   name_file))

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
for(k in 1:10000){

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
      res_temp <- rmultinom(1,1,prob=c(exp(xrt %*% parm_matrix)[j,1],
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

