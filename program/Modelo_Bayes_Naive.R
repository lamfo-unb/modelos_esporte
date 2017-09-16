library(data.table)
library(dplyr)
library(tidyverse)
library(readxl)
library(gtools)

rm(list = ls())
gc()


temporada <- 2016
fileinput <- "data/games"
arquivos <- list.files(path = fileinput,pattern = paste0('resultado_'),
                       full.names = T)

base <- data.table()
for(i in 1:length(arquivos)){
  base_temp <- readRDS(arquivos[i])
  base <- rbind(base,base_temp)
}

base <- base %>%
  select(tipo,nome_time,id_time,id_jogo,rodada,dia_rodada,hora_jogo,score,season)

base1 <- spread(base %>% 
                  select(tipo,season,score,id_jogo),tipo,score)
base1 <- base1 %>%
  mutate(resultado = as.numeric(Casa) - as.numeric(Fora),
         resultado = ifelse(resultado<0,"VB",
                            ifelse(resultado>0,"VA",
                                   'Emp'))) %>%
  select(season,id_jogo,resultado)

base2 <- spread(base %>% 
                  select(tipo,season,id_time,id_jogo),tipo,id_time)

base2 <- left_join(base2,base1,
                   by = c("season","id_jogo"))

base2 <- base2 %>%
  mutate(N = ifelse(season >= temporada,0,1)) %>%
  group_by(Casa,Fora,resultado) %>%
  summarise(N = sum(N,na.rm=T))

base2 <- base2 %>%
  filter(Casa %in% unique(base[season == temporada]$id_time),
         Fora %in% unique(base[season == temporada]$id_time))


base2 <- spread(base2,resultado,N,fill=0)
rm(base1)


dados <- base2
result <- base2
dados2<- data.table(time = unique(base %>% filter(season == temporada) %>% select(id_time)))
setnames(dados2,names(dados2),"times")
dados <- rbind.data.frame(base2,base2)



### CRIANDO UM BANCO DE DADOS PARA HOSPEDAR AS POSICOES
pos <- rbind.data.frame(as.character(dados2$time))

for(h in 1:20){
  pos[,h]=as.numeric(pos[,h])
}

k <- 1
i <- 1
## SIMULANDO OS RESULTADOS DOS JOGOS DADO A PRIORI
for(k in 1:1000){
  resul=NULL
  prob=NULL
  for(i in 1:length(dados$Fora)){
    prob[[i]]<-rdirichlet(1,alpha=c(dados$VA[i]+1,dados$VB[i]+1,dados$Emp[i]+1))
    resul[[i]]<-rmultinom(1,1,prob=c(prob[[i]][1],prob[[i]][2],prob[[i]][3]))
    rownames(resul[[i]])<-c(as.character(dados$Casa[i]),as.character(dados$Fora[i]),"TIE")
  }
  
  dados2<-data.frame(time=names(table(dados$Casa)),pts=rep(0,times=20))
  ## MONTANDO A CONFIGURACAO DE CLASSIFICACAO
  for(i in 1:length(resul)){
    # SE TIME A VENCER    
    if(resul[[i]][1,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul[[i]][1,])==dados2$time[j]){
          dados2$pts[j]<-dados2$pts[j]+3
        }
        else{dados2$pts[j]<-dados2$pts[j]}
      }
    }
    # SE TIME B VENCER      
    if(resul[[i]][2,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul[[i]][2,])==dados2$time[j]){
          dados2$pts[j]<-dados2$pts[j]+3
        }
        else{dados2$pts[j]<-dados2$pts[j]}
      }
    }  
    # SE OCORRER EMPATE
    if(resul[[i]][3,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul[[i]][1,])==dados2$time[j] ){
          dados2$pts[j]<-dados2$pts[j]+1
        }
        if(names(resul[[i]][2,])==dados2$time[j] ){
          dados2$pts[j]<-dados2$pts[j]+1
        }
        else{dados2$pts[j]<-dados2$pts[j]}
      }
    } 
  }
  
  pos[k,]=rank(-dados2$pts, ties.method = "min")
  print(k)
}


saveRDS(pos, file="data/result/forecast_bayes_naive.rds")


### ANALISE DESCRITIVA DOS RESULTADOS

### ABRINDO O BANCO DE DADOS
pos <- readRDS("data/result/forecast_bayes_naive.rds")

### OBTENDO AS CHANCES DE META
chances=data.frame(Time=names(pos),
                   Titulo=rep(0,times=20),
                   Champions=rep(0,times=20),
                   Rebaixamento=rep(0,times=20))
for(i in 1:20){
  tab=as.data.frame(prop.table(table(pos[,i])))
  tab$Var1=as.numeric(as.character(tab$Var1))
  chances[i,2]=sum(tab$Freq[tab$Var1==1])
  chances[i,3]=sum(tab$Freq[tab$Var1<=6])
  chances[i,4]=sum(tab$Freq[tab$Var1>=16])
}

chances <- chances %>% 
     mutate(id_time = gsub("X\\.(.*)\\.","\\1",Time,perl = T) )



tabela_times <- unique(base%>% select(id_time,nome_time))


chances <- left_join(chances,tabela_times,
                     by = "id_time")

