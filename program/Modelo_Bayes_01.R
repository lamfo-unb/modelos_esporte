library(data.table)
library(dplyr)
library(tidyverse)
library(readxl)
library(gtools)
library(stringr)
rm(list = ls())
gc()


temporada <- 2016
### IMPORTANDO INFORMAÇÕES DOS JOGADORES ----
fileinput <- "data/static"
jogadores <- readRDS("data/static/dados_jogadores_ordenado.rds")  


vars <- c(setdiff(names(jogadores),c(grep("time|Time",names(jogadores),value = T),
                                     "nome_jogador","Posições",
                                     "Data de nascimento")),"time_match3","temporada")
jogadores <- jogadores %>%
  select(one_of(vars))

jogadores[,Altura:=gsub("\\scm","",Altura,perl=T)]
jogadores[,Peso:=gsub("\\scm","",Altura,perl=T)]
jogadores[,`Perna boa`:=ifelse(`Perna boa`=="Dir.",1,0)]
jogadores[,`Perna ruim`:=ifelse(`Perna ruim`=="Dir.",1,0)]


jogadores <- gather(jogadores %>%
                      select(one_of(vars)),
                    key = variavel, value = valor,-time_match3,-id_jogador,-temporada)

jogadores <- jogadores %>%
  mutate(season=as.numeric(gsub("fifa(..).*","\\1",temporada,perl=T))+2000) 

detach(package:plyr)
jogadores_time <- jogadores %>% 
  group_by(season,time_match3,variavel) %>%
  summarise(valor= mean(as.numeric(valor)))




fileinput <- "data/games"
arquivos <- list.files(path = fileinput,pattern = paste0('resultado_'),
                       full.names = T)

base <- data.table()
for(i in 1:length(arquivos)){
  base_temp <- readRDS(arquivos[i])
  base <- rbind(base,base_temp)
}
## só não encontrou Wolverhampton Wanderers
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

base_resultados <- left_join(base2,base1,
                   by = c("season","id_jogo"))
rm(base1,base_temp,base2)

## juntando bases
times_fifa <- unique(jogadores_time %>%ungroup() %>% select(time_match3))
tabela_times <- unique(base %>% select(id_time,nome_time))
tabela_times$id_match <- 1:nrow(tabela_times)


# fazendo batimento
v1 <- NULL
for(i in 1:nrow(times_fifa)){
  v1_temp <- grep(times_fifa$time_match3[i],tabela_times$nome_time)
  if(length(v1_temp)==1){
    v1 <- c(v1,v1_temp)
  }else{
    v1 <- c(v1,NA)
  }
}
times_fifa$id_match <- v1

## ajustando na mão
times_fifa$id_match <- ifelse(times_fifa$time_match3=="Man Utd",
                              grep("Manchester United",tabela_times$nome_time),times_fifa$id_match)
times_fifa$id_match <- ifelse(times_fifa$time_match3=="Man City",
                              grep("Manchester City",tabela_times$nome_time),times_fifa$id_match)


tabela_times <- left_join(tabela_times,times_fifa ,
                          by = "id_match")

## só não encontrou Wolverhampton Wanderers
tabela_times <- tabela_times %>%
  mutate(time_match3 = ifelse(is.na(time_match3),nome_time,time_match3))

##colocando o id_time no base de jogadores ----
jogadores_time <- left_join(jogadores_time,
                            tabela_times %>%
                              select("time_match3","id_time"),
                            by = "time_match3")

jogadores <- left_join(jogadores,
                            tabela_times %>%
                              select("time_match3","id_time"),
                            by = "time_match3")

## corrigir buracos na base (Expandgrid) ----

jogadores_completo <- merge(unique(jogadores_time %>% ungroup() %>% select(season)),
               unique(tabela_times %>% filter(!is.na(time_match3)) %>% ungroup() %>% select(time_match3)),by=NULL) %>%
  merge(unique(jogadores_time %>% ungroup() %>% select(variavel)),by=NULL) %>%
  left_join(tabela_times %>% ungroup() %>% select(time_match3,id_time),by = c("time_match3")) %>%
  left_join(jogadores_time %>% ungroup() %>% 
              select(season,id_time,variavel,valor,variavel),
            by = c("season","id_time","variavel"))


jogadores_completo <- data.table(jogadores_completo)
jogadores_completo <- jogadores_completo[order(time_match3,variavel,season)]
jogadores_completo[, lag.value:=ifelse(is.na(valor),shift(valor,type="lag"),valor),
      , by = .(time_match3,variavel) ]

for(i in 2009:2017){
  jogadores_completo[, lag.value:=ifelse(is.na(lag.value),shift(lag.value,type="lag"),lag.value),
        , by = .(time_match3,variavel) ]
}
## ordem inversa
jogadores_completo <- jogadores_completo[order(rev(time_match3),rev(variavel),rev(season))]
jogadores_completo[, lag.value2:=ifelse(is.na(lag.value),shift(lag.value,type="lag"),lag.value),
      , by = .(time_match3,variavel) ]

for(i in 2009:2017){
  jogadores_completo[, lag.value2:=ifelse(is.na(lag.value2),shift(lag.value2,type="lag"),lag.value2),
        , by = .(time_match3,variavel) ]
}

# BASE PROJETADA + OU -
sum(is.na(jogadores_completo$valor))
sum(is.na(jogadores_completo$lag.value))
sum(is.na(jogadores_completo$lag.value2))

## substituindo por zero caso não tenha
jogadores_completo[,lag.value2:=ifelse(is.na(lag.value2),0,lag.value2)]
sum(is.na(jogadores_completo$lag.value2))


## adicionando informações do time casa na base de resultados

base_result_jogadores <- base_resultados %>% left_join(jogadores_completo,
                                                       by = c("season"="season","Casa"="id_time"))
base_result_jogadores<- base_result_jogadores %>%
  mutate(valor_casa = lag.value2,
         time_casa = time_match3) %>%
  select(-lag.value,-valor,-lag.value2,-time_match3)


base_result_jogadores <- base_result_jogadores %>% left_join(jogadores_completo,
                                                       by = c("season"="season","Fora"="id_time","variavel"="variavel"))

base_result_jogadores<- base_result_jogadores %>%
  mutate(valor_fora = lag.value2,
         time_fora = time_match3) %>%
  select(-lag.value,-valor,-lag.value2,-time_match3)

## diferença
base_result_jogadores<- base_result_jogadores %>%
  mutate(valor = valor_casa- valor_fora)


base_result_jogadores_modelo <- spread(base_result_jogadores %>%
                               select(season,Casa,Fora,resultado,variavel,valor),key = variavel,value = valor)


## Modelo ----
saveRDS(base_result_jogadores_modelo,"data/result/base_modelo_bayes01.rds")


jogadores_completo <- spread(jogadores_completo %>%
                               select(-valor,-lag.value),key = variavel,value = lag.value2)


## adicionando informações de jogadores
base_result_jogadores <- base_resultados %>% left_join(jogadores_completo,
                             by = c("season"="season","Casa"="id_time"))

base2 <- base2 %>%
  mutate(N = ifelse(season >= temporada,0,1)) %>%
  group_by(Casa,Fora,resultado) %>%
  summarise(N = sum(N,na.rm=T))

base2 <- base2 %>%
  filter(Casa %in% unique(base[season == temporada]$id_time),
         Fora %in% unique(base[season == temporada]$id_time))


base2 <- spread(base2,resultado,N,fill=0)
rm(base1)
base2 <- data.table(base2)
base2[,`:=`(VA_g=sum(VA,na.rm=T),
                     VB_g=sum(VB,na.rm=T),
                     Emp_g=sum(Emp,na.rm=T))
               ,by = "Casa"]

dados <- base2
result <- base2
dados2<- data.table(time = unique(base %>% filter(season == temporada) %>% select(id_time)))
setnames(dados2,names(dados2),"times")






### CRIANDO UM BANCO DE DADOS PARA HOSPEDAR AS POSICOES
pos <- rbind.data.frame(as.character(dados2$time))
pos_g <- rbind.data.frame(as.character(dados2$time))
for(h in 1:20){
  pos[,h]=as.numeric(pos[,h])
  pos_g[,h]=as.numeric(pos_g[,h])
}

k <- 1
i <- 1
## SIMULANDO OS RESULTADOS DOS JOGOS DADO A PRIORI
for(k in 1:100){
  resul=NULL
  prob=NULL
  resul_g=NULL
  prob_g=NULL
  for(i in 1:length(dados$Fora)){
    prob[[i]]<-rdirichlet(1,alpha=c(dados$VA[i]+1,dados$VB[i]+1,dados$Emp[i]+1))
    resul[[i]]<-rmultinom(1,1,prob=c(prob[[i]][1],prob[[i]][2],prob[[i]][3]))
    rownames(resul[[i]])<-c(as.character(dados$Casa[i]),as.character(dados$Fora[i]),"TIE")
    prob_g[[i]]<-rdirichlet(1,alpha=c(dados$VA_g[i]+1,dados$VB_g[i]+1,dados$Emp_g[i]+1))
    resul_g[[i]]<-rmultinom(1,1,prob=c(prob_g[[i]][1],prob_g[[i]][2],prob_g[[i]][3]))
    rownames(resul_g[[i]])<-c(as.character(dados$Casa[i]),as.character(dados$Fora[i]),"TIE")
  }
  
  dados2<-data.frame(time=names(table(dados$Casa)),
                     pts=rep(0,times=20),
                     pts_g=rep(0,times=20))
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
    
    ## geral (g)
    
    # SE TIME A VENCER    
    if(resul_g[[i]][1,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul_g[[i]][1,])==dados2$time[j]){
          dados2$pts_g[j]<-dados2$pts_g[j]+3
        }
        else{dados2$pts_g[j]<-dados2$pts_g[j]}
      }
    }
    # SE TIME B VENCER      
    if(resul_g[[i]][2,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul_g[[i]][2,])==dados2$time[j]){
          dados2$pts_g[j]<-dados2$pts_g[j]+3
        }
        else{dados2$pts_g[j]<-dados2$pts_g[j]}
      }
    }  
    # SE OCORRER EMPATE
    if(resul_g[[i]][3,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul_g[[i]][1,])==dados2$time[j] ){
          dados2$pts_g[j]<-dados2$pts_g[j]+1
        }
        if(names(resul_g[[i]][2,])==dados2$time[j] ){
          dados2$pts_g[j]<-dados2$pts_g[j]+1
        }
        else{dados2$pts_g[j]<-dados2$pts_g[j]}
      }
    } 
  }
  
  pos[k,]=rank(-dados2$pts, ties.method = "min")
  pos_g[k,]=rank(-dados2$pts_g, ties.method = "min")
  print(k)
}


saveRDS(pos, file="data/result/forecast_bayes_naive.rds")
saveRDS(pos_g, file="data/result/forecast_bayes_naive_geral.rds")


### ANALISE DESCRITIVA DOS RESULTADOS

### ABRINDO O BANCO DE DADOS
pos <- readRDS("data/result/forecast_bayes_naive.rds")
pos_g <- readRDS("data/result/forecast_bayes_naive_geral.rds")

### OBTENDO AS CHANCES DE META
chances=data.frame(Time=names(pos),
                   Titulo=rep(0,times=20),
                   Champions=rep(0,times=20),
                   Rebaixamento=rep(0,times=20),
                   Titulo_g=rep(0,times=20),
                   Champions_g=rep(0,times=20),
                   Rebaixamento_g=rep(0,times=20))
for(i in 1:20){
  tab=as.data.frame(prop.table(table(pos[,i])))
  tab$Var1=as.numeric(as.character(tab$Var1))
  chances[i,2]=sum(tab$Freq[tab$Var1==1],na.rm = T)
  chances[i,3]=sum(tab$Freq[tab$Var1<=6],na.rm = T)
  chances[i,4]=sum(tab$Freq[tab$Var1>=16],na.rm = T)
  tab_g=as.data.frame(prop.table(table(pos_g[,i])))
  tab_g$Var1=as.numeric(as.character(tab_g$Var1))
  chances[i,5]=sum(tab_g$Freq[tab_g$Var1==1],na.rm = T)
  chances[i,6]=sum(tab_g$Freq[tab_g$Var1<=6],na.rm = T)
  chances[i,7]=sum(tab_g$Freq[tab_g$Var1>=16],na.rm = T)
}

chances <- chances %>% 
     mutate(id_time = gsub("X\\.(.*)\\.","\\1",Time,perl = T) )





chances <- left_join(chances,tabela_times,
                     by = "id_time")

