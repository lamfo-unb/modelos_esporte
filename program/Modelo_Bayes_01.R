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
jogadores[,`Perna ruim`:=as.numeric(`Perna ruim`)]


jogadores <- gather(jogadores %>%
                      select(one_of(vars)),
                    key = variavel, value = valor,-time_match3,-id_jogador,-temporada)

jogadores <- jogadores %>%
  mutate(season=as.numeric(gsub("fifa(..).*","\\1",temporada,perl=T))+2000) 


jogadores_time <- jogadores %>% 
  dplyr::group_by(season,time_match3,variavel) %>%
  dplyr::summarise(valor= mean(as.numeric(valor)))




fileinput <- "data/games"
arquivos <- list.files(path = fileinput,pattern = paste0('resultado_'),
                       full.names = T)

base <- data.table()
for(i in 1:length(arquivos)){
  base_temp <- readRDS(arquivos[i])
  base <- rbind(base,base_temp)
}



## pegando posição ----


fileinput <- "data/class"
arquivos <- list.files(path = fileinput,pattern = paste0('S'),
                       full.names = T)


base_class <- data.table()
for(i in 1:length(arquivos)){
  base_temp <- readRDS(arquivos[i])
  base_class <- rbind(base_class,base_temp)
}


### corrigindo nomes dos times

base_class <- base_class %>%
  select(match,season,Date,Time,pos_home,pos_visit,`Home team`,`Visiting team`) %>%
  mutate(dia_rodada = gsub("...\\s\\t*(.*)","\\1",Date))


base_class$`Home team` <- ifelse(base_class$`Home team`=="Blackburn Rvrs","Blackburn Rovers",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Blackburn Rovers","Blackburn",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Birm. City" | base_class$`Home team`=="Birmingham City","Birmingham",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Manchester City","Man City",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Manchester United" | base_class$`Home team`=="Manchester Utd." | base_class$`Home team`=="Manchester Utd","Man Utd",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Newcastle United" | base_class$`Home team`=="Newcastle Utd","Newcastle",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Tottenham Hotspur" | base_class$`Home team`=="Spurs","Tottenham",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="West Bromwich","West Brom",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="West Ham United","West Ham",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Wolverhampton Wanderers" | base_class$`Home team`=="Wolverhampton","Wolverhampton Wanderers",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Wigan Athletic","Wigan",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Swansea City","Swansea",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Norwich City","Norwich",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Leicester City","Leicester",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Cardiff City","Cardiff",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Bolton Wanderers","Bolton",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="QPR","Queens Park Rangers",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Arsenal FC","Arsenal",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Chelsea FC","Chelsea",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Reading FC","Reading",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Portsmouth FC","Portsmouth",base_class$`Home team`)
base_class$`Home team` <- ifelse(base_class$`Home team`=="Charlton Athletic","Charlton",base_class$`Home team`)



base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Blackburn Rvrs","Blackburn Rovers",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Blackburn Rovers","Blackburn",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Birm. City" | base_class$`Visiting team`=="Birmingham City","Birmingham",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Manchester City","Man City",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Manchester United" | base_class$`Visiting team`=="Manchester Utd." | base_class$`Visiting team`=="Manchester Utd","Man Utd",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Newcastle United" | base_class$`Visiting team`=="Newcastle Utd","Newcastle",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Tottenham Hotspur" | base_class$`Visiting team`=="Spurs","Tottenham",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="West Bromwich","West Brom",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="West Ham United","West Ham",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Wolverhampton Wanderers" | base_class$`Visiting team`=="Wolverhampton","Wolverhampton Wanderers",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Wigan Athletic","Wigan",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Swansea City","Swansea",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Norwich City","Norwich",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Leicester City","Leicester",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Cardiff City","Cardiff",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Bolton Wanderers","Bolton",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="QPR","Queens Park Rangers",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Arsenal FC","Arsenal",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Chelsea FC","Chelsea",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Reading FC","Reading",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Portsmouth FC","Portsmouth",base_class$`Visiting team`)
base_class$`Visiting team` <- ifelse(base_class$`Visiting team`=="Charlton Athletic","Charlton",base_class$`Visiting team`)




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
                  select(tipo,season,id_time,id_jogo,dia_rodada),tipo,id_time)

base_resultados <- left_join(base2,base1,
                   by = c("season","id_jogo"))
rm(base1,base_temp,base2)

## juntando bases


times_fifa <- unique(jogadores_time %>% ungroup() %>% select(time_match3))
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



##colocando o id_time no base de jogadores ----
jogadores_time <- left_join(jogadores_time,
                            tabela_times %>%
                              select("time_match3","id_time"),
                            by = "time_match3")

jogadores <- left_join(jogadores,
                            tabela_times %>%
                              select("time_match3","id_time"),
                            by = "time_match3")


### colocando classificação

base_class <- base_class %>%
  left_join(tabela_times %>%
              select(id_time,time_match3),by = c("Home team"="time_match3")) 

setnames(base_class,"id_time","Casa")

base_class <- base_class %>%
  left_join(tabela_times %>%
              select(id_time,time_match3),by = c("Visiting team"="time_match3")) 

setnames(base_class,"id_time","Fora")



base_resultados <- base_resultados %>%
  left_join(base_class %>%
              select(season,dia_rodada,pos_home,Casa),by = c("season","dia_rodada","Casa"))


base_resultados <-base_resultados %>%
  left_join(base_class %>%
              select(season,dia_rodada,pos_visit,Fora),by = c("season","dia_rodada","Fora"))

base_resultados <- base_resultados %>%
  arrange(season,Casa,id_jogo) %>%
  mutate(pos_home_ant = lag(pos_home))
  

base_resultados <- base_resultados %>%
  arrange(season,Fora,id_jogo) %>%
  mutate(pos_visit_ant = lag(pos_visit)) 


base_resultados <- base_resultados %>%
  mutate(pos_home_ant = ifelse(is.na(pos_home_ant),0,pos_home_ant) ,
         pos_visit_ant = ifelse(is.na(pos_visit_ant),0,pos_visit_ant))


base_resultados <- base_resultados %>%
  mutate(pos = as.numeric(pos_home_ant)-as.numeric(pos_visit_ant)) %>%
  select(season,id_jogo,Casa,Fora,resultado,pos)



## corrigir buracos na base (Expandgrid) ----

jogadores_completo <- merge(unique(jogadores_time %>% ungroup() %>% select(season)),
               unique(tabela_times %>% filter(!is.na(time_match3)) %>% ungroup() %>% select(time_match3)),by=NULL) %>%
  merge(unique(jogadores_time %>% ungroup() %>% select(variavel)),by=NULL) %>%
  left_join(tabela_times %>% ungroup() %>% select(time_match3,id_time),by = c("time_match3")) %>%
  left_join(jogadores_time %>% ungroup() %>% 
              select(season,id_time,variavel,valor,variavel),
            by = c("season","id_time","variavel"))

## completando a frente
jogadores_completo <- data.table(jogadores_completo)
jogadores_completo <- jogadores_completo[order(time_match3,variavel,season)]
jogadores_completo[, lag.value:=ifelse(is.na(valor),shift(valor,type="lag"),valor),
      , by = .(time_match3,variavel) ]

for(i in 2009:2017){
  jogadores_completo[, lag.value:=ifelse(is.na(lag.value),shift(lag.value,type="lag"),lag.value),
        , by = .(time_match3,variavel) ]
}
## completando a trás
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


## adicionando informações do time casa na base de resultados ----

base_result_jogadores <- base_resultados %>% left_join(jogadores_completo,
                                                       by = c("season"="season","Casa"="id_time"))


base_result_jogadores<- base_result_jogadores %>%
  mutate(valor_casa = lag.value2,
         time_casa = time_match3) %>%
  select(-lag.value,-valor,-lag.value2,-time_match3)

base_resultados <- data.table(base_resultados)


## adicionando informações visitante
base_result_jogadores <- base_result_jogadores %>% left_join(jogadores_completo,
                                                       by = c("season"="season","Fora"="id_time","variavel"="variavel"))

base_result_jogadores<- base_result_jogadores %>%
  mutate(valor_fora = lag.value2,
         time_fora = time_match3) %>%
  select(-lag.value,-valor,-lag.value2,-time_match3)

## diferença entre skills
base_result_jogadores<- base_result_jogadores %>%
  mutate(valor = valor_casa- valor_fora)


base_result_jogadores_modelo <- spread(base_result_jogadores %>%
                               select(season,Casa,Fora,resultado,pos,variavel,valor),key = variavel,value = valor)



## Modelo ----
saveRDS(base_result_jogadores_modelo,"data/result/base_modelo_bayes01.rds")






## adicionando informações de jogadores


base2 <- base_result_jogadores_modelo %>%
  mutate(N = ifelse(season >= temporada,0,1)) %>%
  group_by(Casa,Fora,resultado) %>%
  summarise(N = sum(N,na.rm=T))

base2 <- base2 %>%
  filter(Casa %in% unique(base[season == temporada]$id_time),
         Fora %in% unique(base[season == temporada]$id_time))


base2 <- spread(base2,resultado,N,fill=0)

base2 <- data.table(base2)
base2[,`:=`(VA_g=sum(VA,na.rm=T),
                     VB_g=sum(VB,na.rm=T),
                     Emp_g=sum(Emp,na.rm=T))
               ,by = "Casa"]



## adicionando informações do passado



hiper_total <- NULL
  parst <- pars_total
  parst <- parst %>% select(-Evento)
  tm <- base_result_jogadores_modelo %>%
    filter(season==temporada) %>% mutate(Int = 1)
  Fora <- tm$Fora
  Casa <- tm$Casa
  tm <-tm %>%
    select(one_of(names(parst)))

    if(nrow(parst)==0){
    hiper <- data.table(VA_m= rep(0,nrow(tm)) ,
                        Emp_m = rep(0,nrow(tm)),
                        VB_m = rep(0,nrow(tm)))
  }else{
    hiper <- round(as.matrix(tm)%*%t(parst),0)
    for(j in 1:3){
      hiper[,j] <- ifelse(hiper[,j]<1,1,hiper[,j])
    }
    hiper <- data.table(hiper)
    setnames(hiper,names(hiper),c("VA_m","Emp_m","VB_m"))
  }
  hiper$Casa <- Casa
  hiper$Fora <- Fora
  hiper_total <- rbind(hiper_total,hiper)


base2 <- left_join(base2,hiper_total,
                   by = c("Casa","Fora"))

dados <- base2
result <- base2
dados2<- data.table(time = unique(base %>% filter(season == temporada) %>% select(id_time)))
setnames(dados2,names(dados2),"times")


### CRIANDO UM BANCO DE DADOS PARA HOSPEDAR AS POSICOES
pos <- rbind.data.frame(as.character(dados2$time))
pos_g <- rbind.data.frame(as.character(dados2$time))
pos_m <- rbind.data.frame(as.character(dados2$time))
for(h in 1:20){
  pos[,h]=as.numeric(pos[,h])
  pos_g[,h]=as.numeric(pos_g[,h])
  pos_m[,h]=as.numeric(pos_m[,h])
}


## SIMULANDO OS RESULTADOS DOS JOGOS DADO A PRIORI
for(k in 1:100){
  resul=NULL
  prob=NULL
  resul_g=NULL
  prob_g=NULL
  resul_m=NULL
  prob_m=NULL
  for(i in 1:length(dados$Fora)){
    prob[[i]]<-rdirichlet(1,alpha=c(dados$VA[i]+1,dados$VB[i]+1,dados$Emp[i]+1))
    resul[[i]]<-rmultinom(1,1,prob=c(prob[[i]][1],prob[[i]][2],prob[[i]][3]))
    rownames(resul[[i]])<-c(as.character(dados$Casa[i]),as.character(dados$Fora[i]),"TIE")
    prob_g[[i]]<-rdirichlet(1,alpha=c(dados$VA_g[i]+1,dados$VB_g[i]+1,dados$Emp_g[i]+1))
    resul_g[[i]]<-rmultinom(1,1,prob=c(prob_g[[i]][1],prob_g[[i]][2],prob_g[[i]][3]))
    rownames(resul_g[[i]])<-c(as.character(dados$Casa[i]),as.character(dados$Fora[i]),"TIE")
    prob_m[[i]]<-rdirichlet(1,alpha=c(dados$VA_m[i]+1,dados$VB_m[i]+1,dados$Emp_m[i]+1))
    resul_m[[i]]<-rmultinom(1,1,prob=c(prob_m[[i]][1],prob_m[[i]][2],prob_m[[i]][3]))
    rownames(resul_m[[i]])<-c(as.character(dados$Casa[i]),as.character(dados$Fora[i]),"TIE")
  }
  
  dados2<-data.frame(time=names(table(dados$Casa)),
                     pts=rep(0,times=20),
                     pts_g=rep(0,times=20),
                     pts_m=rep(0,times=20))
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
    
    ## modelo
    
    if(resul_m[[i]][1,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul_m[[i]][1,])==dados2$time[j]){
          dados2$pts_m[j]<-dados2$pts_m[j]+3
        }
        else{dados2$pts_m[j]<-dados2$pts_m[j]}
      }
    }
    # SE TIME B VENCER      
    if(resul_m[[i]][2,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul_m[[i]][2,])==dados2$time[j]){
          dados2$pts_m[j]<-dados2$pts_m[j]+3
        }
        else{dados2$pts_m[j]<-dados2$pts_m[j]}
      }
    }  
    # SE OCORRER EMPATE
    if(resul_m[[i]][3,]==1){
      for(j in 1:length(dados2$time)){
        if(names(resul_m[[i]][1,])==dados2$time[j] ){
          dados2$pts_m[j]<-dados2$pts_m[j]+1
        }
        if(names(resul_m[[i]][2,])==dados2$time[j] ){
          dados2$pts_m[j]<-dados2$pts_m[j]+1
        }
        else{dados2$pts[j]<-dados2$pts[j]}
      }
    } 
    
  }
  
  pos[k,]=rank(-dados2$pts, ties.method = "min")
  pos_g[k,]=rank(-dados2$pts_g, ties.method = "min")
  pos_m[k,]=rank(-dados2$pts_m, ties.method = "min")
  print(k)
}


saveRDS(pos, file="data/result/forecast_bayes_naive.rds")
saveRDS(pos_g, file="data/result/forecast_bayes_naive_geral.rds")
saveRDS(pos_m, file="data/result/forecast_bayes_dirichlet.rds")

### ANALISE DESCRITIVA DOS RESULTADOS

### ABRINDO O BANCO DE DADOS
pos <- readRDS("data/result/forecast_bayes_naive.rds")
pos_g <- readRDS("data/result/forecast_bayes_naive_geral.rds")
pos_m <- readRDS("data/result/forecast_bayes_dirichlet.rds")

### OBTENDO AS CHANCES DE META
chances=data.frame(Time=names(pos),
                   Titulo=rep(0,times=20),
                   Champions=rep(0,times=20),
                   Rebaixamento=rep(0,times=20),
                   Titulo_g=rep(0,times=20),
                   Champions_g=rep(0,times=20),
                   Rebaixamento_g=rep(0,times=20),
                   Titulo_m=rep(0,times=20),
                   Champions_m=rep(0,times=20),
                   Rebaixamento_m=rep(0,times=20))
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
  tab_m=as.data.frame(prop.table(table(pos_m[,i])))
  tab_m$Var1=as.numeric(as.character(tab_m$Var1))
  chances[i,8]=sum(tab_m$Freq[tab_m$Var1==1],na.rm = T)
  chances[i,9]=sum(tab_m$Freq[tab_m$Var1<=6],na.rm = T)
  chances[i,10]=sum(tab_m$Freq[tab_m$Var1>=16],na.rm = T)
}

chances <- chances %>% 
     mutate(id_time = gsub("X\\.(.*)\\.","\\1",Time,perl = T) )

chances <- left_join(chances,tabela_times,
                     by = "id_time")

