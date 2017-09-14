rm(list = ls())
gc()
# teste <- readRDS("C:/Users/JoaoVictor/Desktop/artigo_futebol/dados_static/base_group_games_2014.rds")
library(tidyr)
library(plyr)

library(tidyverse)
library(data.table)
library(stringr)
# View(dadosfifa[1123:1126,])

arquivos_pasta<- list.files("data/static",pattern = "sfifa",full.names = T)
dadosfifa <- NULL
i <- 1
for(i in 1:length(arquivos_pasta)){
  dados <-  readRDS(arquivos_pasta[i])
  dadosfifa <- rbind(dadosfifa,dados)
  
}
dadosfifa <- dadosfifa %>% select(variavel,valor,id_jogador,nome_jogador,temporada)
dadosfifa<- unique(dadosfifa,by=c("variavel","valor","id_jogador"))
dadosfifa<- dadosfifa[!which(dadosfifa$variavel=="Nº da camisa"),]
dadosfifa<- dadosfifa[!which(dadosfifa$variavel=="Posição"),]

## Reflexo
dadosfifa[,variavel:=ifelse(variavel=="Reflexos GL","Reflexos",variavel)]
## Manejo
dadosfifa[,variavel:=ifelse(variavel=="Manejo GL","Manejo",variavel)]
## carrinho e dividida
dadosfifac <- dadosfifa[variavel=="Dividida",]
dadosfifa[,variavel:=ifelse(variavel=="Dividida","Div. em pé",variavel)]
dadosfifac[,variavel:="Carrinho"]
dadosfifa <- rbind(dadosfifa,dadosfifac)
#removendo variaveis não pareadas
tabela1 <- table(dadosfifa$variavel,dadosfifa$temporada)
tabela1 <- rownames(tabela1[apply(tabela1,1,prod)==0,])

dadosfifa <- dadosfifa[!(variavel %in% tabela1),]

jogadores<- spread(dadosfifa, variavel, valor)
jogadores<- jogadores[with(jogadores, order(temporada)), ]





prod(1:4)
str_detect(jogadores$Times,";")
str_locate(jogadores$Times,";")[,1]
jogadores$time_match <-ifelse(str_detect(jogadores$Times,";"),str_sub(jogadores$Times,0,(str_locate(jogadores$Times,";")[,1]-1)),jogadores$Times)
saveRDS(jogadores,"C:/Users/JoaoVictor/Desktop/artigo_futebol/dados_static/dados_jogadores.rds")
levels(as.factor(jogadores$temporada))

times<- levels(as.factor(jogadores$time_match))
times <- times[-c(1,2,5,6,7,8,17,19,22,23,24,25,27,28,29,30,31,32,33,34,36,37,39,40,42,43,44,45,46,47,58,60,61,63,66,67,71,72,75,78,79,80)]
times <- times[-c(12,11,13,14,21,23,24,25,28,32,33,36,37)]
jogadores$time_match2 <- ifelse(jogadores$time_match %in% times,jogadores$time_match,str_sub(jogadores$Times,(str_locate(jogadores$Times,";")[,1]+1),str_length(jogadores$Times)))


jogadores$time_match3 <- ifelse(jogadores$time_match2=="Blackburn Rvrs","Blackburn Rovers",jogadores$time_match2)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Blackburn Rovers","Blackburn",jogadores$time_match2)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Birm. City" | jogadores$time_match3=="Birmingham City","Birmingham",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Manchester City","Man City",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Manchester United" | jogadores$time_match3=="Manchester Utd." | jogadores$time_match3=="Manchester Utd","Man Utd",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Newcastle United" | jogadores$time_match3=="Newcastle Utd","Newcastle",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Tottenham Hotspur" | jogadores$time_match3=="Tottenham","Spurs",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="West Bromwich","West Brom",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="West Ham United","West Ham",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Wolverhampton Wanderers" | jogadores$time_match3=="Wolverhampton","Wolves",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Wigan Athletic","Wigan",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Swansea City","Swansea",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Norwich City","Norwich",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Leicester City","Leicester",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Cardiff City","Cardiff",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Bolton Wanderers","Bolton",jogadores$time_match3)

jogadores1<- jogadores[with(jogadores, order(temporada,time_match3)), ]
saveRDS(jogadores1,"C:/Users/JoaoVictor/Desktop/artigo_futebol/dados_static/dados_jogadores_ordenado.rds")

jogadores2 <- spread(jogadores,time_match3 )
length(levels(as.factor(jogadores$temporada)))

base <- as.data.frame(1,NULL)
basefinal <- as.data.frame(NULL)
jogadores$time_match3 <- as.character(jogadores$time_match3)
for(l in 1:length(levels(as.factor(jogadores$temporada)))){
temporada <- jogadores[which(jogadores$temporada==levels(as.factor(jogadores$temporada))[l]),]
temporada <- as.data.table(temporada)
for (i in 1:length(levels(as.factor(temporada$time_match3)))){
  linhas<- which(temporada$time_match3==levels(as.factor(temporada$time_match3))[i])
  time <- temporada[linhas,]
  for(k in 1:nrow(time)){
    if(k==1){
   base <- cbind(base,time[k,])
    }else{
      base <- merge(base,time[k,],by = "time_match3")
    }
  }
  
  basefinal <- list(basefinal,as.data.frame(base))
  base <- as.data.frame(1,NULL)
  
}
}

