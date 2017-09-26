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
dadosfifa<- unique(dadosfifa,by=c("variavel","valor","id_jogador","temporada"))
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
dadosfifa <- data.table(dadosfifa)

dadosfifa <- dadosfifa %>% 
  mutate(valor = ifelse(valor == '<span class="star"><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i>', "5",
                         ifelse(valor == '<span class="star"><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star-o fa-lg"></i>',"4",
                                ifelse(valor == '<span class="star"><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star-o fa-lg"></i><i class="fa fa-star-o fa-lg"></i>',3,
                                       ifelse(valor == '<span class="star"><i class="fa fa-star fa-lg"></i><i class="fa fa-star fa-lg"></i><i class="fa fa-star-o fa-lg"></i><i class="fa fa-star-o fa-lg"></i><i class="fa fa-star-o fa-lg"></i>',2,
                                              ifelse(valor == '<span class="star"><i class="fa fa-star fa-lg"></i><i class="fa fa-star-o fa-lg"></i><i class="fa fa-star-o fa-lg"></i><i class="fa fa-star-o fa-lg"></i><i class="fa fa-star-o fa-lg"></i>',1,
                                                     valor))))))

dadosfifa <- data.table(dadosfifa)
#removendo variaveis não pareadas
tabela1 <- table(dadosfifa$variavel,dadosfifa$temporada)
tabela1 <- rownames(tabela1[apply(tabela1,1,prod)==0,])

dadosfifa <- dadosfifa[!(variavel %in% tabela1),]

jogadores<- spread(dadosfifa, variavel, valor)
jogadores<- jogadores[with(jogadores, order(temporada)), ]

jogadores[,time1 := gsub("(.*);.*","\\1",Times)]
jogadores[,time2 := gsub(".*;(.*)","\\1",Times)]
lista_selecoes <- read_csv("data/lista_selecoes.txt",col_names = T,
                             locale = locale(encoding = "latin1"))
lista_selecoes_pt <- read_csv("data/lista_selecoes_pt.txt",col_names = T,
                           locale = locale(encoding = "latin1"))

library(stringi)

lista_selecoes <- stri_trans_general(c(tolower(lista_selecoes$TEAM),tolower(lista_selecoes_pt$TEAM))
                        ,"Latin-ASCII")
lista_selecoes <- c(lista_selecoes,"irlanda","rep. Of korea","united states")
jogadores[,time:= ifelse(stri_trans_general(tolower(time1),"Latin-ASCII") %in% lista_selecoes,
                         ifelse(stri_trans_general(tolower(time2),"Latin-ASCII") %in% lista_selecoes,
                                NA,time2),time1)]

saveRDS(jogadores,"data/static/dados_jogadores.rds")


## Ajuste manual

jogadores$time_match3 <- jogadores$time
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Blackburn Rvrs","Blackburn Rovers",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Blackburn Rovers","Blackburn",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Birm. City" | jogadores$time_match3=="Birmingham City","Birmingham",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Manchester City","Man City",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Manchester United" | jogadores$time_match3=="Manchester Utd." | jogadores$time_match3=="Manchester Utd","Man Utd",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Newcastle United" | jogadores$time_match3=="Newcastle Utd","Newcastle",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Tottenham Hotspur" | jogadores$time_match3=="Spurs","Tottenham",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="West Bromwich","West Brom",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="West Ham United","West Ham",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Wolverhampton Wanderers" | jogadores$time_match3=="Wolverhampton","Wolverhampton Wanderers",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Wigan Athletic","Wigan",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Swansea City","Swansea",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Norwich City","Norwich",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Leicester City","Leicester",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Cardiff City","Cardiff",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Bolton Wanderers","Bolton",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="QPR","Queens Park Rangers",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Arsenal FC","Arsenal",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Chelsea FC","Chelsea",jogadores$time_match3)
jogadores$time_match3 <- ifelse(jogadores$time_match3=="Reading FC","Reading",jogadores$time_match3)


jogadores$time_match3 <- ifelse(jogadores$time_match3=="Derby County","Derby",jogadores$time_match3)




jogadores1<- jogadores[with(jogadores, order(temporada,time_match3)), ]
saveRDS(jogadores1,"data/static/dados_jogadores_ordenado.rds")

apply(table(jogadores1$time_match3,jogadores1$temporada)>0,2,sum)

