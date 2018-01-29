library(data.table)
library(dplyr)
library(tidyverse)
library(readxl)
library(gtools)
library(stringr)
rm(list = ls())
gc()

## dados para transformação
sigma <- 1
gama <- 1

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
jogadores[,`Perna boa`:=ifelse(`Perna boa`=="Dir.",1,0)*10]
jogadores[,`Perna ruim`:=as.numeric(`Perna ruim`)*10]


jogadores <- gather(jogadores %>%
                      select(one_of(vars)),
                    key = variavel, value = valor,-time_match3,-id_jogador,-temporada)

jogadores <- jogadores %>%
  mutate(season=as.numeric(gsub("fifa(..).*","\\1",temporada,perl=T))+2000) 


## média por time
jogadores_time <- jogadores %>% 
  dplyr::group_by(season,time_match3,variavel) %>%
  dplyr::summarise(valor= mean(as.numeric(valor)))

saveRDS(jogadores_time,"data/static/dados_media_time.rds")






### Descritiva ----
times_analise <- c("Arsenal","Chelsea","Liverpool","Man Utd","Man City")
variaveis_analise <- unique(jogadores_time$variavel)


jogadores_time_analise <- jogadores_time %>%
  filter(variavel %in% variaveis_analise &
           time_match3 %in% times_analise)

i <- variaveis_analise[1]
library(stringi)
stri_trans_general(i, "Latin-ASCII")
for(i in variaveis_analise){
  var_name <- gsub(" ","_",tolower(stri_trans_general(i, "Latin-ASCII") ))
  nome_arquivo <- paste0("report/images/",var_name,"_descri.pdf")
  pdf(file = nome_arquivo)
  g <- ggplot(data=jogadores_time_analise %>% filter(variavel == i & season!=2017),
         aes(x=season, y=valor, group = time_match3 , 
             colour=time_match3)) +
    geom_line(size = 2 )+
    geom_point( size=2, shape=22, fill="white")  + #http://sape.inf.usi.ch/quick-reference/ggplot2/shape
    scale_colour_manual(name = "Team",
                        values=c("darkred", "blue","red","lightblue","gold")) + 
    scale_x_discrete(name ="Season", 
                     limits=c(2008:2016)) +
    scale_y_continuous(name = i) +
    scale_shape_discrete(name  ="Team")
  print(g)#https://stackoverflow.com/questions/21321975/loop-does-not-finish-saving-pdf
  dev.off()
}




## resultado jogos ----

fileinput <- "data/games"
arquivos <- list.files(path = fileinput,pattern = paste0('resultado_'),
                       full.names = T)

base <- data.table()
for(i in 1:length(arquivos)){
  base_temp <- readRDS(arquivos[i])
  base <- rbind(base,base_temp)
}

# 
# saas <- grep("2009",arquivos,value=T)
# table(gsub("data/games/S_resultado_.*_J(.*).rds","\\1",saas))


## pegando posição ----


fileinput <- "data/class"
arquivos <- list.files(path = fileinput,pattern = paste0('S'),
                       full.names = T)


base_class <- data.table()
for(i in 1:length(arquivos)){
  base_temp <- readRDS(arquivos[i])
  base_class <- rbind(base_class,base_temp)
}
rm(base_temp)

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
  mutate(score_casa = as.numeric(Casa),
         score_fora = as.numeric(Fora)) %>%
  select(season,id_jogo,score_casa,score_fora)

base2 <- spread(base %>% 
                  select(tipo,season,id_time,id_jogo,dia_rodada),tipo,id_time)

base_resultados <- left_join(base2,base1,
                   by = c("season","id_jogo"))
rm(base1,base2)

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
  select(season,id_jogo,Casa,Fora,score_casa,score_fora,pos)



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
                               select(season,Casa,Fora,score_casa,score_fora,pos,variavel,valor),key = variavel,value = valor)



## Modelo ----
saveRDS(base_result_jogadores_modelo,"data/result/base_modelo_dirichilet_score.rds")


base_result_jogadores_forecast <- base_result_jogadores %>%
  select(season,Casa,time_casa,variavel,valor_casa) %>%
  unique()

# ## Forecast ----
# saveRDS(base_result_jogadores_forecast,"data/result/base_modelo_bayes01-forecast.rds")


base_result_jogadores_modelo <- base_result_jogadores_modelo %>% 
  mutate(resultado = ifelse(score_casa>score_fora,"VH",
                            ifelse(score_casa==score_fora,"TIE","VV")))

aa_analise <- gather(base_result_jogadores_modelo,variable,valor,-season,-Casa,
                     -Fora,-score_casa,-score_fora,-resultado) 
variaveis_analise <- unique(aa_analise$variable)


base_select_variaveis <- aa_analise  %>% filter(season!=2017) %>%
  group_by(variable,resultado) %>% summarise(valor = mean(valor))

base_select_variaveis <- spread(base_select_variaveis,resultado,valor) %>%
  select(variable,VH,TIE,VV)



base_select_variaveis2 <- aa_analise  %>% filter(season!=2017) %>%
  group_by(variable,resultado,season) %>% summarise(valor = mean(valor)) %>%
  spread(resultado,valor) %>%
  group_by(variable) %>%
  summarise(NVHP = mean(VH>TIE & VH>VV ),
            NVVP = mean(VV<TIE & VV<VH),
            NTIEP = mean(VH>TIE & TIE>VV),
            NP    = mean(NVHP,NVVP,NTIEP),
            NVHN = mean(VH<TIE & VH<VV ),
            NVVN = mean(VV>TIE & VV>VH),
            NTIEN = mean(VH<TIE & TIE<VV),
            NN    = mean(NVHN,NVVN,NTIEN))

base_select_variaveis <- base_select_variaveis %>%
  left_join(base_select_variaveis2,by = "variable")


## selecionando variáveis com todos os resultados que fazem sentido
level_select <- 0.8
vars_selecionadas <- (base_select_variaveis %>%
                        filter(NP>=level_select | NN>=level_select))$variable


base_select_variaveis[,-(1:4)] <- base_select_variaveis[,-(1:4)]*100
base_select_variaveis$variable <- paste0(rep(c("\rowcolor{gray!30!}","\rowcolor{gray!10!}"),15)," ",
                                         base_select_variaveis$variable)
library(xtable)
print(xtable(base_select_variaveis,
             digits = c(0,0,3,3,3,1,1,1,1,1,1,1,1)),include.rownames = F)


var_name <- gsub(" ","",tolower(stri_trans_general(variaveis_analise, "Latin-ASCII") ))
var_name <- gsub("\\.","_",var_name )
nome_arquivo <- paste0(var_name,"_result.pdf")

# setdiff( c("Aceleração","Carrinho","Ch. de longe",
#                        "Cobr. falta","Contr. bola","Cruzamento",
#                        "Div. em pé","Dribles","Duração Do Contrato",
#                        "Finalização","Força chute","Idade",
#                        "Lançamento","Marcação","Passe curto",
#                        "Perna ruim","Pique","Posicion. GL",
#                        "Reação"),vars_selecionadas$variable)
# 
# setdiff(vars_selecionadas$variable,
#         c("Aceleração","Carrinho","Ch. de longe",
#           "Cobr. falta","Contr. bola","Cruzamento",
#           "Div. em pé","Dribles","Duração Do Contrato",
#           "Finalização","Força chute","Idade",
#           "Lançamento","Marcação","Passe curto",
#           "Perna ruim","Pique","Posicion. GL",
#           "Reação"))

vars_deletadas <- setdiff(variaveis_analise,vars_selecionadas)

for(i in variaveis_analise){
  var_name <- gsub(" ","",tolower(stri_trans_general(i, "Latin-ASCII") ))
  var_name <- gsub("\\.","_",var_name )
  nome_arquivo <- paste0("report/images/",var_name,"_result.pdf")
  pdf(file = nome_arquivo)
  g <-ggplot(data =aa_analise  %>% filter(variable == i & season!=2017),
             aes(x=as.factor(season), y=valor)) +
    # geom_boxplot(aes(fill=resultado,x=as.factor(season), y=valor)) +
    # scale_colour_manual(values = c("green", "blue","red")) + 
    stat_summary(fun.y=mean,  geom="line",
                 aes( group = resultado ,colour = resultado),size=2) + 
    scale_x_discrete(name ="Season") +
    scale_fill_manual(name = "Result",
                      values=c("green", "blue","red"))  +
    scale_colour_discrete("Result")  + 
    scale_y_continuous(name = i) 
  
  
  
  if(i %in% vars_deletadas){
    
    baselimite <- aa_analise  %>% filter(variable == i & season!=2017) %>%
      group_by(resultado,season) %>% summarise(valor = mean(valor))
    
    
    minv <- min(baselimite$valor)
    maxv <- max(baselimite$valor)
    
    # diagonal 1
    ablines <- data.frame(rbind(c(1,maxv),
                                c(9,minv)))
    names(ablines) <- c("ano","valor")
    coefs <- coef(lm(valor ~ ano, data = ablines))
    
    g <- g + geom_abline(intercept = coefs[1], slope = coefs[2],size = 5,alpha = .25)
    
    # diagonal 2
    ablines <- data.frame(rbind(c(1,minv),
                                c(9,maxv)))
    names(ablines) <- c("ano","valor")
    coefs <- coef(lm(valor ~ ano, data = ablines))
    g <- g + geom_abline(intercept = coefs[1], slope = coefs[2],size = 5,alpha = .25)
    
  }
  print(g)#https://stackoverflow.com/questions/21321975/loop-does-not-finish-saving-pdf
  dev.off()
}


## Com transformação media ----

base_select_variaveis <- aa_analise  %>% filter(season!=2017) %>%
  group_by(variable,resultado) %>%
  summarise(valor  = mean(valor)) %>%
  mutate(valor = gama*exp(-sigma*(valor^2))) 


base_select_variaveis <- spread(base_select_variaveis,resultado,valor) %>%
  select(variable,VH,TIE,VV)

base_select_variaveis2 <- aa_analise  %>% filter(season!=2017) %>%
  group_by(variable,resultado,season)%>%  
  summarise(valor = mean(valor))  %>% 
  mutate(valor =gama*exp(-sigma*(valor^2))) %>%
  spread(resultado,valor) %>%
  group_by(variable) %>%
  summarise(MNP = mean(2*TIE - VH-VV)/2,
            NP = mean(VH<TIE & TIE>VV),
            NN = mean(VH>TIE | TIE<VV))

base_select_variaveis <- base_select_variaveis %>%
  left_join(base_select_variaveis2,by = "variable")



level_select <- .5
vars_selecionadas_trans <- (base_select_variaveis %>%
                              filter(MNP>=level_select ))$variable;vars_selecionadas_trans


base_select_variaveis[,-(1:5)] <- base_select_variaveis[,-(1:5)]*100
library(xtable)
base_select_variaveis$variable <- paste0(rep(c("\rowcolor{gray!30!}","\rowcolor{gray!10!}"),15)," ",
                                         base_select_variaveis$variable)

print(xtable(base_select_variaveis,
             digits = c(0,0,3,3,3,3,1,1)),include.rownames = F)



vars_deletadas_trans <- setdiff(variaveis_analise,vars_selecionadas_trans)

## transformação 
i <- variaveis_analise[1]
for(i in variaveis_analise){
  var_name <- gsub(" ","",tolower(stri_trans_general(i, "Latin-ASCII") ))
  var_name <- gsub("\\.","_",var_name )
  nome_arquivo <- paste0("report/images/",var_name,"_result_trans_media.pdf")
  pdf(file = nome_arquivo)
  g <-ggplot(data =aa_analise  %>% filter(variable == i & season!=2017),
             aes(x=as.factor(season), y=valor)) +
    # geom_boxplot(aes(fill=resultado,x=as.factor(season), y=valor)) +
    # scale_colour_manual(values = c("green", "blue","red")) + 
    stat_summary(fun.y=function(x){gama*exp(-sigma*(mean(x)^2))},  geom="line",
                 aes( group = resultado ,colour = resultado),size=2) + 
    scale_x_discrete(name ="Season") +
    scale_fill_manual(name = "Result",
                      values=c("green", "blue","red"))  +
    scale_colour_discrete("Result")  + 
    scale_y_continuous(name = i) 
  
  if(i %in% vars_deletadas_trans){
    
    baselimite <- aa_analise  %>% filter(variable == i & season!=2017)%>%
      dplyr::group_by(resultado,as.factor(season)) %>% summarise(valor = mean(valor)) %>%
      dplyr::mutate(valor = gama*exp(-sigma*(valor^2)))
    
    minv <- min(baselimite$valor)
    maxv <- max(baselimite$valor)
    
    # diagonal 1
    ablines <- data.frame(rbind(c(1,maxv),
                                c(9,minv)))
    names(ablines) <- c("ano","valor")
    coefs <- coef(lm(valor ~ ano, data = ablines))
    
    g <- g + geom_abline(intercept = coefs[1], slope = coefs[2],size = 5,alpha = .25)
    
    # diagonal 2
    ablines <- data.frame(rbind(c(1,minv),
                                c(9,maxv)))
    names(ablines) <- c("ano","valor")
    coefs <- coef(lm(valor ~ ano, data = ablines))
    g <- g + geom_abline(intercept = coefs[1], slope = coefs[2],size = 5,alpha = .25)
    
  }
  print(g)#https://stackoverflow.com/questions/21321975/loop-does-not-finish-saving-pdf
  dev.off()
}


saveRDS(data.frame(vars_selecionadas = unique(vars_selecionadas)),"data/result/variaveis_modelo_select_dissimilaridade.rds")
saveRDS(data.frame(vars_selecionadas = unique(vars_selecionadas_trans)),"data/result/variaveis_modelo_select_similaridade.rds")


## Com transformação ----

sigma <- 10
gama <- 10
base_select_variaveis <- aa_analise  %>% filter(season!=2017) %>%
  group_by(variable,resultado) %>% mutate(valor = gama*exp(-sigma*(valor^2)))%>%
summarise(valor  = mean(valor)) 

       
base_select_variaveis <- spread(base_select_variaveis,resultado,valor) %>%
  select(variable,VH,TIE,VV)

base_select_variaveis2 <- aa_analise  %>% filter(season!=2017) %>%
  group_by(variable,resultado,season)%>%  mutate(valor =gama*exp(-sigma*(valor^2))) %>%
  summarise(valor = mean(valor))  %>% 
  spread(resultado,valor) %>%
  group_by(variable) %>%
  summarise(MNP = mean(2*TIE - VH-VV)/2,
            NP = mean(VH<TIE & TIE>VV),
            NN = mean(VH>TIE | TIE<VV))

base_select_variaveis <- base_select_variaveis %>%
  left_join(base_select_variaveis2,by = "variable")



level_select <- .15
vars_selecionadas_trans <- (base_select_variaveis %>%
                        filter(MNP>=level_select ))$variable;vars_selecionadas_trans


base_select_variaveis[,-(1:5)] <- base_select_variaveis[,-(1:5)]*100
library(xtable)
base_select_variaveis$variable <- paste0(rep(c("\rowcolor{gray!30!}","\rowcolor{gray!10!}"),15)," ",
                                         base_select_variaveis$variable)

print(xtable(base_select_variaveis,
             digits = c(0,0,3,3,3,3,1,1)),include.rownames = F)



vars_deletadas_trans <- setdiff(variaveis_analise,vars_selecionadas_trans)
## transformação 
i <- variaveis_analise[1]
for(i in variaveis_analise){
  var_name <- gsub(" ","",tolower(stri_trans_general(i, "Latin-ASCII") ))
  var_name <- gsub("\\.","_",var_name )
  nome_arquivo <- paste0("report/images/",var_name,"_result_trans.pdf")
  pdf(file = nome_arquivo)
    g <-ggplot(data =aa_analise  %>% filter(variable == i & season!=2017),
               aes(x=as.factor(season), y=gama*exp(-sigma*(valor^2)))) +
      # geom_boxplot(aes(fill=resultado,x=as.factor(season), y=valor)) +
      # scale_colour_manual(values = c("green", "blue","red")) + 
      stat_summary(fun.y=mean,  geom="line",
                   aes( group = resultado ,colour = resultado),size=2) + 
      scale_x_discrete(name ="Season") +
      scale_fill_manual(name = "Result",
                        values=c("green", "blue","red"))  +
      scale_colour_discrete("Result")  + 
      scale_y_continuous(name = i) 
    
    if(i %in% vars_deletadas_trans){
      
      baselimite <- aa_analise  %>% filter(variable == i & season!=2017)%>%
        dplyr::group_by(resultado,as.factor(season)) %>%
        dplyr::mutate(valor = gama*exp(-sigma*(valor^2))) %>% 
     summarise(valor = mean(valor))
        

      minv <- min(baselimite$valor)
      maxv <- max(baselimite$valor)
      
      # diagonal 1
      ablines <- data.frame(rbind(c(1,maxv),
                                  c(9,minv)))
      names(ablines) <- c("ano","valor")
      coefs <- coef(lm(valor ~ ano, data = ablines))
      
      g <- g + geom_abline(intercept = coefs[1], slope = coefs[2],size = 5,alpha = .25)
      
      # diagonal 2
      ablines <- data.frame(rbind(c(1,minv),
                                  c(9,maxv)))
      names(ablines) <- c("ano","valor")
      coefs <- coef(lm(valor ~ ano, data = ablines))
      g <- g + geom_abline(intercept = coefs[1], slope = coefs[2],size = 5,alpha = .25)
      
    }
    print(g)#https://stackoverflow.com/questions/21321975/loop-does-not-finish-saving-pdf
    dev.off()
}

