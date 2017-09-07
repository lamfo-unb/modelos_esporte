rm(list = ls())
gc()

#install.packages("RCurl")
#install.packages("data.table")


##### Comentários ##########
#   
# raspagem0: importa os links de acesso de uma pagina específica do site da FIFA
# raspagem1: importa as versões do FIFA para um link de um jogador específico
# raspagem2: importa as atualizações de uma versão específica de um jogador específico
# raspagem3: importa as informações|características do jogador em uma atualização específica de uma versão FIFA específica.



# Carregando pacotes necessários
library(RCurl)
library(data.table)
library(gsubfn)
library(stringr)
library(purrr)
library(rvest) # the new package, version 0.3.0
library(RCurl)
library(XML)
library(dplyr)
rm(list = ls())


pathout <- "data/games"
temporada <- 2016

for(temporada in 2016:2016){
  all_match <- data.table()
  link <- paste0("https://www.transfermarkt.com/premier-league/gesamtspielplan/wettbewerb/GB1?saison_id=",temporada,
                 "&spieltagVon=1&spieltagBis=38")
  
  
  
  html_base_jogos <- "https://www.transfermarkt.com/"
  html1 <- read_html(link, encoding = "UTF-8")
  texto <- htmlParse(html1)
  texto.doc <- capture.output(texto)
  links_jogos <- grep(".*spielbericht.(.*).\".*",paste0(texto.doc),value = T,perl=T)
  links_jogos_direto <- gsub(".*id.*href../(.*)\".*","\\1",links_jogos)



  all_times <- data.table()
  all_jogadores <- data.table()
  all_gols <- data.table()
  all_cards <- data.table()
  all_subs <- data.table()
  
  # Micro info jogos ----
  
  for(i in 1:length(links_jogos_direto)){
    link_jogo <- paste0(html_base_jogos,links_jogos_direto[i])
    html2 <- read_html(link_jogo, encoding = "UTF-8")
    texto2 <- htmlParse(html2)
    texto.doc2 <- capture.output(texto2)

    #> times----
    times <- grep("</a></nobr></div>",texto.doc2,value = T,perl=T)
    id_times <- gsub(".*id..(.*)\".href.*","\\1",times)
    times <- gsub(".*\">(.*)</a>.*","\\1",times)
    
    ##>> esquema tático----
    esquema_tatico <- 
      grep("Starting line-up:",texto.doc2,value = T,perl=T)
    esquema_tatico <- gsub(".*Starting line-up..(.*)\t\t\t.*","\\1",esquema_tatico)
    
    ##>> técnicos
    info_tecnicos_nome <- 
      grep("profil/trainer.*",texto.doc2,value = T,perl=T)
    info_tecnicos_nome <- gsub(".*title..(.*)\".id.*","\\1",info_tecnicos_nome)
    
    
    
    base_jogo_temp_times <- data.table(tipo = c("Casa","Fora"),
                                      nome_time = times,
                                      id_time = id_times,
                                      esquema_tatico = esquema_tatico,
                                      tecnico = info_tecnicos_nome,
                                      link_jogo = link_jogo,
                                      id_jogo = i,
                                      season = temporada)

    
    ##> timeline ----
    inicio_time <- grep("Timeline",texto.doc2,value = F,perl=T)
    inicio_line <- grep("Line-Ups",texto.doc2,value = F,perl=T)
    
    txt_time <- texto.doc2[inicio_time:(inicio_line-1)]
    inicio_time_home <- grep("sb-leiste-heim",txt_time,value = F,perl=T)
    inicio_time_visit <- grep("sb-leiste-gast",txt_time,value = F,perl=T)
    txt_time_home <- txt_time[inicio_time_home:(inicio_time_visit-1)]
    txt_time_visit <- txt_time[-(1:(inicio_time_visit-1))]
    
    ##>> home ----
    info_time_home <- 
      grep("sb-leiste-ereignis",txt_time_home,value = T,perl=T)
    info_time_home <- gsub(".*style(.*)</div>.*","\\1",info_time_home)
    info_time_home_classe <- gsub(".*class.\"(.*)\".*","\\1",info_time_home)
    info_time_home_tempo  <- gsub(".*top..(.*)\\%.*","\\1",info_time_home)
    
    base_jogo_temp_home_times <- data.table(tipo="Casa",
                                           tipo_evento = info_time_home_classe,
                                           tempo_evento_perc = info_time_home_tempo,
                                           tempo_evento = as.numeric(info_time_home_tempo)*90/100)
    
    ##>> visit ----
    info_time_visit <- 
      grep("sb-leiste-ereignis",txt_time_visit,value = T,perl=T)
    info_time_visit <- gsub(".*style(.*)</div>.*","\\1",info_time_visit)
    info_time_visit_classe <- gsub(".*class.\"(.*)\".*","\\1",info_time_visit)
    info_time_visit_tempo  <- gsub(".*top..(.*)\\%.*","\\1",info_time_visit)
    
    base_jogo_temp_visit_times <- data.table(tipo="Fora",
                                           tipo_evento = info_time_visit_classe,
                                           tempo_evento_perc = info_time_visit_tempo,
                                           tempo_evento = as.numeric(info_time_visit_tempo)*90/100)
    
    base_jogo_temp_time <- rbind(base_jogo_temp_home_times,
                                 base_jogo_temp_visit_times)
    rm(base_jogo_temp_home_times,base_jogo_temp_visit_times)
    
    
    ##> relacionando com base de eventos (timeline) ----
    base_jogo_temp_time <- base_jogo_temp_time %>%
      group_by(tipo_evento,tipo) %>%
      mutate(id_evento = cumsum(!is.na(tempo_evento)))
    
    
    ##> jogadores ----
    ##>> id jogadores ----
    info_jogadores_id <- 
      grep(".*aufstellung-rueckennummer-name.*id.\"(.*)\".*",texto.doc2,value = T,perl=T)
    
    
    info_jogadores_id <- gsub(".*id.\"(.*)\".*","\\1",info_jogadores_id)
    
    ##>> nome jogadores ----
    info_jogadores_nome <- 
      grep(".*a.href.*profil.spieler.*span..",texto.doc2,value = T,perl=T)
    info_jogadores_link <- gsub(".*href.\".(.*)\".*","\\1",info_jogadores_nome)
    info_jogadores_nome <- gsub(".*spieler.*\".(.*)<.a.*","\\1",info_jogadores_nome)
    
    
    ##>> titulares ----
    base_jogo_temp_jogadores <- data.table(id = info_jogadores_id,
                                      nome = info_jogadores_nome)

    base_jogo_temp_jogadores[,tipo:=ifelse(.I<=nrow(base_jogo_temp_jogadores)/2,"Casa","Fora")]
    base_jogo_temp_jogadores[,classe:="Titular"]
    
    
    ##>> reservas ----

    
    info_reservas_numero_team <- 
      grep("Starting line-up:",texto.doc2,value = F,perl=T)
    ##>>> home ----
    texto.doc2_temp <- texto.doc2[info_reservas_numero_team[1]:(info_reservas_numero_team[2]-1)]
      info_reservas_numero <- 
        grep(".*ersatz-rn.*",texto.doc2_temp,value = T,perl=T)
      info_reservas_numero <- gsub(".*ersatz-rn\".(.*)..div.*","\\1",info_reservas_numero)
      
      info_reservas_nome <- 
        grep(".a.title.*spielprofil_tooltip.*",texto.doc2_temp,value = T,perl=T)
      info_reservas_nome <- gsub(".*title..(.*)\".class..spielprofil_tooltip.*","\\1",info_reservas_nome)
      
      info_reservas_id <- 
        grep(".a.title.*spielprofil_tooltip.*",texto.doc2_temp,value = T,perl=T)
      info_reservas_id <- gsub(".*id.\"(.*)..href.*","\\1",info_reservas_id)
      
      base_jogo_temp_jogadores_banco_home <- data.table(id = info_reservas_id,
                                             nome = info_reservas_nome)
      ##>>> visit ----
      texto.doc2_temp <- texto.doc2[info_reservas_numero_team[2]:length(texto.doc2)]
      info_reservas_numero <- 
        grep(".*ersatz-rn.*",texto.doc2_temp,value = T,perl=T)
      info_reservas_numero <- gsub(".*ersatz-rn\".(.*)..div.*","\\1",info_reservas_numero)
      
      info_reservas_nome <- 
        grep(".a.title.*spielprofil_tooltip.*",texto.doc2_temp,value = T,perl=T)
      info_reservas_nome <- gsub(".*title..(.*)\".class..spielprofil_tooltip.*","\\1",info_reservas_nome)
      
      info_reservas_id <- 
        grep(".a.title.*spielprofil_tooltip.*",texto.doc2_temp,value = T,perl=T)
      info_reservas_id <- gsub(".*id.\"(.*)..href.*","\\1",info_reservas_id)
      
      base_jogo_temp_jogadores_banco_visit <- data.table(id = info_reservas_id,
                                                        nome = info_reservas_nome)
      
      
      base_jogo_temp_jogadores_banco_home[,tipo:="Casa"]
      base_jogo_temp_jogadores_banco_home[,classe:="Reserva"]
      base_jogo_temp_jogadores_banco_visit[,tipo:="Casa"]
      base_jogo_temp_jogadores_banco_visit[,classe:="Reserva"]
      
      
      base_jogo_temp_jogadores <- rbind(base_jogo_temp_jogadores,
                                        base_jogo_temp_jogadores_banco_home,
                                        base_jogo_temp_jogadores_banco_visit)
      
      rm(base_jogo_temp_jogadores_banco_home,base_jogo_temp_jogadores_banco_visit)
    
      base_jogo_temp_jogadores <- base_jogo_temp_jogadores %>%
        left_join(base_jogo_temp_times %>% 
                    select(tipo,nome_time,link_jogo,id_jogo,season),
                  by = c("tipo"="tipo"))
      
      
    ##> eventos ----
    inicio_gols <- grep("Goals",texto.doc2,value = F,perl=T)
    inicio_subs <- grep("Substit",texto.doc2,value = F,perl=T)
    inicio_cards <- grep("Cards",texto.doc2,value = F,perl=T)
    fim_gols <- ifelse(length(inicio_subs)>0,inicio_subs-1,
                       ifelse(length(inicio_cards)>0,inicio_cards-1,length(texto.doc2)))
    fim_subs <- ifelse(length(inicio_cards)>0,inicio_cards-1,length(texto.doc2))
    fim_cards <- length(texto.doc2)
    
    
    ##>> gols e assistencias ----
    
    if(length(inicio_gols)>0){

      txt_gols <- texto.doc2[inicio_gols:fim_gols]
      
      ###>> total gols----
      info_tot_gols <- 
        grep("sb-aktion-aktion",txt_gols,value = F,perl=T)
      ngols <- length(info_tot_gols)
      info_tot_gols <- c(info_tot_gols,length(txt_gols)+1)
      
      base_jogo_temp_gols <- data.table()
      for(k in 1:ngols){
        txt_temp <- txt_gols[info_tot_gols[k]:(info_tot_gols[k+1]-1)]
        
        ###>> assistencias ----
        info_assist_n <- 
          grep("Assist*",txt_temp,value = F,perl=T)
        
        if(length(info_assist_n)>0){
          info_assist <- txt_temp[info_assist_n]
          info_assist_id <- gsub(".*id.\"(.*)..href.*","\\1",info_assist)
          info_assist <- gsub(".*\">(.)","\\1",info_assist)
          info_assist_nome <- gsub("(.)</a.*","\\1",info_assist)
          info_assist_modo <- gsub(".*</a>,.(.*),.*","\\1",info_assist)
          info_assist_modo <- gsub(".*</a>,.(.*):.*","\\1",info_assist_modo) ##TRATANDO EXCEÇÃO (Penalty: fouled player)
          info_gols <- txt_temp[-info_assist_n] 
        }else{
          info_assist_id = NA
          info_assist_nome = NA
          info_assist_modo = NA
          info_gols <- txt_temp
        }
        
        
        ###>> gols ----
        
        info_gols <-  grep("wichtig*",info_gols,value = T,perl=T)
        info_gols_id <- gsub(".*id.\"(.*)..href.*","\\1",info_gols)
        info_gols <- gsub(".*\">(.)","\\1",info_gols)
        info_gols_nome <- gsub("(.)</a.*","\\1",info_gols)
        info_gols_modo <- gsub(".*</a>,.(.*),.*","\\1",info_gols)
        
        
        base_jogo_temp_gols_temp <- data.table(marcador_id = info_gols_id,
                                               marcador = info_gols_nome,
                                               modo_marcador = info_gols_modo,
                                               assistente_id = info_assist_id,
                                               assistente = info_assist_nome,
                                               modo_assistente = info_assist_modo)
        base_jogo_temp_gols <- rbind(base_jogo_temp_gols,base_jogo_temp_gols_temp)
        rm(base_jogo_temp_gols_temp)
        
      }
      
      base_jogo_temp_gols <- base_jogo_temp_gols  %>%
        left_join(base_jogo_temp_jogadores %>% 
                    select(id,tipo,link_jogo,id_jogo,season),
                  by = c("marcador_id"="id"))   %>%
        group_by(tipo) %>%
        mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
        left_join(filter(base_jogo_temp_time ,tipo_evento == "sb-sprite sb-tor") %>%
                    select(tipo,tipo_evento,id_evento,tempo_evento),
                  by = c("tipo" = "tipo", "id_evento" = "id_evento"))
      
    }else{
      base_jogo_temp_gols_temp <- data.table()
    }
    

    ##>> substituicoes ----
    if(length(inicio_subs)>0){
      txt_subs <- texto.doc2[inicio_subs:fim_subs]
      info_subs <- 
        grep("sb-aus",txt_subs,value = T,perl=T)
      info_subs <- gsub(".*href(.)","\\1",info_subs)
      info_subs_out_id <- gsub(".*spieler/(.*)/saison.*","\\1",info_subs)
      info_subs_out <- gsub(".*spieler.*\">(.*)</a>.*","\\1",info_subs)
      info_subs_modo <- gsub(".*,.(.*)..</span><span.*","\\1",info_subs)
      info_subs_in <- 
        grep("sb-ein",txt_subs,value = T,perl=T)
      info_subs_in <- gsub(".*href(.)","\\1",info_subs_in)
      info_subs_in_id <- gsub(".*spieler/(.*)/saison.*","\\1",info_subs_in)
      info_subs_in <- gsub(".*spieler.*\">(.*)</a>.*","\\1",info_subs_in)
      
      
      
      base_jogo_temp_subs <- data.table(jogador_out_id = info_subs_out_id,
                                        jogador_out = info_subs_out,
                                        jogador_out_modo = info_subs_modo,
                                        jogador_in_id = info_subs_in_id,
                                        jogador_in = info_subs_in)
      
    
      
      base_jogo_temp_subs <- base_jogo_temp_subs %>%
        left_join(base_jogo_temp_jogadores %>% 
                    select(id,tipo,link_jogo,id_jogo,season),
                  by = c("jogador_out_id"="id"))%>%
        group_by(tipo) %>%
        mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
        left_join(filter(base_jogo_temp_time ,tipo_evento == "sb-sprite sb-wechsel") %>%
                    select(tipo,tipo_evento,id_evento,tempo_evento),
                  by = c("tipo" = "tipo", "id_evento" = "id_evento"))
      
      
    }else{
      base_jogo_temp_subs <- data.table()
    }
      
    
    ##>> cartoes ----
    if(length(inicio_cards)>0){
      txt_cards <- texto.doc2[inicio_cards:fim_cards]
      
      info_cards_n <- 
        grep("sb-aktion-aktion",txt_cards,value = F,perl=T)
      info_cards_id <-  gsub(".*id..(.*)..href.*","\\1",txt_cards[info_cards_n+1])
      info_cards_name <-  gsub(".*spieler.*\">(.*)</a>.*","\\1",txt_cards[info_cards_n+1])
      info_cards_type <-  gsub(".*\\..(.*).,.*","\\1",txt_cards[info_cards_n+2])
      info_cards_modo <-  gsub(".*\\..*,.(.*)<.*","\\1",txt_cards[info_cards_n+2])
      
      base_jogo_temp_cards <- data.table(jogador_card_id = info_cards_id,
                                         jogador_card = info_cards_name,
                                         card_tipo = info_cards_type,
                                         card_modo = info_cards_modo)
      
      
      base_jogo_temp_cards <- base_jogo_temp_cards %>%
        left_join(base_jogo_temp_jogadores %>% 
                    select(id,tipo,link_jogo,id_jogo,season),
                  by = c("jogador_card_id"="id")) %>%
        group_by(tipo) %>%
        mutate(id_evento = cumsum(!is.na(tipo)) ) %>%
        left_join(filter(base_jogo_temp_time ,tipo_evento == "sb-sprite sb-gelb") %>%
                    select(tipo,tipo_evento,id_evento,tempo_evento),
                  by = c("tipo" = "tipo", "id_evento" = "id_evento"))
      
    }else{
      base_jogo_temp_cards <- data.table()
    }


    
    name_file <- paste0("S_times_",temporada,"_J",i,".rds")
    saveRDS(all_times,
            file.path(pathout,
                      name_file))
    
    name_file <- paste0("S_jogadores_",temporada,"_J",i,".rds")
    saveRDS(all_jogadores,
            file.path(pathout,
                      name_file))
    
    name_file <- paste0("S_gols_",temporada,"_J",i,".rds")
    saveRDS(all_gols,
            file.path(pathout,
                      name_file))
    
    name_file <- paste0("S_cards_",temporada,"_J",i,".rds")
    saveRDS(all_cards,
            file.path(pathout,
                      name_file))
    
    name_file <- paste0("S_subs_",temporada,"_J",i,".rds")
    saveRDS(all_subs,
            file.path(pathout,
                      name_file))
    
    
    ##> Salvando bases ----
    all_times <- rbind(all_times,data.table(base_jogo_temp_time))
    all_jogadores <- rbind(all_jogadores,data.table(base_jogo_temp_jogadores))
    all_gols <- rbind(all_gols,data.table(base_jogo_temp_gols))
    all_cards <- rbind(all_cards,data.table(base_jogo_temp_cards))
    all_subs <- rbind(all_subs,data.table(base_jogo_temp_subs))

  }
  
  name_file <- paste0("S_times_",temporada,".rds")
  saveRDS(all_times,
          file.path(pathout,
                    name_file))
  
  name_file <- paste0("S_jogadores_",temporada,".rds")
  saveRDS(all_jogadores,
          file.path(pathout,
                    name_file))
  
  name_file <- paste0("S_gols_",temporada,".rds")
  saveRDS(all_gols,
          file.path(pathout,
                    name_file))
  
  name_file <- paste0("S_cards_",temporada,".rds")
  saveRDS(all_cards,
          file.path(pathout,
                    name_file))
  
  name_file <- paste0("S_subs_",temporada,".rds")
  saveRDS(all_subs,
          file.path(pathout,
                    name_file))
  
#Macro info jogos ----
  
  # tables <- html_table(html1, fill = TRUE)
  # 
  # i <- 1
  # for(i in 1:38){
  #   ### JOGO 1
  #   playoff <- data.table(tables[[i+3]][c(1,2,3,5,7)])
  #   playoff <- playoff[grepl("\\(",`Home team`),]
  #   vecs <- c("Date","Time")
  #   ## replicando datas e horários
  #   playoff <- playoff[,c(paste0(vecs,"_")) := lapply(.SD,function(x) cumsum(x!="")),.SDcols = vecs]
  #   for(j in vecs){
  #     stage <- unique(playoff[eval(parse(text=paste0(j,"!=''"))),
  #                              c(j,paste0(j,"_")),
  #                              with=FALSE])
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
  # name_file <- paste0("S",temporada,".rds")
  # saveRDS(all_match,
  #         file.path(pathout,
  #                   name_file))
}
