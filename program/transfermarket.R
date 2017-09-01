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

rm(list = ls())


pathout <- "data/games"
temporada <- 2004

for(temporada in 2005:2017){
  all_match <- data.table()
  link <- paste0("https://www.transfermarkt.com/premier-league/gesamtspielplan/wettbewerb/GB1?saison_id=",temporada,
                 "&spieltagVon=1&spieltagBis=38")
  
  
  
  
  html1 <- read_html(link, encoding = "UTF-8")
  
  tables <- html_table(html1, fill = TRUE)
  
  i <- 1
  for(i in 1:38){
    ### JOGO 1
    playoff <- data.table(tables[[i+3]][c(1,2,3,5,7)])
    playoff <- playoff[grepl("\\(",`Home team`),]
    vecs <- c("Date","Time")
    ## replicando datas e horários
    playoff <- playoff[,c(paste0(vecs,"_")) := lapply(.SD,function(x) cumsum(x!="")),.SDcols = vecs]
    for(j in vecs){
      stage <- unique(playoff[eval(parse(text=paste0(j,"!=''"))),
                               c(j,paste0(j,"_")),
                               with=FALSE])
      playoff[,(j):=NULL]
      playoff <- stage[playoff,on=(paste0(j,"_"))]
      playoff[,(paste0(j,"_")):=NULL]
      rm(stage)
    }
    
    
    setnames(playoff,c("Home","Result"),
             c("Result","Visiting team"))
    
    playoff[,pos_home:= gsub(".*\\((.*)\\..*","\\1",`Home team`)]
    playoff[,pos_visit:= gsub(".*\\((.*)\\..*","\\1",`Visiting team`)]
    playoff[,`Home team`:= gsub(".*\\(.*\\)\\s\\s(.*)","\\1",`Home team`, perl=T)]
    playoff[,`Visiting team`:= gsub("(.*)\\s\\s\\(.*","\\1",`Visiting team`, perl=T)]
    playoff[,score_home:= gsub("(.*)\\:.*","\\1",Result)]
    playoff[,score_visit:= gsub(".*\\:(.*)","\\1",Result)]
    playoff[,match:=i]
    playoff[,season:=temporada]
  
    all_match <- rbind(all_match,playoff)
  }
  name_file <- paste0("S",temporada,".rds")
  saveRDS(all_match,
          file.path(pathout,
                    name_file))
}
