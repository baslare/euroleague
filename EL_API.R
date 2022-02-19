require(jsonlite)
require(tidyverse)

setwd("C:/Users/Efe/Desktop/Projeler/euroleague/")


season <- 2020
all_seasons <- list()

while(season <= 2021){
  game <- 1
  cons <- 0
  
  season_list <- list()
  
  while(cons <= 50){
    
    header_url<- paste0("https://live.euroleague.net/api/Header?gamecode=",game,"&seasoncode=E",season)
    
    
    
    header_json <- try(fromJSON(header_url))
    
    
    
    
    
    if(!is.list(header_json)){
      cons <- cons + 1
    }else{
      
      home_team <- header_json$CodeTeamA %>% str_squish()
      away_team <- header_json$CodeTeamB %>% str_squish()
      
      
      points_url <- paste0("https://live.euroleague.net/api/Points?gamecode=",game,"&seasoncode=E",season,"&disp=")
      home_players_url <- paste0("https://live.euroleague.net/api/Players?gamecode=",game,"&seasoncode=E",season,"&disp=&equipo=",home_team,"&temp=E",season)
      away_players_url <- paste0("https://live.euroleague.net/api/Players?gamecode=",game,"&seasoncode=E",season,"&disp=&equipo=",away_team,"&temp=E",season)
      playbyplay_url <- paste0("https://live.euroleague.net/api/PlayByPlay?gamecode=",game,"&seasoncode=E",season,"&disp=")
      
      game_list <- list()
      game_list[["header"]] <- header_json
      game_list[["points"]] <- try(fromJSON(points_url))
      game_list[["home_players"]] <- try(fromJSON(home_players_url))
      game_list[["away_players"]] <- try(fromJSON(away_players_url))
      game_list[["playbyplay"]] <- try(fromJSON(playbyplay_url))
      
      season_list <- append(season_list,list(game_list))
      
    }
    
    
    message(paste(season,game,"cleared"))
    game <- game + 1
  }
  
  all_seasons[[paste(season)]] <- season_list
  season <- season + 1
  saveRDS(all_seasons,"all_seasons.RDS")
  
}
