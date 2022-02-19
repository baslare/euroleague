require(tidyverse)


source("EL_functions.R")



seasons_list <- readRDS("el_raw.rds")



parsed_season <- lapply(seasons_list[3:10], function(y) lapply(y, function(x){
  
  home <- x$header$CodeTeamA
  away <- x$header$CodeTeamB
  
  x[[paste(home)]] <- event_parser(x,home)
  x[[paste(away)]] <- event_parser(x,away)
  
  return(x)
  
}))



parsed_season <- lapply(parsed_season, function(y) lapply(y, function(x){
  
  home <- x$header$CodeTeamA
  away <- x$header$CodeTeamB
  
  x[[paste(home)]] <- x[[paste(home)]] %>% posFinder()
  x[[paste(away)]] <- x[[paste(away)]] %>% posFinder()
  
  return(x)
  
}))


parsed_season <- lapply(parsed_season, function(y) lapply(y, function(x){
  
  home <- x$header$CodeTeamA
  away <- x$header$CodeTeamB
  
  x[[paste(home)]]$isHome <- x[[paste(home)]]$CODETEAM %>% str_trim() == home
  x[[paste(away)]]$isHome <- x[[paste(away)]]$CODETEAM %>% str_trim() == home
  
  return(x)
  
}))

st_list <- list()

invisible(Map(function(y,z){
  
  season <- z
  teams_list <- list()
  
  lapply(y, function(x){
    
    home <- x$header$CodeTeamA
    away <- x$header$CodeTeamB
    
    home_add <- list(list(team=x[[paste(home)]],opp=x[[paste(away)]]))
    away_add <- list(list(team=x[[paste(away)]],opp=x[[paste(home)]]))
    
    
    
    names(home_add) <- away
    names(away_add) <- home
    
    teams_list[[paste(home)]] <<- append(teams_list[[paste(home)]],home_add)
    teams_list[[paste(away)]] <<- append(teams_list[[paste(away)]],away_add)
    
    
    
  })
  
  st_list[[paste(z)]] <<- teams_list
  
  
  
},y=parsed_season,z=names(parsed_season)))



players_db <- lapply(parsed_season, function(x) lapply(x, function(y){
  
  return(rbind(y$home_players,y$away_players))
  
}))


players_db <- lapply(players_db, bind_rows)
players_db <- lapply(players_db, function(x) x %>% distinct(ac,.keep_all = T) %>% select(ac,na,c))

  
players_hash <- lapply(players_db, function(x){
  
  
  pl_hash <- as.list(x$na)
  names(pl_hash) <- x$ac
  
  return(pl_hash)
})


saveRDS(st_list,"st_list.rds")
saveRDS(players_hash,"players_hash.rds")
saveRDS(players_db,"players_db.rds")














