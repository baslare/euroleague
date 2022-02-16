


shotList <- list()

seasons <- 2013:2022


season <- "2021"

seasonList <- list()

gameCounter <- 1


while(TRUE){
  
  shotDF <- try(jsonlite::fromJSON(paste0("https://live.euroleague.net/api/Points?gamecode=",gameCounter,"&seasoncode=E",season,"&disp="))[[1]])
  if(length(shotDF > 1)){
    
    teams <- unique(shotDF$TEAM)
    seasonList[[teams[1]]] <- append(seasonList[[teams[1]]],list(shotDF %>% filter(TEAM %>% str_detect(teams[1]))))
    seasonList[[teams[2]]] <- append(seasonList[[teams[2]]],list(shotDF %>% filter(TEAM %>% str_detect(teams[2]))))
    
    
    
  }
  
  
  
  message(paste0("completed: ", gameCounter))
  gameCounter <- gameCounter + 1
}

saveRDS(seasonList,paste0("seasonList",season,".rds"))


saveRDS(charts,"charts.rds")





shotCoords <- lapply(assist_list, function(w) lapply(w, function(x) x%>% filter(!logchar %>% str_detect("Assist"))))
charts <- readRDS("charts.rds")
charts[[1]] <- NULL
names(charts) <- 2010:2019
charts <- lapply(charts, function(w) lapply(w, function(x) lapply(x, data.frame)))
charts <- lapply(charts, function(w) lapply(w, function(x) lapply(x, function(y) y %>% filter(!ACTION %>% str_detect("Missed")))))
charts <- lapply(charts, function(w) lapply(w, bind_rows))
charts <- lapply(charts, function(w) w %>% .[order(names(.))])

shotCoords <- lapply(shotCoords, function(w) w %>% .[order(names(.))])
masterList <- mapply(function(x,y) mapply(safely(function(a,b) data.frame(a,b)), a=x,b=y,SIMPLIFY = F), x=charts,y=shotCoords,SIMPLIFY = F )
combined <- lapply(masterList[["2019"]], function(x) x[[1]])
names(combined) <- names(combined) %>% str_trim()
ulk <- combined$ULK
