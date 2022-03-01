###all teams###

require(tidyverse)
require(ggraph)
require(igraph)


st_list <- readRDS("st_list.RDS")


allTeams <- lapply(st_list, function(w) lapply(w, function(x) lapply(x, function(y) y[[1]])))
allTeams <- lapply(allTeams, function(w) lapply(w, function(x) lapply(x, as.data.frame)))
allTeams <- lapply(allTeams, function(w) lapply(w, function(x) lapply(x, bind_rows)))
allTeams <- lapply(allTeams, function(w) lapply(w, bind_rows))
allTeams <- lapply(allTeams, function(w) lapply(w, function(x) x %>% filter(str_detect(PLAYTYPE,pattern = "2FGM|AS|3FGM|FTM"))))



assist_list <- lapply(allTeams, function(w) lapply(w, function(x) x %>% mutate(check = FALSE)))
assist_list <- lapply(assist_list, function(w) lapply(w, function(x) x %>% mutate(from = "")))
assist_list <- lapply(assist_list, function(w) lapply(w, function(x){
  hld <- c(x$PLAYTYPE[-1],"")
  x %>% mutate(check = hld %>% str_detect("AS") )
} ))

assist_list <- lapply(assist_list, function(w) lapply(w, function(x){
  hld <- c(x$PLAYER_ID[-1],"")
  x %>% mutate(from = ifelse(check,hld,""))
} ))

codes <- lapply(assist_list, function(w) names(w))

assist_list <-  lapply(assist_list, function(w) lapply(w, function(x)x %>% rename(team = CODETEAM)))
assist_list <- Map(function(w,h) Map(function(x,y) x %>% mutate(season = y), x=w, y=h),w=assist_list,h=names(assist_list))
assist_list <- lapply(assist_list, function(w) lapply(w, function(x) x %>% mutate(type = PLAYTYPE)))
assist_list <- lapply(assist_list, function(w) lapply(w, function(x) x %>% mutate(count = 1)))

assist <- lapply(assist_list, function(w) lapply(w, function(x) x[x$check,]))
assist <- lapply(assist, function(w) lapply(w, function(x) x %>% mutate(merge = paste(from,">",PLAYER_ID))))
assist <- lapply(assist, function(w) lapply(w, function(x) x %>% count(from,PLAYTYPE,team,season,merge,PLAYER_ID,sort = T)))


####Merge Chars



##### MERGE #####
###the most frequent connections

connections <- lapply(assist, bind_rows)
spli <- lapply(connections, function(w) as.character(w$merge) %>% str_split(" > "))
connections <- mapply(function(x,y) x %>% mutate(from=sapply(y, function(a) a[1])),x=connections,y=spli,SIMPLIFY = F)
connections <- mapply(function(x,y) x %>% mutate(to=sapply(y, function(a) a[2])),x=connections,y=spli,SIMPLIFY = F)

#connections <- lapply(connections, function(x) x %>% rename(to=toComma))

#### assisted 2p count
assisted2p <- lapply(assist_list, function(w) lapply(w, function(x) x[x$check & (x$type == "2FGM"),]))
assisted2pteam <- lapply(assisted2p, function(w) lapply(w, function(x) x %>% group_by(team) %>% summarise(count = sum(count))))
assisted2p <- lapply(assisted2p, function(w) lapply(w, function(x) x %>% group_by(PLAYER_ID,team) %>% summarise(count = sum(count))))
a2p <- lapply(assisted2p, bind_rows)
a2pteam <- lapply(assisted2pteam, bind_rows)


## unassisted 2p count
unas2p <- lapply(assist_list, function(w) lapply(w, function(x) x[!x$check & (x$type == "2FGM"),]))
unas2pteam <- lapply(unas2p, function(w) lapply(w, function(x) x %>% group_by(team) %>% summarise(count = sum(count))))
unas2p <- lapply(unas2p, function(w) lapply(w, function(x) x %>% group_by(PLAYER_ID,team) %>% summarise(count = sum(count))))
u2p <- lapply(unas2p, bind_rows)
u2pteam <- lapply(unas2pteam, bind_rows)

## ratios for assisted - unassisted 2p
ratios <- mapply(function(x,y) full_join(x,y,by=c("PLAYER_ID","team")),x=a2p,y=u2p,SIMPLIFY = F)
ratios <-  lapply(ratios, function(y) y <- lapply(y, function(x) replace_na(x,0)))
ratios <- lapply(ratios, function(x) setNames(x,c("PLAYER_ID","team","assisted2P","unassisted2P")))
ratios <- lapply(ratios, function(x) data.frame(x))
ratios <- lapply(ratios, function(x) x %>% mutate(a2p_ratio = assisted2P/(assisted2P+unassisted2P)))
ratios <- mapply(function(x,y) x %>% mutate(season=y), x=ratios,y=names(assist_list),SIMPLIFY = F)

team2p <- mapply(function(x,y) left_join(x,y,by="team"), x=a2pteam,y=u2pteam, SIMPLIFY = F)
team2p <- lapply(team2p, function(x) setNames(x,c("team","assisted2P","unassisted2P")))
team2p <- lapply(team2p, function(x) data.frame(x))
team2p <- lapply(team2p, function(x) x %>% mutate(a2p_ratio = assisted2P/(assisted2P+unassisted2P)))
team2p <- mapply(function(x,y) x %>% mutate(season=y), x=team2p,y=names(assist_list),SIMPLIFY = F)


#### assisted 3p count
assisted3p <- lapply(assist_list, function(w) lapply(w, function(x) x[x$check & (x$type == "3FGM"),]))
assisted3pteam <- lapply(assisted3p, function(w) lapply(w, function(x) x %>% group_by(team) %>% summarise(count = sum(count))))
assisted3p <- lapply(assisted3p, function(w) lapply(w, function(x) x %>% group_by(PLAYER_ID,team) %>% summarise(count = sum(count))))
a3p <- lapply(assisted3p, bind_rows)
a3pteam <- lapply(assisted3pteam, bind_rows)

## unassisted 2p count
unas3p <- lapply(assist_list, function(w) lapply(w, function(x) x[!x$check & (x$type == "3FGM"),]))
unas3pteam <- lapply(unas3p, function(w) lapply(w, function(x) x %>% group_by(team) %>% summarise(count = sum(count))))
unas3p <- lapply(unas3p, function(w) lapply(w, function(x) x %>% group_by(PLAYER_ID,team) %>% summarise(count = sum(count))))
u3p <- lapply(unas3p, bind_rows)
u3pteam <- lapply(unas3pteam, bind_rows)


## ratios for assisted - unassisted 3p

ratios3 <- mapply(function(x,y) full_join(x,y,by=c("PLAYER_ID","team")),x=a3p,y=u3p,SIMPLIFY = F)
ratios3 <-  lapply(ratios3, function(y) y <- lapply(y, function(x) replace_na(x,0)))
ratios3 <- lapply(ratios3, function(x) setNames(x,c("PLAYER_ID","team","assisted3P","unassisted3P")))
ratios3 <- lapply(ratios3, function(x) data.frame(x))
ratios3 <- lapply(ratios3, function(x) x %>% mutate(a3p_ratio = assisted3P/(assisted3P+unassisted3P)))
ratios3 <- mapply(function(x,y) x %>% mutate(season=y), x=ratios3,y=names(assist_list),SIMPLIFY = F)

team3p <- mapply(function(x,y) left_join(x,y,by="team"), x=a3pteam,y=u3pteam, SIMPLIFY = F)
team3p <- lapply(team3p, function(x) setNames(x,c("team","assisted3P","unassisted3P","a3p_ratio","season")))
team3p <- lapply(team3p, function(x) data.frame(x))
team3p <- lapply(team3p, function(x) x %>% mutate(a3p_ratio = assisted3P/(assisted3P+unassisted3P)))
team3p <- mapply(function(x,y) x %>% mutate(season=y), x=team3p,y=names(assist_list),SIMPLIFY = F)

players <- mapply(function(x,y) full_join(x,y,by=(c("PLAYER_ID","team","season"))),x=ratios,y=ratios3,SIMPLIFY = F)
players <- lapply(players, function(x) lapply(x, function(y) y %>% replace_na(0)))
teams <- mapply(function(x,y) full_join(x,y,by=c("team","season")),x=team2p,y=team3p,SIMPLIFY = F)
teams <- lapply(teams, function(x) lapply(x, function(y) y %>% replace_na(0)))


playersDF <- players %>% bind_rows()
teamsDF_as <- teams %>% bind_rows()

playersDF_as <- playersDF %>% relocate(season=`season`,.after=team)

playersDF_as$season <- as.character(playersDF$season)

teamsDF_as$team <- teamsDF_as$team %>% str_trim()
playersDF_as$team <- playersDF_as$team %>% str_trim()

saveRDS(teamsDF_as,"teamsDF_as.rds")
saveRDS(playersDF_as,"playersDF_as.rds")
