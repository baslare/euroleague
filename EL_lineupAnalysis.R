require(tidyverse)

tList <- readRDS("processed_data.RDS")
players_db <- readRDS("players_db.rds")


#### TODO: bring these to the data processing script

aggTeams <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(game) lapply(game, function(x) x %>% filter(!str_detect(five_id,"FALSE"))))))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) lapply(team, function(game) lapply(game, function(x) x %>% mutate(five_id = str_split(five_id,"; "))))))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) lapply(team, function(game) lapply(game, function(x)
  x %>% mutate(five_id = lapply(five_id, sort))))))

aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(five_id=sapply(five_id, function(x) str_c(x[1],x[2],x[3],x[4],x[5],sep="; ")))))))

#####




aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) lapply(team, function(game) lapply(game, function(x) x %>% group_by(five_id) %>% summarise_if(is.numeric,sum)))))
aggTeams[["2018"]] <- aggTeams[["2018"]][-c((length(aggTeams[["2018"]])-3):(length(aggTeams[["2018"]])))]

aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) lapply(team, function(game) game[[1]] %>% mutate(games_played=1))))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% bind_rows))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% mutate(possessionOp= team$attempted2pOp + team$attempted3pOp + team$posCheckOp + team$turnoverOp - team$offRebOp)))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% mutate(possession= team$attempted2p + team$attempted3p + team$posCheck + team$turnover - team$offReb)))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% mutate(posPerMin= team$possession/(team$duration/60))))



allFives <- lapply(aggTeams, function(x) lapply(x, function(y)  y %>% group_by(five_id) %>% summarise_if(.predicate =  is.numeric ,.funs=sum)))
team_codes <- lapply(aggTeams, names)
allFives <- Map(function(x,y) Map(function(a,b) a %>% mutate(team=b), a=x,b=y),x=allFives,y=team_codes)
season_codes <- names(allFives)
allFives <- Map(function(x,y) Map(function(a,b) a %>% mutate(season=b), a=x,b=y),x=allFives,y=season_codes)



allFives <- lapply(allFives, bind_rows)
allFives <- allFives %>% bind_rows()
allFives <- allFives %>% select(-c(MINUTE,POINTS_A,POINTS_B,quarter,timeNum))
allFives <- allFives %>% mutate(posPerMin=possession/(duration/60))
allFives <- allFives %>% filter(!str_detect(five_id,"FALSE"))

players_db<- players_db%>% bind_rows()
players_db <- players_db %>% distinct(ac,.keep_all = T)
rownames(players_db) <- players_db$ac
players_db <- players_db %>% select(na)

fives_key <- allFives$five_id %>% str_split("; ")
fives_key <- lapply(fives_key, function(x) players_db[x,"na"])

allFives <- allFives %>% mutate(P1 = sapply(fives_key, function(x) x[1]),
                             P2 = sapply(fives_key, function(x) x[2]),
                             P3 = sapply(fives_key, function(x) x[3]),
                             P4 = sapply(fives_key, function(x) x[4]),
                             P5 = sapply(fives_key, function(x) x[5]))
                                         

####calculating statistics



allFives <- allFives %>% mutate(minute = duration/60,
                                pts = madeFT + 2*made2p + 3*made3p,
                                pace = 40*(possession + possessionOp)/(2*minute),
                                mpg = minute/games_played,
                                ppg = pts/games_played,
                                ppp = pts/possession,
                                apg = assist/games_played,
                                rpg = (defReb + offReb)/games_played,
                                topg = turnover/games_played,
                                ptsOp = madeFTOp + 2*made2pOp + 3*made3pOp,
                                pppOp = ptsOp/possessionOp,
                                tsRatio = pts/(2*(attempted2p + attempted3p + posCheck)),
                                ratio2P = made2p/attempted2p,
                                ratio3P = made3p/attempted3p,
                                FTratio = madeFT/attemptedFT,
                                as_to = assist/turnover,
                                defRebRatio = defReb/(defReb+offRebOp),
                                offRebRatio = offReb/(defRebOp+offReb))




to_display <- c("team","season","P1","P2","P3","P4","P5","minute","mpg","ppp","pppOp","pace","tsRatio","games_played","ratio2P","ratio3P","FTratio"              
                ,"assist","turnover","as_to","made2p","attempted2p","made3p","attempted3p","madeFT","attemptedFT",  
                "defReb","defRebRatio","offReb","offRebRatio","totReb","foulDrawn","foul","block","rejected","steal",    
                "oFoul","oFoulOp","assistOp","turnoverOp","made3pOp","attempted3pOp","made2pOp","attempted2pOp","madeFTOp",     
                "attemptedFTOp", "defRebOp","offRebOp","totRebOp","stealOp","possession","possessionOp")



allFives <- allFives %>% select(to_display)

allFives <- allFives %>% mutate_if(.predicate = is.numeric, ~round(.,digits=2))



saveRDS(allFives,"allLineups.rds")

allFives <- allFives %>% filter(minute > 15) 
write.csv(allFives,"allLineups.csv",fileEncoding = "utf-8",row.names = FALSE)




