require(tidyverse)

tList <- readRDS("processed_data.RDS")


isN <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) sapply(games, function(cols) sapply(cols, is.numeric)))))
aggTeams <- mapply(function(m,n) mapply(function(a,b) mapply(function(p,r) list(mapply(function(x,y) x %>% group_by(five_id) %>% summarise_if(.predicate = is.numeric,.funs = sum) , x=p,y=r)),p=a,r=b),a=m,b=n),m=tList,n=isN,SIMPLIFY = F)

aggTeams <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(game) lapply(game, function(x) x %>% group_by(five_id) %>% summarise_if(is.numeric,sum)))))
aggTeams[["2018"]] <- aggTeams[["2018"]][-c((length(aggTeams[["2018"]])-3):(length(aggTeams[["2018"]])))]
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) lapply(team, function(game) game[[1]])))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% bind_rows))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% mutate(possessionOp= team$attempted2pOp + team$attempted3pOp + team$posCheckOp + team$turnoverOp - team$offRebOp)))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% mutate(possession= team$attempted2p + team$attempted3p + team$posCheck + team$turnover - team$offReb)))
aggTeams <- lapply(aggTeams, function(season) lapply(season, function(team) team %>% mutate(posPerMin= team$possession/(team$duration/60))))



allFives <- lapply(aggTeams, function(x) lapply(x, function(y)  y %>% group_by(five_id) %>% summarise_if(.predicate =  sapply(y,is.numeric)[-1] ,.funs=sum)))
team_codes <- lapply(aggTeams, names)
allFives <- Map(function(x,y) Map(function(a,b) a %>% mutate(team=b), a=x,b=y),x=allFives,y=team_codes)
season_codes <- names(allFives)
allFives <- Map(function(x,y) Map(function(a,b) a %>% mutate(season=b), a=x,b=y),x=allFives,y=season_codes)



allFives <- lapply(allFives, bind_rows)
allFives <- allFives %>% bind_rows()
allFives <- allFives %>% select(-c(MINUTE,POINTS_A,POINTS_B,quarter,timeNum))
allFives <- allFives %>% mutate(posPerMin=possession/(duration/60))


saveRDS(allFives,"allLineups.rds")




