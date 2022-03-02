require(tidyverse)

tList <- readRDS("st_list.rds")
allFives <- readRDS("allLineups.rds")
teamsDF_as <- readRDS("teamsDF_as.rds")


nList <- lapply(tList, function(x) lapply(x, function(y) data.frame(length(y))))
nList <- lapply(nList, function(x) x %>% bind_rows(.id = "team"))
nList <- nList %>% bind_rows(.id = "season")
colnames(nList)[3] <- "games_played"





allTeams <- allFives %>% group_by(team,season) %>% summarise_if(.predicate = is.numeric,.funs = sum)
allTeams <- allTeams %>% select(-games_played)
allTeams$posPerMin <- allTeams$possession/(allTeams$duration/60)
allTeams$ppp <- (allTeams$madeFT + 2*allTeams$made2p + 3*allTeams$made3p)/allTeams$possession

allTeams <- left_join(allTeams,nList,by=c("team","season"))
allTeams <- left_join(allTeams,teamsDF_as,by=c("team","season"))

allTeams <- allTeams %>% mutate(minute = duration/60,
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




saveRDS(allTeams,"allTeams.rds")


to_display <- c("team","season","games_played","ppp","pppOp","pace","tsRatio","ratio2P","ratio3P","a2p_ratio","a3p_ratio","FTratio"              
                ,"assist","turnover","as_to","made2p","attempted2p","made3p","attempted3p","madeFT","attemptedFT",  
                "defReb","defRebRatio","offReb","offRebRatio","totReb","foulDrawn","foul","block","rejected","steal",    
                "oFoul","oFoulOp","assistOp","turnoverOp","made3pOp","attempted3pOp","made2pOp","attempted2pOp","madeFTOp",     
                "attemptedFTOp", "defRebOp","offRebOp","totRebOp","stealOp","possession","possessionOp")

allTeams <- allTeams %>% select(to_display)
