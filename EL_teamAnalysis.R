require(tidyverse)


allFives <- readRDS("allLineups.rds")
teamsDF_as <- readRDS("teamsDF_as.rds")



allTeams <- allFives %>% group_by(team,season) %>% summarise_if(.predicate = sapply(allFives,is.numeric)[1:(dim(allFives)[2]-2)],.funs = sum)
allTeams$posPerMin <- allTeams$possession/(allTeams$duration/60)
allTeams$PPP <- (allTeams$madeFT + 2*allTeams$made2p + 3*allTeams$made3p)/allTeams$possession

allTeams <- left_join(allTeams,teamsDF_as,by=c("team","season"))