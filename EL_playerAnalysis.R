require(tidyverse)
require(parallel)

tList <- readRDS("processed_data.RDS")
players_db <- readRDS("players_db.rds")
allFives <- readRDS("allLineups.rds")
players_hash <- readRDS("players_hash.rds")
playersDF_as <- readRDS("playersDF_as.rds")

plnames <- unique(dnm$PLAYER_ID)
plnames <- plnames[-which(plnames=="")]



cl <- makeCluster(detectCores())
clusterEvalQ(cl, require(tidyverse))

tList[["2018"]] <- tList[["2018"]][-c((length(tList[["2018"]])-3):(length(tList[["2018"]])))]

aggPlayers <- parLapply(cl, tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(x)
  x %>% group_by(PLAYER_ID,CODETEAM) %>% summarise_if(.predicate = is.numeric,.funs = sum)))))



aggPlayers <- lapply(aggPlayers, function(season) lapply(season, function(team) lapply(team, function(games) 
  games[[1]])))

aggPlayers <- lapply(aggPlayers, function(season) lapply(season, function(team) 
  team %>% bind_rows))

aggPlayers <- lapply(aggPlayers, function(season) lapply(season, function(team){
  isn_check <- sapply(team,is.numeric)
  isn_check <- names(isn_check)[which(isn_check)]
  team %>% group_by(PLAYER_ID) %>% summarise_at(.vars = isn_check,.funs = sum)
  
} ))


aggPlayers <- lapply(aggPlayers, function(season) lapply(season, function(team){
  team %>% mutate(pts=madeFT+2*made2p+3*made3p,
                  pos=attempted2p + attempted3p + posCheck + turnover)
  
  
} ))

aggPlayers <- lapply(aggPlayers, function(season) lapply(season, function(team){
  team %>% filter(str_detect(PLAYER_ID,"P(.)+"))
  
  
} ))

aggPlayers <- lapply(aggPlayers, function(x) x %>% bind_rows(.id = "team"))

aggPlayers <- aggPlayers %>% bind_rows(.id="season")

aggPlayers <- aggPlayers %>% select(-c(MINUTE,POINTS_A,POINTS_B,quarter,timeNum,duration,cumSec,posCheckOp,assistOp:stealOp))
aggPlayers$ppp <- aggPlayers$pts/aggPlayers$pos


players_db <- players_db %>% bind_rows()
players_db <- players_db %>% distinct(ac,.keep_all = T)
players_hash <- players_db$na
names(players_hash) <- players_db$ac

aggPlayers$player_name <- players_hash[aggPlayers$PLAYER_ID]


fives_seasons <- lapply(unique(allFives$season), function(x) allFives %>% filter(season==x))

isN_aF <- sapply(allFives,is.numeric)
isN_aF <- names(isN_aF)[which(isN_aF)]



aggOther <- lapply(players_db$ac, function(x) allFives %>%
                     filter(str_detect(five_id,x)) %>% 
                     group_by(team,season) %>% 
                     summarise_if(.predicate = is.numeric,.funs = sum ))

names(aggOther) <- players_db$ac
aggOther <- aggOther %>% bind_rows(.id = "PLAYER_ID")
aggOther <- aggOther %>% select(-c(assistOp:attemptedFTOp,stealOp,oFoulOp,posCheckOp,posPerMin,cumSec))
colnames_saved <- colnames(aggOther)
colnames(aggOther)[-c(1:4)] <- str_c("team_",colnames(aggOther)[-c(1:4)])

aP_backup <- aggPlayers
aggPlayers <- left_join(aggPlayers,aggOther,by=c("PLAYER_ID","team","season"))
aggPlayers <- aggPlayers %>% relocate(player_name,.before = assist)
aggPlayers$defReb_pct <- aggPlayers$defReb/(aggPlayers$team_defReb+aggPlayers$team_offRebOp)
aggPlayers$offReb_pct <- aggPlayers$offReb/(aggPlayers$team_offReb+aggPlayers$team_defRebOp)
aggPlayers$usage <- aggPlayers$pos/(aggPlayers$team_possession +aggPlayers$team_offReb)
aggPlayers$bl_pct <- aggPlayers$block/aggPlayers$team_possessionOp
aggPlayers$ts_pct <- aggPlayers$pts/(2*(aggPlayers$pos - aggPlayers$turnover))


for(x in colnames_saved[5:22]){
  
  varName <- paste0(x,"_s")
  var1 <- x
  var2 <- paste0("team_",x)
  
  aggPlayers <- aggPlayers %>% mutate(!!varName := aggPlayers[[var1]]/aggPlayers[[var2]])
  aggPlayers <- aggPlayers %>% relocate(!!varName, .after=var1)
  
}
  
aggPlayers <- aggPlayers %>% relocate(defReb_pct,.after=defReb)
aggPlayers <- aggPlayers %>% relocate(offReb_pct,.after=offReb)
aggPlayers <- aggPlayers %>% relocate(usage,.after=player_name)



playersDF_as$team <- playersDF_as$team %>% str_trim()

aggPlayers <- left_join(aggPlayers,playersDF_as,by=c("PLAYER_ID","team","season"))
aggPlayers <- aggPlayers %>% filter(!(team %>% str_detect("T0")))
aggPlayers$player_name <- str_replace(aggPlayers$player_name,"BAYCAN OGULCAN","BAYCAN, OGULCAN")



spl_ <- aggPlayers$player_name%>% str_split("[,]")



aggPlayers$display <- sapply(spl_, function(x) str_to_title(paste(x[[2]],x[[1]])))

aggPlayers <- aggPlayers %>% mutate(tsRatio = pts/(2*(attempted2p + attempted3p + posCheck)),
                                    ratio2P = made2p/attempted2p,
                                    ratio3P = made3p/attempted3p,
                                    FTratio = madeFT/attemptedFT,
                                    as_to = assist/turnover)


minutes <- aggPlayers$duration/60
aggPlayers$minute <- minutes

aggPlayers <- aggPlayers %>% mutate_if(.predicate = is.numeric, ~round(.,digits=2))


to_display <- c("display","team","season",
                "minute","usage","ppp",
                "tsRatio","ratio2P","ratio3P",
                "FTratio" ,"a2p_ratio", "a3p_ratio",
                "assist","assist_s","turnover",
                "turnover_s","as_to", "made2p","attempted2p",
                "made2p_s","attempted2p_s","made3p",
                "attempted3p","made3p_s", "attempted3p_s",
                "madeFT","attemptedFT","madeFT_s",
                "attemptedFT_s","defReb","defReb_s",
                "offReb","offReb_s","foulDrawn",
                "foulDrawn_s","foul","foul_s",
                "block","block_s","steal",
                "steal_s", "oFoulOp",
                "oFoul","tFoul")

AP_to_csv <- aggPlayers %>% select(to_display)
AP_to_csv <- AP_to_csv %>% rename(PLAYERS=display)

saveRDS(AP_to_csv,"AP_to_sql.rds")
  
  
parallel::stopCluster(cl)
gc()




saveRDS(aggPlayers,"aggPlayers.rds")




