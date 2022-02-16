
require(tidyverse)
require(parallel)

source("EL_functions.R")


st_list <- readRDS("st_list.rds")
players_hash <- readRDS("players_hash.rds")



#### Tidy ####


tList <- st_list

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt) 
  alt %>% rename(event=PLAYTYPE)))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt) 
  alt %>% mutate(timeNum= unlist(sapply(sapply(alt$MARKERTIME,function(x) x %>% str_split(":")), function(y) as.numeric(y[[1]])*60 + as.numeric(y[[2]]))))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt) 
  alt %>% mutate(duration = mapply(function(x,y,z){
    if(z > 40){
      if(y-x < -100){
        300 + y-x 
      }else{
        y-x 
      }
    }else{
      if(y-x < -100){
        600 + y-x 
      }else{
        y-x
      } 
    }
  }  , x=alt$timeNum, y=c(0,alt$timeNum[-length(alt$timeNum)]),z=alt$MINUTE))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(five=mapply(function(x,y,z,u,w) c(x,y,z,u,w), x=alt$P1, y= alt$P2, z=alt$P3, u=alt$P4, w=alt$P5,SIMPLIFY = FALSE))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt) 
  alt %>% mutate(five_id = five)))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(five_id = lapply(five_id,sort))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(five_id=sapply(five, function(x) str_c(x[1],x[2],x[3],x[4],x[5],sep="; ")))))))

tList <- Map(function(season,season_hash) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt){
  alt %>% mutate(five=mapply(function(x,y,z,u,w) c(x,y,z,u,w), 
                             x=season_hash[alt$P1], 
                             y=season_hash[alt$P2], 
                             z=season_hash[alt$P3], 
                             u=season_hash[alt$P4], 
                             w=season_hash[alt$P5],
                             SIMPLIFY = FALSE))
}
  ))),
  season=tList, season_hash=players_hash)

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt) 
  alt %>% mutate(duration = ifelse(alt$duration <0,0,duration))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(five = lapply(alt$five,sort))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(fiveChar = sapply(five, function(x) str_c(x[1],x[2],x[3],x[4],x[5],sep="; ")))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(fiveCheck = mapply(function(x,y) x == y, x=alt$fiveChar, y=c(alt$fiveChar[-1],"")))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(assist= sapply(alt$event, function(x) ifelse(x=="AS",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(turnover= sapply(alt$event, function(x) ifelse(x=="TO",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(made3p= sapply(alt$event, function(x) ifelse(x=="3FGM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(attempted3p= sapply(alt$event, function(x) ifelse(x=="3FGA" | x=="3FGM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(made2p= sapply(alt$event, function(x) ifelse(x=="2FGM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(attempted2p= sapply(alt$event, function(x) ifelse(x=="2FGA" | x== "2FGM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(madeFT= sapply(alt$event, function(x) ifelse(x=="FTM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(attemptedFT= sapply(alt$event, function(x) ifelse(x=="FTA" | x=="FTM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(defReb= sapply(alt$event, function(x) ifelse(x=="D",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(offReb= sapply(alt$event, function(x) ifelse(x=="O",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(totReb= sapply(alt$event, function(x) ifelse(x=="O" | x== "D",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(foulDrawn= sapply(alt$event, function(x) ifelse(x=="RV",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(foul= sapply(alt$event, function(x) ifelse(x=="CM",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(block= sapply(alt$event, function(x) ifelse(x=="FV",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(rejected= sapply(alt$event, function(x) ifelse(x=="AG",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(steal= sapply(alt$event, function(x) ifelse(x=="ST",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(oFoul= sapply(alt$event, function(x) ifelse(x=="OF",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(tFoul= sapply(alt$event, function(x) ifelse(x=="CMT",1,0)))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(cumSec=cumsum(alt$duration))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% select(-c(TYPE,NUMBEROFPLAY,TEAM,DORSAL,PLAYINFO))))))

tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt){
  alt[dim(alt)[1]+1,] <- FALSE
  alt$cumSec[dim(alt)[1]] <- max(alt$MINUTE,na.rm = T)*60
  alt$duration[dim(alt)[1]] <- alt$cumSec[dim(alt)[1]] - alt$cumSec[dim(alt)[1]-1]
  alt$fiveCheck[dim(alt)[1]-1] <- TRUE
  alt$fiveCheck[dim(alt)[1]] <- FALSE
  alt$five[dim(alt)[1]] <- alt$five[dim(alt)[1] -1] 
  alt$fiveChar[dim(alt)[1]] <- alt$fiveChar[dim(alt)[1] -1]
  alt$P1[dim(alt)[1]] <- alt$P1[dim(alt)[1] -1]
  alt$P2[dim(alt)[1]] <- alt$P2[dim(alt)[1] -1]
  alt$P3[dim(alt)[1]] <- alt$P3[dim(alt)[1] -1]
  alt$P4[dim(alt)[1]] <- alt$P4[dim(alt)[1] -1]
  alt$P5[dim(alt)[1]] <- alt$P5[dim(alt)[1] -1]
  return(alt)
}
   ))))





#running the next part in parallel makes it run more than two times faster

cl <- makeCluster(detectCores())

clusterEvalQ(cl, require(parallel))
clusterEvalQ(cl, source("EL_functions.R"))
clusterEvalQ(cl, require(tidyverse))

tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"assist"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"turnover"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"made3p"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"attempted3p"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"made2p"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"attempted2p"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"madeFT"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"attemptedFT"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"defReb"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"offReb"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"totReb"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"steal"))))
tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) opp_oFoul(game[[1]],game[[2]]))))



tList <- lapply(tList, function(season) lapply(season, function(team) lapply(team, function(games) lapply(games, function(alt)
  alt %>% mutate(posCheck=as.numeric(posChecker))))))

tList <- lapply(tList, function(season) parLapply(cl, season, function(team) lapply(team, function(game) oppStat(game[[1]],game[[2]],"posCheck"))))



saveRDS(tList,"processed_data.RDS")


