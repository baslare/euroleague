




event_parser <- function(game_data=season_list[[1]],team=season_list[[1]]$header$CodeTeamA){
  
  qrtr_count <- sapply(game_data$playbyplay,is.data.frame)
  qrtrs <- game_data$playbyplay[qrtr_count] # the number of quarters
  qrtrs <-  qrtrs %>% bind_rows()
  qrtrs$quarter <- ifelse(qrtrs$MINUTE <= 41, 1 + floor(qrtrs$MINUTE/10),4 + ceiling((qrtrs$MINUTE-40)/5))
  
  allevents <- qrtrs
  
  isHome <- team == (game_data$header$CodeTeamA) %>% str_squish()
  
  if(isHome){
    currentFive <- game_data$home_players$ac[which(game_data$home_players$st == 1)]
  }else{
    currentFive <- game_data$away_players$ac[which(game_data$away_players$st == 1)]
  }
  
  
  
  
  plyrIn <-  allevents$PLAYTYPE == "IN"
  plyrOut <- allevents$PLAYTYPE == "OUT"
  
  allevents <- data.frame(allevents,plyrIn,plyrOut)
  allevents$PLAYER_ID <- allevents$PLAYER_ID %>% str_squish()
  allevents <- allevents[allevents$CODETEAM %>% str_squish()==team,]
  
  if(dim(allevents)[1] > 0){
    
    allevents$P1 <- ""
    allevents$P2 <- ""
    allevents$P3 <- ""
    allevents$P4 <- ""
    allevents$P5 <- ""
    #colnames(allevents)[3] <- "event"
    
    idxOut <- numeric()
    
    incPlayer <- character()
    outPlayer <- character()
    
    for (i in 1:length(allevents$plyrOut)) {
      
      
      
      if(allevents$plyrOut[i]){
        
        idxOut <- which(currentFive == allevents$PLAYER_ID[i])
        
        
        
        if(is_empty(outPlayer)){#no prior records for a subbed out player
          
          if(is_empty(incPlayer)){ #no prior records for a subbed in player. Can't do anything other than making a record of the subbed out player
            
            outPlayer <- currentFive[which(currentFive == allevents$PLAYER_ID[i])] #must create the record variable, in case more players are subbed out.
            
            
            
            
          }else { #no prior records for a subbed out player, however there's a single record for a subbed in player. The player can be subbed in for this specific subbed out player.
            
            currentFive[idxOut[1]] <- incPlayer[1]
            incPlayer <- incPlayer[-1]
            
          }
          
        }else{ #at least one record of a subbed out player
          if(is_empty(incPlayer)){
            outPlayer[length(outPlayer)+1] <- currentFive[idxOut]
            
          }else{
            outPlayer[length(outPlayer)+1] <- currentFive[idxOut]
            currentFive[idxOut[1]] <- incPlayer[1]
            incPlayer <- incPlayer[-1]
            
          }
          
        }
      }
      
      
      
      if(allevents$plyrIn[i]){
        if(is_empty(outPlayer)){ #no prior records of subbed out player, player that's subbed in was recored eariler. Then, the players cannot be switched right away. 
          
          if(is_empty(incPlayer)){ # to see if a multiple subbing is about to take place
            
            
            
            
            incPlayer <-  allevents$PLAYER_ID[i] 
            
            
          }else{ #there's a record for another player being subbed in. the next record is going to make this a multiple sub.
            
            
            
            
            incPlayer[length(incPlayer) +1] <-  allevents$PLAYER_ID[i]
            
            
            
            
          }
        }else if(length(outPlayer) == 1){ #there's a prior record for single a subbed out player, and this particular record belongs to a subbed in player. 
          #                                                  #Then, the subbing can take place straight away
          if(is_empty(incPlayer)){
            
            
            currentFive[which(currentFive ==  outPlayer[1])] <- allevents$PLAYER_ID[i]
            outPlayer <- outPlayer[-1] ##use the array as a queue
            
            
            
          }else{
            currentFive[which(currentFive ==  outPlayer[1])] <- incPlayer[1]
            incPlayer <- incPlayer[-1]
            
            incPlayer[length(incPlayer) +1] <- allevents$PLAYER_ID[i]
            
            
            outPlayer <- outPlayer[-1]
          }
          
          
        }else{       #there's a record for multiple players getting subbed out.
          if(is_empty(incPlayer)){
            
            
            currentFive[which(currentFive == outPlayer[1])] <- allevents$PLAYER_ID[i]
            
            
            
            outPlayer <- outPlayer[-1]
          }else{
            
            
            incPlayer[length(incPlayer) +1] <-  allevents$PLAYER_ID
            
            
            
            currentFive[which(currentFive == outPlayer[1])] <- incPlayer[1]
            outPlayer <- outPlayer[-1]
            incPlayer <- incPlayer[-1]
            
          }
          
        }
      }
      
      
      
      
      
      for(j in 1:5){
        allevents[i,17+j] <- currentFive[j]
      }
      
      
      
      
      
      
    }
  
  return(allevents)
}



  

}





posFinder <- function(dnm){
  
  dnm$posChecker <- FALSE
  rv <- FALSE
  timeFT <- ""
  timeRV <- ""
  timeFG <- ""
  for(i in 1:dim(dnm)[1]){
    
    
    if(dnm$PLAYTYPE[i] %>% str_detect("FGM")){
      timeFG <- dnm$MARKERTIME[i]
    }
    
    
    if(rv){
      if(dnm$PLAYTYPE[i] == "RV"){
        rv <- TRUE
        timeRV <- dnm$MARKERTIME[i]
        indexRV <- i
        
      }else if(dnm$PLAYTYPE[i] %>% str_detect("FT")){
        timeFT <- dnm$MARKERTIME[i]
        indexFT <- i
        
      }
    }else{
      if(dnm$PLAYTYPE[i] == "RV"){
        rv <- TRUE
        timeRV <- dnm$MARKERTIME[i]
        indexRV <- i
        
      }else if(dnm$PLAYTYPE[i] %>% str_detect("FT")){
        timeFT <- dnm$MARKERTIME[i]
        indexFT <- i
        
      }
    }
    
    if(rv & (timeFT == timeRV)){
      if(timeFG == timeFT){
        dnm$posChecker[indexRV] <- FALSE
        rv <- FALSE
      }else{
        dnm$posChecker[indexRV] <- TRUE
        rv <- FALSE
      }
      
      
    }
    
    
  }
  
  return(dnm)
  
}




oppStat <- function(tt,ops,variable){
  startIndex = 1
  endIndex = 1
  mutVar <- paste(variable,"Op",sep ="")
  tt <- tt %>% mutate(!!mutVar := 0)
  
  for(i in 1:dim(tt)[1]){
    if(!tt$fiveCheck[i] | i == dim(tt)[1]){
      endIndex <- i
      rangeLow <- tt$cumSec[startIndex]
      rangeHigh <- tt$cumSec[endIndex]
      if(i == dim(tt)[1]){
        
          
        
          stat <- sum((ops %>% filter(cumSec <= max(cumSec) & cumSec >= tt$cumSec[startIndex]) %>% select(variable))[,1])
        
       
      }else{
      
          
      
          stat <- sum((ops %>% filter(cumSec < tt$cumSec[endIndex] & cumSec >= tt$cumSec[startIndex]) %>% select(variable))[,1])
        
        
        
      }
      
      tt[startIndex,] <- tt[startIndex,] %>% mutate(!!mutVar := stat[1]) 
      
      
      startIndex <- endIndex + 1
    }
  }
  retList <- list()
  retList[["team"]] <- tt
  retList[["opp"]] <- ops
  return(retList)
}


opp_oFoul <- function(tt,ops){
  tt <- tt %>% mutate(oFoulOp = 0)
  
  
  all_ofouls <- which(ops$oFoul == 1)
  if(length(all_ofouls) > 0){
    ofoul_time <- ops$MARKERTIME[all_ofouls]
    ofoul_time_tt <- which((tt$MARKERTIME %in% ofoul_time ) & tt$event == "RV")
    tt$oFoulOp[ofoul_time_tt] <- 1
  }
    
    retList <- list()
    retList[["team"]] <- tt
    retList[["opp"]] <- ops
    return(retList)
    
  
  
}


