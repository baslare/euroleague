require(tidyverse)
require(parallel)
require(xgboost)

tList <- readRDS("processed_data.RDS")

cl <- makeCluster(detectCores())
clusterEvalQ(cl, require(tidyverse))
clusterEvalQ(cl, require(parallel))

gameStats <- tList 
gameStats <- lapply(gameStats, function(season) lapply(season, function(team) lapply(team, function(games) games[[1]])))



begin <- Sys.time()
gameStats <- lapply(gameStats, function(season) parLapply(cl, season, function(team) lapply(team, function(games){
  
  isN <- colnames(games)[which(sapply(games,is.numeric))]
  games %>% group_by(CODETEAM) %>% summarise_at(.vars = isN,.funs = sum)
  
}
  )))
print(Sys.time() - begin)


gameStats <-lapply(gameStats, function(season) lapply(season, function(team) team %>% bind_rows(.id = "opp") %>% filter(CODETEAM!="FALSE")))
gameStats <-lapply(gameStats, function(season) lapply(season, function(team) team %>% mutate(game_count=1:n()))) 
gameStats <-lapply(gameStats, function(season) lapply(season, function(team) team %>% mutate(pts=3*made3p +2*made2p +madeFT,
                                                                                             oppPts=3*made3pOp + 2*made2pOp + madeFTOp,
                                                                                             oEFF = 100*pts/possession,
                                                                                             dEFF = 100*oppPts/possessionOp,
                                                                                             dReb_pct = defReb/(defReb+offRebOp),
                                                                                             oReb_pct = offReb/(defRebOp+offReb),
                                                                                             oEFF_Op = 100*oppPts/possessionOp,
                                                                                             dEFF_Op =100*pts/possession,
                                                                                             pace = 40*possession/((duration/60)),
                                                                                             paceOp = 40*possessionOp/((duration/60)),
                                                                                             win = as.numeric(pts > oppPts)
                                                                                             )))

gameStats <-lapply(gameStats, function(season) lapply(season, function(team) 
  team %>% group_by(CODETEAM,opp) %>% mutate(enc=1:n()) %>% ungroup()
))   

   
  
  

gameStats <-lapply(gameStats, function(season) lapply(season, function(team) 
  team %>% select(-c(MINUTE,POINTS_A,POINTS_B,quarter,timeNum))
))   



#cl <- makeCluster(detectCores())
#clusterEvalQ(cl, require(tidyverse))
#clusterEvalQ(cl, require(parallel))

begin <- Sys.time()
gameStats <- lapply(gameStats, function(season) parLapply(cl, season, function(team){
  tmp <- lapply(team$game_count, function(x){
    isN <- colnames(team)[which(sapply(team,is.numeric))]
    flt <- team %>% filter(game_count <= x)
    
    if(dim(flt)[1] < 6){
      flt <- flt %>% group_by(CODETEAM) %>% summarise_at(.vars = isN,.funs = mean)
    }else{
      flt <- flt[((dim(flt)[1])-3):(dim(flt)[1]-1),]
      flt <- flt %>% group_by(CODETEAM) %>% summarise_at(.vars = isN,.funs = mean)
    }
    
  })
  
  tmp <- tmp %>% bind_rows()
  
  tmp$opp <- team$opp 
  tmp$enc <- team$enc
  tmp$pts <- team$pts
  tmp$oppPts <- team$oppPts
  tmp$game_count <- team$game_count
  
  return(tmp)
}))
print(Sys.time() - begin)


gameStats2 <- lapply(gameStats, function(season) lapply(season, function(team){
  team <- team %>% mutate(fg_2 = made2p/attempted2p,
                          fg_3 = made3p/attempted3p,
                          fg_ft = madeFT/attemptedFT,
                          fg_2_Op = made2pOp/attempted2pOp,
                          fg_3_Op = made3pOp/attempted3pOp,
                          fg_ft_Op = madeFTOp/attemptedFTOp,
                          dReb_pct = defReb/(defReb+offRebOp),
                          oReb_pct = offReb/(defRebOp+offReb))
  
  team <- team %>% select(CODETEAM,opp,game_count,win,enc,pace,assist,turnover,fg_2,fg_3,fg_ft,defReb,dReb_pct,offReb,oReb_pct,foul,foulDrawn,block,rejected,steal,assistOp,turnoverOp,fg_2_Op,fg_3_Op,fg_ft_Op,defRebOp,offRebOp,stealOp,oEFF,dEFF,oEFF_Op,dEFF_Op,pts,oppPts)
  
  return(team)
}))

gS_names <- lapply(gameStats2,names)


ff <- function(a,b){
  
  idx <- which(b$opp == a)
  if(length(idx) > 0){
    return(b[idx,])
  }else{
    return(NULL)
  }
  
  
}

gameStats_others <- Map(function(name,df) lapply(name, function(y) lapply(df, function(x) ff(y,x))),name=gS_names,df=gameStats2)
gameStats_others <- Map(function(x,y) setNames(x,y),x=gameStats_others,y=gS_names)
gameStats_others <- lapply(gameStats_others, function(y) lapply(y,  function(x)x[lengths(x) != 0]))

gameStats_others <- lapply(gameStats_others,function(x) lapply(x, bind_rows))
gameStats_merged <- Map(function(x,y) Map(function(a,b) left_join(a %>% mutate(across(where(is.character), str_trim)),b %>% mutate(across(where(is.character), str_trim)),by=c("CODETEAM"="opp","opp"="CODETEAM","enc"),suffix=c("","_other")),a=x,b=y),x=gameStats2,y=gameStats_others)

gameStats_merged <- lapply(gameStats_merged, bind_rows)
gameStats_merged <- gameStats_merged %>% bind_rows(.id = "season")  
  
stats_other_op <- colnames(gameStats_merged)[which(colnames(gameStats_merged) %>% str_detect("_other")  &  (colnames(gameStats_merged) %>% str_detect("Op")))]
stats_other <- colnames(gameStats_merged)[which(colnames(gameStats_merged) %>% str_detect("_other")  &  !(colnames(gameStats_merged) %>% str_detect("Op")))] 
stats_op <- colnames(gameStats_merged)[which(!colnames(gameStats_merged) %>% str_detect("_other")  &  (colnames(gameStats_merged) %>% str_detect("Op")))]
stats_ <- colnames(gameStats_merged)[which(!colnames(gameStats_merged) %>% str_detect("_other")  &  !(colnames(gameStats_merged) %>% str_detect("Op")))]

stats_ <- stats_[-(1:6)]
stats_other <- stats_other[-(1:2)]

stats_idx <- sapply(stats_, function(x) sum(sapply(stats_other_op, function(y) y %>% str_detect(x) )))
stats_op_idx <- sapply(stats_other %>% str_remove("_other"), function(x) sum(sapply(stats_op, function(y) y %>% str_detect(x) )))



for(x in 1:length(stats_[stats_idx == 1])){
  
  
  varName <- paste0(stats_[stats_idx==1][x] %>% str_remove_all("Op|_other"),"_s")
  var1 <- stats_[stats_idx==1][x]
  var2 <- stats_other_op[x]
  
  gameStats_merged <- gameStats_merged %>% mutate(!!varName := gameStats_merged[[var1]] - gameStats_merged[[var2]])
  
  
}

for(x in 1:length(stats_other[stats_op_idx == 1])){
  
  
  varName <- paste0(stats_other[stats_op_idx == 1][x] %>% str_remove_all("Op|_other"),"_a")
  var1 <- stats_other[stats_op_idx == 1][x]
  var2 <- stats_op[x]
  
  gameStats_merged <- gameStats_merged %>% mutate(!!varName := gameStats_merged[[var1]] - gameStats_merged[[var2]])
  
  
}


gameStats_merged <- gameStats_merged %>% arrange(season,game_count)


gs_tt <- gameStats_merged[-((dim(gameStats_merged)[1]-50):(dim(gameStats_merged)[1])),]
gs_pred <- gameStats_merged[((dim(gameStats_merged)[1]-50):(dim(gameStats_merged)[1])),]



####xgboost####


gs_tt <- gs_tt[,c(stats_,stats_op,stats_other,stats_other_op)]
gs_pred <- gs_pred[,c(stats_,stats_op,stats_other,stats_other_op)]

pts_tt <- gs_tt$pts
pts_pred <- gs_pred$pts

gs_tt <- gs_tt %>% select(-c("oppPts","pts_other","oppPts_other","pts"))
gs_pred <- gs_pred %>% select(-c("oppPts","pts_other","oppPts_other","pts"))



index.test <- sample(nrow(gs_tt),floor(0.8*nrow(gs_tt)))

dataMatrix <- as.matrix(gs_tt[index.test,])
testMatrix <- as.matrix(gs_tt[-index.test,])

xgbTrain <- xgb.DMatrix(data = dataMatrix,label=pts_tt[index.test])
xgbTest <- xgb.DMatrix(data = testMatrix,label=pts_tt[-index.test])

xgb_model_full <- xgb.train(params = list(
  booster="gbtree",
  eta=0.01,
  max_depth=10,
  gamma=10,
  subsample=0.828,
  colsample_bytree=0.837,
  objective="reg:squarederror",
  eval_metric="rmse",
  num_class=1
  
),data=xgbTrain,
nrounds=20000,

early_stopping_rounds=50,verbose = 1,watchlist = list(train=xgbTrain,test=xgbTest))

xgb.ggplot.importance(xgb.importance(model=xgb_model_full))


xgbOutofSample <- xgb.DMatrix(as.matrix(gs_pred)) 




outOfSampleF = data.frame(predictions=predict(xgb_model_full,xgbOutofSample,reshape=T))
outOfSampleF$team <- gs_pred$CODETEAM
outOfSampleF$opp <- gs_pred$opp
outOfSampleF$pts <- pts_pred
outOfSampleF$ptsOpp <- gs_pred$oppPts
View(outOfSampleF)
