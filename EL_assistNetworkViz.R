require(tidyverse)
require(ggraph)
require(igraph)


###all teams###



vert <- team_cnx %>% group_by(from) %>% summarise(size=sum(Freq))
vert2 <- team_cnx %>% group_by(to) %>% summarise(size=sum(Freq))

non_vert <- unique(c(team_cnx$to,team_cnx$from)) %in% vert$from
size1 <- length(unique(c(team_cnx$to,team_cnx$from))[!non_vert])

if(size1 > 0){
  
  vert <- rbind(vert,data.frame(from=unique(c(team_cnx$to,team_cnx$from))[!non_vert],size=0))
  
}



non_vert_2 <- unique(c(team_cnx$to,team_cnx$from)) %in% vert2$to
size2 <- length(unique(c(team_cnx$to,team_cnx$from))[!non_vert_2])

if(size1 > 0){
  
  vert2 <- rbind(vert2,data.frame(to=unique(c(team_cnx$to,team_cnx$from))[!non_vert_2],size=0))
  
}



colnames(vert)[2] <- "from_size"
colnames(vert2)[2] <- "to_size"

vert_final <- left_join(vert,vert2,by=c("from"="to"))
vert_final$total_size <- vert_final$from_size + vert_final$to_size

normalise <- function (x, from = range(x), to = c(0, 1)) {
  x <- (x - from[1])/(from[2] - from[1])
  if (!identical(to, c(0, 1))) {
    x <- x * (to[2] - to[1]) + to[1]
  }
  x
}



vert_final$total_size_normal <- vert_final$total_size %>% normalise()
vert_final$from_ratio <- vert_final$from_size/vert_final$total_size




idf <- igraph::graph.data.frame(team_cnx,vertices = vert_final,directed = T)


ggraph(idf,layout="gem") +  
  geom_edge_parallel(aes(edge_color=stat(index),width=log(Freq),alpha=log(Freq)),sep = unit(3,"pt")) + 
  geom_node_point(aes(size=total_size,fill=from_ratio),shape=21,color="transparent") +
  geom_node_text(aes(label=name %>% tolower()),family="Noto Sans",size=2,color="#ffe0b2",nudge_y = -0.1) +
  theme(panel.background = element_rect(fill = "#1d1f21",color = "grey"),
        plot.background = element_rect(fill = "#1d1f21"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        title = element_text(family="Noto Sans",color="#ffe0b2"))  + guides(size=FALSE,alpha=FALSE,width=FALSE) +
  scale_edge_color_continuous(high = "#ff8f00",low = "#1a237e") +
  scale_edge_width_continuous(range=c(0,1)) +
  scale_fill_continuous(low = "#ff8f00",high = "#1a237e") +
  ggtitle("Assist Network",subtitle = "Alba Berlin")



  
#ad1457 = pinkish red
ggsave("graph_alba.jpg",dpi = 250,width = 10,height =8)


idf_layout = igraph::layout_with_gem(idf)



V(idf)$x <- idf_layout[,1]
V(idf)$y <- idf_layout[,2]



