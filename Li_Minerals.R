# Author -- Anirudh Prabhu
# Email -- prabha2@rpi.edu

# Li Minerals

library(igraph)

library(readxl)
NodeXL_Lithium <- read_excel("~/Downloads/Li.xlsx", 
                                  sheet = "Edges",skip = 1)

Lithium_Nodes <- read_excel("~/Downloads/Li.xlsx", 
                                  sheet = "Vertices", skip = 1)

Li_Network <- graph_from_data_frame(d = NodeXL_Lithium[,c(1,2)],directed = F,vertices = Lithium_Nodes$Vertex)


# Simple Network
plot(Li_Network)

# Network with Node sizes = degree
plot.igraph(x = Li_Network,vertex.size=(2+2*log(degree(Li_Network))),vertex.color = V(Li_Network)$cluster,asp = 1,rescale = T)

# Perform Louvain Clustering
Clus<-cluster_louvain(Li_Network)
V(Li_Network)$cluster<-Clus$membership

# Set Node colors
color<-c("green","blue","orange","yellow","lightblue","red","pink")

# Plot Simple Clustering Diagram
plot(cluster_louvain(Li_Network),Li_Network)

# Store Color as a Variable based on the Metric in the Dataset (Age)
Lithium_Nodes$Color<-color[(as.factor(Lithium_Nodes$Color))]

# Final Network Colored by Louvain Clustering
plot.igraph(x = Li_Network,vertex.size=2+2*log(degree(Li_Network)),vertex.color = V(Li_Network)$cluster,vertex.label.color = "black",layout = layout_nicely)
# Final Network Colored by Age
plot.igraph(x = Li_Network,vertex.size=2+2*log(degree(Li_Network)),vertex.color = Lithium_Nodes$Color,vertex.label.color = "black",layout = layout_with_fr)
summary(as.factor(Lithium_Nodes$Color))

#V(Li_Network)$color <- Lithium_Nodes$Color

############################

# Plotting Louvain cluster results on a map.

MinClus<-data.frame(Mineral = V(Li_Network)$name,Cluster = V(Li_Network)$cluster)

summary(as.factor(MinClus$Cluster))

MinClus1<-MinClus[MinClus$Cluster == 1,]
MinClus10<-MinClus[MinClus$Cluster == 10,]
MinClus5<-MinClus[MinClus$Cluster == 5,]
MinClus15<-MinClus[MinClus$Cluster == 15,]
MinClus18<-MinClus[MinClus$Cluster == 18,]
MinClus14<-MinClus[MinClus$Cluster == 14,]



library(stringr)
blah<-as.data.frame(str_split_fixed(GPS$Lat_Long_Decimal, ",", 2))

GPS<-cbind(GPS,blah)

GPS$V2 <- sub("^$", "0", GPS$V2)

GPS$V1<-as.numeric(as.character(GPS$V1))
GPS$V2<-as.numeric(GPS$V2)


Clus1<-GPS[grep(as.character(MinClus1$Mineral[1]), GPS$Minerals),]
for (i in 2:nrow(MinClus1)) 
{
  TempClus<-GPS[grep(as.character(MinClus1$Mineral[i]), GPS$Minerals),]
  Clus1<-rbind(TempClus,Clus1)  
}

Clus10<-GPS[grep(as.character(MinClus10$Mineral[1]), GPS$Minerals),]
for (i in 2:nrow(MinClus10)) 
{
  TempClus<-GPS[grep(as.character(MinClus10$Mineral[i]), GPS$Minerals),]
  Clus10<-rbind(TempClus,Clus10)  
}


Clus5<-GPS[grep(as.character(MinClus5$Mineral[1]), GPS$Minerals),]
for (i in 2:nrow(MinClus5)) 
{
  TempClus<-GPS[grep(as.character(MinClus5$Mineral[i]), GPS$Minerals),]
  Clus5<-rbind(TempClus,Clus5)  
}


Clus15<-GPS[grep(as.character(MinClus15$Mineral[1]), GPS$Minerals),]
for (i in 2:nrow(MinClus15)) 
{
  TempClus<-GPS[grep(as.character(MinClus15$Mineral[i]), GPS$Minerals),]
  Clus15<-rbind(TempClus,Clus15)  
}

Clus18<-GPS[grep(as.character(MinClus18$Mineral[1]), GPS$Minerals),]
for (i in 2:nrow(MinClus18)) 
{
  TempClus<-GPS[grep(as.character(MinClus18$Mineral[i]), GPS$Minerals),]
  Clus18<-rbind(TempClus,Clus18)  
}

Clus14<-GPS[grep(as.character(MinClus14$Mineral[1]), GPS$Minerals),]
for (i in 2:nrow(MinClus14)) 
{
  TempClus<-GPS[grep(as.character(MinClus14$Mineral[i]), GPS$Minerals),]
  Clus14<-rbind(TempClus,Clus14)  
}

Clus1<-Clus1[!duplicated(Clus1),]
Clus5<-Clus5[!duplicated(Clus5),]
Clus10<-Clus10[!duplicated(Clus10),]
Clus14<-Clus14[!duplicated(Clus14),]
Clus15<-Clus15[!duplicated(Clus15),]
Clus18<-Clus18[!duplicated(Clus18),]


# Writing results to a files for google map visualization
library(readr)
library(readxl)
write_csv(x = Clus1,path = "~/Desktop/DTDI/Li_Clus1.csv")
write_csv(x = Clus5,path = "~/Desktop/DTDI/Li_Clus5.csv")
write_csv(x = Clus10,path = "~/Desktop/DTDI/Li_Clus10.csv")
write_csv(x = Clus14,path = "~/Desktop/DTDI/Li_Clus14.csv")
write_csv(x = Clus15,path = "~/Desktop/DTDI/Li_Clus15.csv")
write_csv(x = Clus18,path = "~/Desktop/DTDI/Li_Clus18.csv")

# Map visualization in R
library(ggmap)
library(maptools)
library(maps)
map1 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
map1 <- ggplot() +   mapWorld
map1 <- map1+ geom_point(aes(x=Clus1$V2, y=Clus1$V1) ,color="blue", size=3)+ ggtitle(label = "Cluster 1")
map1

map5 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
map5 <- ggplot() +   mapWorld
map5 <- map5+ geom_point(aes(x=Clus5$V2, y=Clus5$V1) ,color="black", size=3)+ ggtitle(label = "Cluster 5")
map5

map10 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
map10 <- ggplot() +   mapWorld
map10 <- map10+ geom_point(aes(x=Clus10$V2, y=Clus10$V1) ,color="green", size=3)+ ggtitle(label = "Cluster 10")
map10

map14 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
map14 <- ggplot() +   mapWorld
map14 <- map14+ geom_point(aes(x=Clus14$V2, y=Clus14$V1) ,color="pink", size=3)+ ggtitle(label = "Cluster 14")
map14

map15 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
map15 <- ggplot() +   mapWorld
map15 <- map15+ geom_point(aes(x=Clus15$V2, y=Clus15$V1) ,color="red", size=3)+ ggtitle(label = "Cluster 15")
map15

map18 <- NULL
mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
map18 <- ggplot() +   mapWorld
map18 <- map18+ geom_point(aes(x=Clus18$V2, y=Clus18$V1) ,color="orange", size=3) + ggtitle(label = "Cluster 18")
map18

#grid.arrange(map1,map5,map10,map14,map15,map18, ncol = 2, main = "Li Mineral Localoties by Louvain Clustering")







