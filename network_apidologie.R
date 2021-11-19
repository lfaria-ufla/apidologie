library(igraph)
library(tidyverse)
setwd("~/estatistica/R/epifanio")

rm(list=ls())
dados <- read.csv("./dados.gerais.apidologie.csv",sep = ";", head = T)


links <- dados %>% 
  group_by(sp_planta) %>% summarise_all(sum) #%>% column_to_rownames("sp_planta")

make_matrix <- function(df,rownames = NULL){
  my_matrix <-  as.matrix(df)
  if(!is.null(rownames))
    rownames(my_matrix) = rownames
  my_matrix
}

(fw = tibble(links))
links = make_matrix(select(fw,-1),pull(fw,1))

nodes = read.delim(file = "nodes.csv",sep = ";", head = T)

# Examine the data:
head(nodes)
head(links)
nrow(nodes); length(unique(nodes$id))
anyDuplicated(nodes$id) 

links <- as.matrix(links)
dim(links)
dim(nodes)


g1 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g1)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g1)
bet <- igraph::betweenness(g1)
clos <- igraph::closeness(g1)
eig <- igraph::eigen_centrality(g1)$vector
V(g1)$label.cex <- 0.5
V(g1)$label.color = "black"
V(g1)$label.font = 0.5
V(g1)$shape <- ifelse(V(g1)$type, "circle", "circle")
E(g1)$color <- "gray"
V(g1)$soc = as.character(nodes$sociality)
V(g1)$ord = as.character(nodes$ordem)
V(g1)$fam = as.character(nodes$familia)
V(g1)$weight = as.numeric(nodes$weight)
V(g1)$closeness = as.numeric(nodes$closeness)
V(g1)$c = as.numeric(nodes$c)
V(g1)$z = as.numeric(nodes$z)

cluster_louvain(g1)

names(nodes)
V(g1)[soc=="Yes"]$size  = V(g1)$closeness*11000
V(g1)[soc=="No"]$size  = V(g1)$closeness*11000
V(g1)[soc=="P"]$size  = 3


V(g1)[soc=="Yes"]$color = "cornflowerblue"
V(g1)[soc=="No"]$color = "firebrick"
V(g1)[soc=="P"]$color = "aquamarine4"
V(g1)[soc=="P"]$frame.color = "aquamarine4"
V(g1)[soc=="Yes"]$frame.color = "cornflowerblue"
V(g1)[soc=="No"]$frame.color = "firebrick"


g2 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g2)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g2)
bet <- igraph::betweenness(g2)
clos <- igraph::closeness(g2)
eig <- igraph::eigen_centrality(g2)$vector
V(g2)$label.cex <- 0.5
V(g2)$label.color = "black"
V(g2)$label.font = 0.5
V(g2)$shape <- ifelse(V(g2)$type, "circle", "circle")
E(g2)$color <- "gray"
V(g2)$soc = as.character(nodes$sociality)
V(g2)$ord = as.character(nodes$ordem)
V(g2)$fam = as.character(nodes$familia)
V(g2)$weight = as.numeric(nodes$weight)
V(g2)$closeness = as.numeric(nodes$closeness)
V(g2)$c = as.numeric(nodes$c)
V(g2)$z = as.numeric(nodes$z)

V(g2)[soc=="Yes"]$size  = V(g1)$c*12
V(g2)[soc=="No"]$size  = V(g1)$c*12
V(g2)[soc=="P"]$size  = 3

V(g2)[soc=="Yes"]$color = "cornflowerblue"
V(g2)[soc=="No"]$color = "firebrick"
V(g2)[soc=="P"]$color = "aquamarine4"
V(g2)[soc=="P"]$frame.color = "aquamarine4"
V(g2)[soc=="Yes"]$frame.color = "cornflowerblue"
V(g2)[soc=="No"]$frame.color = "firebrick"


g4 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g4)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g4)
bet <- igraph::betweenness(g4)
clos <- igraph::closeness(g4)
eig <- igraph::eigen_centrality(g4)$vector
V(g4)$label.cex <- 0.5
V(g4)$label.color = "black"
V(g4)$label.font = 0.5
V(g4)$shape <- ifelse(V(g4)$type, "circle", "circle")
E(g4)$color <- "gray"
V(g4)$soc = as.character(nodes$sociality)
V(g4)$ord = as.character(nodes$ordem)
V(g4)$fam = as.character(nodes$familia)
V(g4)$weight = as.numeric(nodes$weight)

V(g4)[soc=="Yes"]$size  = sqrt(deg)*1.2
V(g4)[soc=="No"]$size  = sqrt(deg)*1.2
V(g4)[soc=="P"]$size  = 3

V(g4)[soc=="Yes"]$color = "cornflowerblue"
V(g4)[soc=="No"]$color = "firebrick"
V(g4)[soc=="P"]$color = "aquamarine4"
V(g4)[soc=="P"]$frame.color = "aquamarine4"
V(g4)[soc=="Yes"]$frame.color = "cornflowerblue"
V(g4)[soc=="No"]$frame.color = "firebrick"


g3 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g3)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g3)
bet <- igraph::betweenness(g3)
clos <- igraph::closeness(g3)
eig <- igraph::eigen_centrality(g3)$vector
V(g3)$label.cex <- 0.5
V(g3)$label.color = "black"
V(g3)$label.font = 0.5
V(g3)$shape <- ifelse(V(g3)$type, "circle", "circle")
E(g3)$color <- "gray"
V(g3)$soc = as.character(nodes$sociality)
V(g3)$ord = as.character(nodes$ordem)
V(g3)$fam = as.character(nodes$familia)
V(g3)$weight = as.numeric(nodes$weight)
V(g3)$closeness = as.numeric(nodes$closeness)
V(g3)$c = as.numeric(nodes$c)
V(g3)$z = as.numeric(nodes$z)

V(g3)[soc=="Yes"]$size  = V(g1)$z+8
V(g3)[soc=="No"]$size  = V(g1)$z+8
V(g3)[soc=="P"]$size  = 3

V(g3)[soc=="Yes"]$color = "cornflowerblue"
V(g3)[soc=="No"]$color = "firebrick"
V(g3)[soc=="P"]$color = "aquamarine4"
V(g3)[soc=="P"]$frame.color = "aquamarine4"
V(g3)[soc=="Yes"]$frame.color = "cornflowerblue"
V(g3)[soc=="No"]$frame.color = "firebrick"

g5 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g5)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g5)
bet <- igraph::betweenness(g5)
clos <- igraph::closeness(g5)
eig <- igraph::eigen_centrality(g5)$vector
V(g5)$label.cex <- 0.5
V(g5)$label.color = "black"
V(g5)$label.font = 0.5
V(g5)$shape <- ifelse(V(g5)$type, "circle", "circle")
E(g5)$color <- "gray"
V(g5)$soc = as.character(nodes$sociality)
V(g5)$ord = as.character(nodes$ordem)
V(g5)$fam = as.character(nodes$familia)
V(g5)$weight = as.numeric(nodes$weight)
V(g5)$closeness = as.numeric(nodes$closeness)
V(g5)$c = as.numeric(nodes$c)
V(g5)$z = as.numeric(nodes$z)

V(g5)[soc=="Yes"]$size  = eig * 20
V(g5)[soc=="No"]$size  = eig *20
V(g5)[soc=="P"]$size  = 3

V(g5)[soc=="Yes"]$color = "cornflowerblue"
V(g5)[soc=="No"]$color = "firebrick"
V(g5)[soc=="P"]$color = "aquamarine4"
V(g5)[soc=="P"]$frame.color = "aquamarine4"
V(g5)[soc=="Yes"]$frame.color = "cornflowerblue"
V(g5)[soc=="No"]$frame.color = "firebrick"


g6 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g6)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g6)
bet <- igraph::betweenness(g6)
clos <- igraph::closeness(g6)
eig <- igraph::eigen_centrality(g6)$vector
V(g6)$label.cex <- 0.5
V(g6)$label.color = "black"
V(g6)$label.font = 0.5
V(g6)$shape <- ifelse(V(g6)$type, "circle", "circle")
E(g6)$color <- "gray"
V(g6)$soc = as.character(nodes$sociality)
V(g6)$ord = as.character(nodes$ordem)
V(g6)$fam = as.character(nodes$familia)
V(g6)$weight = as.numeric(nodes$weight)
V(g6)$closeness = as.numeric(nodes$closeness)
V(g6)$c = as.numeric(nodes$c)
V(g6)$z = as.numeric(nodes$z)

V(g6)[soc=="Yes"]$size  = NA
V(g6)[soc=="No"]$size  = NA
V(g6)[soc=="P"]$size  = NA

V(g6)[soc=="Yes"]$color = "cornflowerblue"
V(g6)[soc=="No"]$color = "firebrick"
V(g6)[soc=="P"]$color = "aquamarine4"
V(g6)[soc=="P"]$frame.color = "aquamarine4"
V(g6)[soc=="Yes"]$frame.color = "cornflowerblue"
V(g6)[soc=="No"]$frame.color = "firebrick"

g7 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g7)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g7)
bet <- igraph::betweenness(g7)
clos <- igraph::closeness(g7)
eig <- igraph::eigen_centrality(g7)$vector
V(g7)$label.cex <- 0.5
V(g7)$label.color = "black"
V(g7)$label.font = 0.5
V(g7)$shape <- ifelse(V(g7)$type, "circle", "square")
E(g7)$color <- "gray"
V(g7)$soc = as.character(nodes$sociality)
V(g7)$ord = as.character(nodes$ordem)
V(g7)$fam = as.character(nodes$familia)
V(g7)$weight = as.numeric(nodes$weight)
V(g7)$closeness = as.numeric(nodes$closeness)
V(g7)$c = as.numeric(nodes$c)
V(g7)$z = as.numeric(nodes$z)
V(g7)[soc=="Yes"]$size  = eig * 10
V(g7)[soc=="No"]$size  = eig *10
V(g7)[soc=="P"]$size  = 5

communityMulti <- multilevel.community(g7)
communityMulti$modularity

V(g7)$color <- membership(communityMulti)
plot(g7, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Degree Centrality", 
     layout = layout.graphopt)




tiff("rede.tiff", units="in",
     width=10, height=6, res=300)
par(mar = c(0,0,1,0))
par(mfrow = c(2, 3))
plot(g4, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Degree Centrality", 
     layout = layout.graphopt)

plot(g5, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Eigenvector Centrality", 
     layout = layout.graphopt)

plot(g1, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Closeness Centrality",
     layout = layout.graphopt)

plot(g2, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Between-module Connectivity",
     layout = layout.graphopt)

plot(g3, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Within-module Connectivity",
     layout = layout.graphopt)


plot(g6, vertex.label = NA, edge.width = NA,
     edge.color = "grey", main = "",
     layout = layout.graphopt)

legend(x=0.25, y=0.8, 
       c("Social","Solitary", "Plants"), pch=21,
       col="#777777", 
       pt.bg= c("cornflowerblue", "firebrick", "aquamarine4"),
       pt.cex=3, cex=2, bty="n", ncol=1)
legend(x=0.25, y=0.4, "???weights",
       pch=21,
       col="black", 
       pt.cex=2, cex=2, bty="n", ncol=1)
legend(x=0.25, y=0.33, "",
       pch=21,
       col="black", 
       pt.cex=3, cex=2, bty="n", ncol=1)
legend(x=0.25, y=0.25, "???weights",
       pch=21,
       col="black", 
       pt.cex=4, cex=2, bty="n", ncol=1)

dev.off()


library(bipartite)

###calculando c & z ###
mod <- computeModules(links)
###importante depois que tiverem gerado os valores das metricas para as espécies
### precisa agrupar os dados em um data frame unico

####para calcular o nulo para modularidade
nulls <- nullmodel(links, N=1000) # this should be larger, of course
null.mod.list <- sapply(nulls, computeModules)
null.cs <- sapply(null.mod.list, function(x) x@likelihood) # c-values across all species in nulls
mean(null.cs)
sd(null.cs)
mod@likelihood
praw <- sum(null.cs>(mod@likelihood)) / length(null.cs)
ifelse(praw > 0.5, 1-praw, praw)                                  # P-value
praw

####hubs, authority and assortative
g9 = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(g9)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(g9)
bet <- igraph::betweenness(g9)
clos <- igraph::closeness(g9)
eig <- igraph::eigen_centrality(g9)$vector
V(g9)$label.cex <- 0.5
V(g9)$label.color = "black"
V(g9)$label.font = 0.5
V(g9)$shape <- ifelse(V(g9)$type, "circle", "circle")
E(g9)$color <- "gray"
V(g9)$soc = as.character(nodes$sociality)
V(g9)$ord = as.character(nodes$ordem)
V(g9)$fam = as.character(nodes$familia)
V(g9)$weight = as.numeric(nodes$weight)
V(g9)$closeness = as.numeric(nodes$closeness)
V(g9)$c = as.numeric(nodes$c)
V(g9)$z = as.numeric(nodes$z)
V(g9)$size  = 1.5

V(g9)$hubs = hub.score(g9)$vector 
V(g9)$hubs[1:10]

au = authority_score(g9)$vector
hub = order(-hub.score(g9)$vector)

write.csv(au, file = "authority.csv")

head(order(au), n=10)
au[103]; au[7]; au[53]; au[43]; au[12]; au[3]; au[17]; au[100]; au[33]; au[105]

assortativity_degree(g9, directed=T)

####plotting modularity
gm = graph_from_incidence_matrix(links, weighted = NULL)

types <- V(gm)$type  ## getting each vertex `type` let's us sort easily
deg <- igraph::degree(gm)
bet <- igraph::betweenness(gm)
clos <- igraph::closeness(gm)
eig <- igraph::eigen_centrality(gm)$vector
V(gm)$label.cex <- 0.5
V(gm)$label.color = "black"
V(gm)$label.font = 0.5
V(gm)$shape <- ifelse(V(gm)$type, "circle", "square")
E(gm)$color <- "gray"
V(gm)$soc = as.character(nodes$sociality)
V(gm)$ord = as.character(nodes$ordem)
V(gm)$fam = as.character(nodes$familia)
V(gm)$weight = as.numeric(nodes$weight)
V(gm)$closeness = as.numeric(nodes$closeness)
V(gm)$c = as.numeric(nodes$c)
V(gm)$z = as.numeric(nodes$z)
V(gm)[soc=="Yes"]$size  = eig * 20
V(gm)[soc=="No"]$size  = eig *20
V(gm)[soc=="P"]$size  = 5

c1 = cluster_edge_betweenness(gm, bridges = TRUE, 
                              edge.betweenness = TRUE)
V(gm)$color <- membership(c1)

dendPlot(c1, mode="hclust")
tiff("net_modu_eig.tiff", units="in",
     width=10, height=6, res=300)
par(mar = c(0,0,1,0))
plot(gm, vertex.label = NA, edge.width = 1, 
     edge.color = "grey", main = "Modularity and Eigenvalues (weights)", 
     layout = layout_with_graphopt)
dev.off()
