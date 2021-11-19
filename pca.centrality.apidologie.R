#cabeçalho
rm(list=ls())
setwd("~/estatistica/R/epifanio")
dados=read.delim(file= "nodes.apidologie.csv", sep= ";", header = T)
names(dados)
str(dados)
dados
library(tidyverse)
library(factoextra)
library(ggplot2)

dados$sociality = as.factor(dados$sociality)
dados$ordem = as.factor(dados$ordem)
dados$familia = as.factor(dados$familia)
dados$degree = as.numeric(dados$degree)
str(dados)
summary(dados)

data = dados%>%
  dplyr::select(familia, ordem, sociality)

table(data)
unique(data$ordem)

###rede total###
total = dados%>%
  dplyr::select(degree, betweenness, closeness, c, z, core,
                familia, ordem, sociality, eig, Role)%>%
  filter(sociality != "P")%>%
  rename( "Degree" = "degree","Betweenness" = "betweenness" , 
          "Closeness" = "closeness", 
          "Core/Periphery" = "core", 
          "Eigenvector" = "eig")%>%
  drop_na()

names(total)

pca_t1 = prcomp(total[,c(1:6, 10)], center = T, scale. = T)
summary(pca_t1)
fviz_eig(pca_t1, addlabels = T)
eig.val = get_pca_ind(pca_t1)
tes = eig.val$contrib[,1]
pca_t = fviz_pca_biplot(pca_t1, 
                        # Individuals
                        geom.ind = "point",
                        pointshape = 21,
                        fill.ind = total$Role,
                        col.ind = "black",
                        pointsize = 3,
                        addEllipses = T,
                        ellipse.level=0.95,
                        ellipse.type = "confidence",
                        # Variables
                        col.var = "contrib",
                        legend.title = list(fill = "Species' Role", 
                                            color = "Contr(metrics)",
                                            alpha = "Contrib"), 
                        ggtheme = theme_classic(base_size=15),
                        label = "all", 
                        palette = "Set1", 
                        title = "", 
                        repel = T)+
  scale_color_gradient2(low = "gold",mid = "red",
                        high="blue", midpoint = 13)+
  labs(x = "PC1(64%)", y = "PC2(17%)")
  
pca_t

tiff("pca_role.tiff", units="in", width=10, height=8, res=300)
pca_t
dev.off()


pca_m = prcomp(total[,c(1:6, 10)], center = T, scale. = T)
summary(pca_m)

eig.val = get_pca(pca_m)

fviz_eig(pca_m, addlabels = T)
pca_a = fviz_pca_biplot(pca_m, 
                        # Individuals
                        geom.ind = "point",
                        pointshape = 21,
                        fill.ind = total$sociality,
                        col.ind = "black",
                        pointsize = 3,
                        addEllipses = TRUE,
                        ellipse.type = "confidence",
                        # Variables
                        col.var = "contrib",
                        legend.title = list(fill = "Sociality", 
                                  color = "Contr(metrics)",
                                  pointsize = "Cont(sp)"),
                        ellipse.level=0.95, 
                        ggtheme = theme_classic(base_size=15),
                        label = "var", 
                        palette = "Set1", 
                        title = "", 
                        repel = TRUE)+
  scale_color_gradient2(low = "gold",mid = "red",
                        high="blue", midpoint = 13) +
  labs(x = "PC1(64%)", y = "PC2(17%)")
tiff("pca_api.tiff", units="in", width=8, height=6, res=300)
pca_a
dev.off()

eig.val = get_pca(pca_a)
print(eig.val)
eig.val


library(ggpubr)
tiff("figure_pca.tiff", units="in", width=15, height=15, res=300)
ggarrange(pca_a, hd, hco, hcl, hc, hz, labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3, common.legend = F, vjust = 1)
dev.off()

library(gridExtra)
tiff("figure_pca.tiff", units="in", width=15, height=15, res=300)
grid.arrange(cor, hd, hco, hcl, hc, hz, ncol=2, nrow = 3)
dev.off()

library(patchwork)
(cor | pca_a)/
  (hd | hco | hcl | hc | hz)




library(ggfortify)
library(ggplot2)
library(ggthemes)
library(EnvStats)

t = autoplot(pca_t, data=total, colour = "sociality", label = F, scale = T,
             label.size = 2, loadings = T, loadings.label = T, size = "degree", 
             frame.type = "norm", shape = "ordem", loadings.colour = 'black', 
             loadings.label.colour = "black", loadings.label.size = 4)
t=t+theme_classic()+labs(tags = "A")+
  scale_color_brewer(palette = "Set1", aesthetics = "colour")
t

library(RColorBrewer)
mycolor = brewer.pal(3, "Set1")
a = autoplot(pca_m, data=api, colour = "sociality", label = F, scale = T,
             label.size = 2, loadings = T, loadings.label = T, size = "degree", 
             frame.type = "norm", loadings.colour = 'black', 
             loadings.label.colour = "black", loadings.label.size = 4)
a=a+theme_classic()+labs(tags = "C")+
  scale_color_brewer(palette = "Set1", aesthetics = "colour")

a

