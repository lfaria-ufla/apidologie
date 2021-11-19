#cabeçalho
rm(list=ls())
setwd("~/estatistica/R/epifanio")
dados=read.delim(file= "nodes.apidologie.csv", sep= ";", header = T)
names(dados)
str(dados)
dados

dados$sociality = as.factor(dados$sociality)
dados$ordem = as.factor(dados$ordem)
dados$familia = as.factor(dados$familia)
dados$degree = as.numeric(dados$degree)
str(dados)
summary(dados)
names(dados)

library(ggplot2)
library(tidyverse)

data = dados%>%
  select(id, sociality, degree, betweenness, closeness, 
         c, z, core, eig, ordem)%>%
  mutate(id = recode(id, "trigona_spinipes" = "Trigona spinipes", 
                           "centris_tarsata" = "Centris tarsata" , 
                           "dialictus_sp" = "Dialictus sp", 
                           "actinote_pellenea" = "Actinote pellenea", 
                           "toxomerus_sp" = "Toxomerus sp", 
                           "melipona_quinquefasciata" = "Melipona quinquefasciata", 
                           "apis_mellifera" = "Apis mellifera", 
                           "melipona_quadrifasciata" = "Melipona quadrifasciata", 
                           "tetragona_clavipes" = "Tetragona clavipes", 
                           "bombus_atratus" = "Bombus atratus", 
                           "bombus_morio" = "Bombus morio", 
                           "euglossa_sp" = "Euglossa sp", 
                           "palpada_conica" = "Palpada conica", 
                           "xylocopa_subsciane" = "Xylocopa subsciane", 
                           "paratrigona_subnuda" = "Paratrigona subnuda", 
                           "eulaema_nigrita" = "Eulaema nigrita"))



##########################################################
#####histogramas
data = dados%>%
  select(id, sociality, degree, betweenness, 
         closeness, c, z, core, eig, ordem)%>%
  filter(ordem  != "P")%>%
  drop_na()
mean(data$z)

library(ggplot2)
library(RColorBrewer)

plot_multi_histogram <- function(data, feature, label_column) {
  plt <- ggplot(data, aes(x=eval(parse(text=feature)), 
                          fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.5, position="identity", aes(y = ..density..), 
                   color="black") +
    geom_density(alpha=0.5) +
    # geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="gray20", 
    #            linetype="dashed", size=0.7) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
       }

hd = plot_multi_histogram(data, 'degree', 'sociality')
hd = hd + guides(fill=guide_legend(title="Sociality")) +
  guides(color=guide_legend(title="Sociality"))+
  scale_fill_brewer(palette="Set1") + theme_classic(base_size = 15) +
  labs(x = "Degree Centrality") + scale_color_brewer(palette="Set1")+
  theme(legend.position = "none")
hd 


hb = plot_multi_histogram(data, 'betweenness', 'sociality')
hb = hb + guides(fill=guide_legend(title="Sociality")) +
  scale_fill_brewer(palette="Set1") + theme_classic(base_size = 15)+
  labs(x = "Betweenness Centrality")+
  theme(legend.position = "none")
hb


hcl = plot_multi_histogram(data, 'closeness', 'sociality')
hcl = hcl + guides(fill=guide_legend(title="Sociality")) +
  scale_fill_brewer(palette="Set1") + theme_classic(base_size = 15)+
  labs(x = "Closeness Centrality")+
  theme(legend.position = "none")
hcl


hc = plot_multi_histogram(data, 'c', 'sociality')
hc = hc + guides(fill=guide_legend(title="Sociality")) +
  scale_fill_brewer(palette="Set1") + theme_classic(base_size = 15)+
  labs(x = "Between-module Connectivity (c)")+
  theme(legend.position = "none")
hc


hz = plot_multi_histogram(data, 'z', 'sociality')
hz = hz + guides(fill=guide_legend(title="Sociality")) +
  scale_fill_brewer(palette="Set1") + theme_classic(base_size = 15)+
  labs(x = "Within-module Connectivity (z)")+
  theme(legend.position = "none")
hz


hco = plot_multi_histogram(data, 'eig', 'sociality')
hco = hco + guides(fill=guide_legend(title="Sociality")) +
  scale_fill_brewer(palette="Set1") + theme_classic(base_size = 15)+
  labs(x = "Eigenvector Centrality")+
  theme(legend.position = "none")
hco


###boxplot
data = dados%>%
  select(sociality, degree,closeness,  
         c, z, eig, ordem)%>%
  filter(ordem  != "P")%>%
  drop_na()%>%
  mutate_if(is.numeric, scale)%>%
  rename( "Degree" = "degree","Eigenvector" = "eig" , 
          "Closeness" = "closeness", 
          "C" = "c", 
          "Z" = "z")%>%
  gather(key = metrics, value = value, -sociality, -ordem)

bo = ggplot(data, aes(x = as.factor(metrics), y = value, 
                 fill = as.factor(sociality)))+
  geom_boxplot()+theme_classic(base_size = 15)+
  scale_fill_brewer(palette="Set1")+
  guides(fill=guide_legend(title="Sociality"))+
  labs(x = "Network Metrics", y = "Values (scale)")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

bo



library(ggpubr)
leg = get_legend(hd)
hd = hd+theme(legend.position = "none")
hco = hco+theme(legend.position = "none")
hcl = hcl+theme(legend.position = "none")
hc = hc+theme(legend.position = "none")
hz = hz+theme(legend.position = "none")

tiff("figure_metrics_dispersion.tiff", units="in", width=15, height=8, res=300)
ggarrange(hd,hco,hcl, hc, hz, bo,  labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 3, nrow = 2)
dev.off()
