#cabeçalho
library(ggpubr)
library(RColorBrewer)
library(tidyverse)
rm(list=ls())
setwd("~/estatistica/R/epifanio")
dados=read.delim(file= "nodes.apidologie.csv", sep= ";", header = T)
options(na.action = "na.fail")
str(dados)

data = dados%>%
  filter(ordem != "P")%>%
  dplyr::select(id, ordem, Role, sociality, familia, degree, 
                betweenness, closeness, c, z, core)%>%
  drop_na()
  rename( "Degree" = "degree","Betweenness" = "betweenness" , 
        "Closeness" = "closeness", 
        "Core/Periphery" = "core", 
        "Family"= "familia", 
        "Sociality" = "sociality")

api = data%>%
  filter(familia == "Apidae")%>%
  drop_na()
ves = data%>%
  filter(familia == "Vespidae")%>%
  drop_na()

dado = rbind(ves, api)

summary(dado)
dado$Role = as.factor(dado$Role)
dado$familia = as.factor(dado$familia)
dado$sociality = as.factor(dado$sociality)

tab = table(dado$Role, dado$sociality, dado$familia)
prop.table(tab, margin = 2)




tiff("figure_cz_t.tiff", units="in", width=10, height=6, res=300)
t = ggplot(data, aes(x = c, y = z, color = Role, shape = ordem, 
                     label = id))+
  #geom_text()+
  geom_point( size = 3)+
  theme_classic(base_size = 15)+
  scale_color_brewer(palette = "Dark2")+
  geom_hline(yintercept = 2.5, lty = 2)+
  geom_vline(xintercept = 0.62, lty = 2)+ 
  labs(x = "Between-module (c)", y = "Within-module (z)", 
       shape= "Order", alpha = "Sociality")+
  annotate("text", x = 0.57, y = 3.45, label = "C. aenea")+
  annotate("text", x = 0.05, y = 3, label = "E. smaragdina")+
  annotate("text", x = 0.655, y = 2.75, label = "B. morio")+
  annotate("text", x = 0.8, y = 6.57, label = "O. tataira")+
  annotate("text", x = 0.8, y = 4.35, label = "T. clavipes")+
  annotate("text", x = 0.7, y = 3.6, label = "M. marginata")+
  annotate("text", x = 0.83, y = 2.65, label = "T. hyalinata")+
  annotate("text", x = 0.5, y = -0.7, label = "A. mellifera")+
  annotate("text", x = 0.81, y = 3.19, label = "Chrysopidae sp")+
  scale_shape_manual(values = c(15:18, 8, 25, 11))+
  scale_alpha_manual(values = c(1, 0.4))+
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2),
         alpha = guide_legend(order = 2))+
  scale_color_discrete(name = "Species' role", 
                       labels = c("Connectors",
                                  "Module Hubs",
                                  "Network Hubs",
                                  "Peripherals"))

t
dev.off()


tiff("figure_cz_fam.tiff", units="in", width=10, height=6, res=300)
p = ggplot(dado, aes(x = c, y = z, color = Role, shape = familia, 
                 label = id, alpha = sociality))+
  #geom_text()+
  geom_point(aes(shape = familia), size = 3.5)+
  theme_classic(base_size = 15)+
  scale_color_brewer(palette = "Set1")+
  geom_hline(yintercept = 2.5, lty = 2)+
  geom_vline(xintercept = 0.62, lty = 2)+ 
  labs(x = "Between-module (c)", y = "Within-module (z)", 
       color = "Role", shape = "Family", alpha = "Sociality")+
  annotate("text", x = 0.57, y = 3.45, label = "C. aenea")+
  annotate("text", x = 0.05, y = 3, label = "E. smaragdina")+
  annotate("text", x = 0.655, y = 2.75, label = "B. morio")+
  annotate("text", x = 0.8, y = 6.57, label = "O. tataira")+
  annotate("text", x = 0.8, y = 4.35, label = "T. clavipes")+
  annotate("text", x = 0.78, y = 3.32, label = "M. marginata")+
  annotate("text", x = 0.81, y = 2.7, label = "T. hyalinata")+
  annotate("text", x = 0.5, y = -0.7, label = "A. mellifera")+
  
  scale_color_discrete(name = "Species' role", 
                      labels = c("Connectors",
                                 "Module Hubs",
                                 "Network Hubs",
                                 "Peripherals"))+
  scale_shape_manual(values = c(18,17))+
  scale_alpha_manual(values = c(0.4, 1))+
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 3),
         alpha = guide_legend(order = 2))

p
dev.off()
  

library(ggpubr)
tiff("figure_cz.tiff", units="in", width=10, height=10, res=300)
ggarrange(t,p, labels = c("A", "B"),
          ncol = 1, nrow = 2, common.legend = F, vjust = 1)
dev.off()



annotate("text", x = 0.72, y = 6, label = "Network hubs", size = 6)+
  annotate("text", x = 0.05, y = 6, label = "Module hubs", size = 6)+
  annotate("text", x = 0.71, y = -1.9, label = "Connectors", size = 6)+
  annotate("text", x = 0.045, y = -1.9, label = "Peripherals", size = 6)+