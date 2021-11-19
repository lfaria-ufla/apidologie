#cabeçalho
rm(list=ls())
library(corrplot)
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
  dplyr::select(id, Role, sociality, familia, degree, betweenness, closeness, c, z, core, eig)%>%
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


names(data)
data$sociality = as.factor(data$sociality)
m1 = glm(as.factor(sociality) ~   eig+closeness+ c + degree + z  , 
         family = "binomial", data = data)
car::vif(m1, digits = 2)


summary(m1)

library(DHARMa)
testDispersion(m1)
res<-simulateResiduals(m1, plot = T)
as.numeric(summary(m1)["deviance"])/as.numeric(summary(m1)["df.residual"])
summary(m1)["dispersion"]

library(car)
Anova(m1, type="II", test="Wald")

library(MuMIn)
my_sel=dredge(m1)
my_sel_d2=get.models(my_sel, subset = delta < 2) #2 modelos
my_sel_d2
avg_model=model.avg(my_sel_d2)
avg_model
m_final=summary(avg_model) 
m_final
confint(avg_model,full = F)
importance(subset(model.sel(my_sel, rank = AICc),
                  subset = delta < 2))

##### grupo social
names(data)
data$sociality = as.factor(data$sociality)
m2 = glm(as.factor(sociality) ~    eig+ degree + c +  z +familia , 
         family = "binomial", data = dado)
car::vif(m2, digits = 2)


summary(m2)

library(DHARMa)
testDispersion(m2)
res<-simulateResiduals(m2, plot = T)
as.numeric(summary(m2)["deviance"])/as.numeric(summary(m2)["df.residual"])
summary(m2)["dispersion"]

library(car)
Anova(m2, type="II", test="Wald")

library(MuMIn)
my_sel=dredge(m2)
my_sel_d2=get.models(my_sel, subset = delta < 2) #2 modelos
my_sel_d2
avg_model=model.avg(my_sel_d2)
avg_model
m_final=summary(avg_model) 
m_final
 confint(avg_model,full = F)
importance(subset(model.sel(my_sel, rank = AICc),
                  subset = delta < 2))


##### grupo familia
names(data)
api$sociality = as.factor(api$sociality)
m2 = glm(as.factor(sociality) ~   eig+  c +  z , 
         family = "binomial", data = api)
car::vif(m2, digits = 2)


summary(m2)

library(DHARMa)
testDispersion(m2)
res<-simulateResiduals(m2, plot = T)
as.numeric(summary(m2)["deviance"])/as.numeric(summary(m2)["df.residual"])
summary(m2)["dispersion"]

library(car)
Anova(m2, type="II", test="Wald")

library(MuMIn)
my_sel=dredge(m2)
my_sel_d2=get.models(my_sel, subset = delta < 2) #2 modelos
my_sel_d2
avg_model=model.avg(my_sel_d2)
avg_model
m_final=summary(avg_model) 
m_final
confint(avg_model,full = F)
importance(subset(model.sel(my_sel, rank = AICc),
                  subset = delta < 2))


##### grupo familia vespidae
names(data)
ves$sociality = as.factor(ves$sociality)
m2 = glm(as.factor(sociality) ~  eig+ c + degree + z , 
         family = "binomial", data = ves)
car::vif(m2, digits = 2)


summary(m2)

library(DHARMa)
testDispersion(m2)
res<-simulateResiduals(m2, plot = T)
as.numeric(summary(m2)["deviance"])/as.numeric(summary(m2)["df.residual"])
summary(m2)["dispersion"]

library(car)
Anova(m2, type="II", test="Wald")

library(MuMIn)
my_sel=dredge(m2)
my_sel_d2=get.models(my_sel, subset = delta < 2) #2 modelos
my_sel_d2
avg_model=model.avg(my_sel_d2)
avg_model
m_final=summary(avg_model) 
m_final
confint(avg_model,full = F)
importance(subset(model.sel(my_sel, rank = AICc),
                  subset = delta < 2))
