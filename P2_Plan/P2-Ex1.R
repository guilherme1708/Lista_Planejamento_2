library(readr)
library(tidyr)
library(ggplot2)


dados <- read_csv("BV.csv", locale = locale(decimal_mark = ",",grouping_mark = "."))

attach(dados)
dados$Material <- as.factor(dados$Material)
dados$Grupo <- as.factor(dados$Grupo)
dados$Individuo <- as.factor(dados$Individuo)
dados$Trat <- as.factor(dados$Trat)

# Analise descritiva

dados %>%
  ggplot(aes(x=BV)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  labs(x="BV(mm3)",
       y="Frequencia",
       title="Histograma")

dados %>%
  ggplot(aes(y=BV,x=Grupo)) +
  geom_boxplot(color="Black") +
  geom_boxplot(fill = c("tan1","tan4")) +
  labs(x="Grupo",
       y="BV",
       title="Grupo")

dados %>%
  ggplot(aes(y=BV,x=Material)) +
  geom_boxplot(color="Black") +
  geom_boxplot(fill = c("gold1","gold2","gold3","gold4")) +
  labs(y="BV", 
       x="Material")

dados %>%
  ggplot(aes(x=Material, y=BV, fill=Grupo)) +
  geom_dotplot(binaxis='y', stackdir='center')+
  scale_fill_manual(values=c("black", "white"))


dados %>%
  ggplot(aes(x = Material , y=BV , group = Grupo, color = Grupo)) +
  scale_color_manual(values=c('tan1','tan4')) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  xlab("Material") +
  ylab("BV")

dados %>%
  ggplot(aes(x = Individuo , y=BV , group = Grupo, color = Grupo)) +
  scale_color_manual(values=c('tan1','tan4')) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  xlab("Individuo") +
  ylab("BV")

dados %>%
  ggplot(aes(x = Material , y=BV , group = factor(Trat),color = factor(Trat))) +
  scale_color_manual(values=c('tan1','tan4','tan1','tan4','tan1','tan4','tan1','tan4','tan1','tan4')) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  xlab("Material") +
  ylab("BV")

# Carrega pacotes necess?rios
require(nlme)
library(ez)

# Ajusta o modelo e obt?m estat?stica e n?vel descritivo do efeitos dos fatoresde interesse. 
Modelo1<- lme(BV ~ Grupo*Material, random = ~1| Trat, data=dados)
summary(Modelo1)
anova(Modelo1)

Modelo2<- lme(BV ~ Grupo+Material, random = ~1| Trat, data=dados)
summary(Modelo2)
anova(Modelo2)

source('residdiag_nlme.R')  

residdiag.nlme( Modelo2, limit=2, plotid=6)
residdiag.nlme( Modelo2, limit=2, plotid=4)
residdiag.nlme( Modelo2, limit=2, plotid=5)
residdiag.nlme( Modelo2, limit=2, plotid=7)

Modelo3 <- ezANOVA(data = dados, dv = BV, wid = Trat, within = .(Material), between = Grupo)
Modelo3

QMR2<-Modelo2$sig^2
QMR1 <- 0.3880708^2

mean.g <- tapply(BV,Grupo,mean)
mean.m <- tapply(BV,Material,mean)

C <- qtukey(0.95,2,27)/sqrt(2)
mean.g[1]-mean.g[2] - C*sqrt(2*QMR1)
mean.g[1]-mean.g[2] + C*sqrt(2*QMR1)




mean.m[1]-mean.m[2] - C*sqrt((2/(5*2))*QMR2)
mean.m[1]-mean.m[2] + C*sqrt((2/(5*2))*QMR2)


mean.m[1]-mean.m[3] - C*sqrt((2/(5*2))*QMR2)
mean.m[1]-mean.m[3] + C*sqrt((2/(5*2))*QMR2)

mean.m[1]-mean.m[4] - C*sqrt((2/(5*2))*QMR2)
mean.m[1]-mean.m[4] + C*sqrt((2/(5*2))*QMR2)

mean.m[2]-mean.m[3] - C*sqrt((2/(5*2))*QMR2)
mean.m[2]-mean.m[3] + C*sqrt((2/(5*2))*QMR2)

mean.m[2]-mean.m[4] - C*sqrt((2/(5*2))*QMR2)
mean.m[2]-mean.m[4] + C*sqrt((2/(5*2))*QMR2)

mean.m[3]-mean.m[4] - C*sqrt((2/(5*2))*QMR2)
mean.m[3]-mean.m[4] + C*sqrt((2/(5*2))*QMR2)
