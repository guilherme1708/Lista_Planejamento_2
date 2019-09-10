# Lista 1 de planejamento II
setwd("/home/gui/Área de Trabalho/Lista de plan2")

library(car)
library(nortest)
library(onewaytests)
source("games.R")

# Exercício 1

# item a

dados <- read.csv("reab_Plan2", sep=";")
dados <- data.frame(dados$Dias,as.factor(dados$Media))
names(dados) <- c("Dias", "Medias")

fit.model<- lm(Dias~Medias, data=dados)
t.anova <- Anova(fit.model,type="III")

knitr::kable(caption = "Tabela de Anova", t.anova)

# item b

tapply(dados$Dias,dados$Medias,mean)

# item c

fit1.tk <- TukeyHSD(aov(fit.model), "Medias",conf.level = 0.99)
plot(fit1.tk)

# Exercicio 2

# item c

dt <- read.csv("coracao.csv",sep=";",dec=',')
dt$Grupo <- as.factor(dt$Grupo)
attach(dt)

welch.test(Coracao~Grupo,data=dt,alpha = 0.1)

tb <- games.howell(Grupo,Coracao)

#oneway(Grupo, y = Coracao, posthoc = 'games-howell')

detach(dt)
# Exercicio 3

# item a

dadosm <-read.csv('Mamoes.csv',sep=";",dec=",")
dadosm$tempo <- as.factor(dadosm$tempo)
dadosm$irradiacao <- as.factor(dadosm$irradiacao)
dadosm$tratamento <- as.factor(dadosm$tratamento)

attach(dadosm)

m1 <- tapply(Acidez, irradiacao, mean)
m2 <- tapply(Acidez, tempo, mean)
m3 <- tapply(Acidez, tratamento, mean)

d1 <- tapply(Acidez, irradiacao, sd)
d2 <- tapply(Acidez, tempo, sd)
d3 <- tapply(Acidez, tratamento, sd)

me1 <- tapply(Acidez, irradiacao, median)
me2 <- tapply(Acidez, tempo, median)
me3 <- tapply(Acidez, tratamento, median)

par(mfrow=c(1,2))
boxplot(Acidez~tempo, main="Acidez x Tempo")
boxplot(Acidez~irradiacao, main="Acidez x Irreadiação")

par(mfrow=c(1,1))
boxplot(Acidez~tratamento, main="Acidez x Tratamento")

interaction.plot(tempo, irradiacao, Acidez,main="Interção tempo e irradiação")

# item b

fit.model1<- lm(Acidez~irradiacao*tempo)
Anova(fit.model1,type="III")

tb <- games.howell(tratamento,Acidez)

detach(dadosm)

# Exercicio 4

# item b

horm <- read.csv("horm.txt",sep=";")
attach(horm)
sexo <- factor(horm$sexo)
desenvolvimentoosseo <- factor(horm$desenvolvimentoosseo)
tratamento <- factor(horm$tratamento)

fit.modelC<- lm(crescimento~sexo*desenvolvimentoosseo)
Anova(fit.modelC,type="III")

fit.modelI<- lm(crescimento~desenvolvimentoosseo)
Anova(fit.modelI,type="III")

# Diagnostico----------------------------------------------#

#identify(fitted(fit.modelC),h,n=3)
X <- model.matrix(fit.modelC)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.modelC)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.modelC)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
#plot(h,xlab="Indice", ylab="Medida h", pch=16, ylim=c(0,1.0))
#identify(h, n=5)
#title(sub="(a)")
#
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16)
abline(h=3*mean(di),lty=3)
#identify(di, n=3)
#
plot(tsi,xlab="Indice", ylab="Residuo Padronizado",
     ylim=c(-5,5), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#
plot(fitted(fit.modelC),tsi,xlab="Valor Ajustado", 
     ylab="Residuo Padronizado", ylim=c(-5,5), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.modelC),tsi, n=1)
#par(mfrow=c(1,1))

leveneTest(ts,tratamento)

# Testes de normalidade

shapiro.test(ts)

ad.test(ts)

# item d

df <- summary(fit.modelI)$df[2]
alpha <- 0.05
n.comp <- 3
qt(1-alpha/(2*n.comp),df)

summary(fit.modelI)
vcov(fit.modelI)

detach(horm)

# Exercicio 5

# item b

df <- read.csv("milho1.txt",sep=";")
df$Fertilizante <- as.factor(df$Fertilizante)
df$FertAntesM <- (df$FertAntes-2.14)
df$Fert2M <- (df$FertAntesM*df$Fert2)
df$Fert3M <- (df$FertAntesM*df$Fert3)
color = c("red", "blue", "black")[df$Fertilizante]

modeloancova <- lm(FertDepois~Fertilizante+FertAntesM,data=df)

plot(df$FertAntes,df$FertDepois,col=color,pch=16)


# Diagnostico----------------------------------------------#

#identify(fitted(modeloancova),h,n=3)
X <- model.matrix(modeloancova)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(modeloancova)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(modeloancova)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
par(mfrow=c(2,2))
plot(h,xlab="Indice", ylab="Medida h", pch=16, ylim=c(0,1.0))
cut <- 3*p/n
abline(cut,0,lty=2)
#identify(h, n=5)
#title(sub="(a)")
#
plot(di,xlab="Indice", ylab="Distancia de Cook", pch=16)
abline(h=3*mean(di),lty=3)
#identify(di, n=3)
#
plot(tsi,xlab="Indice", ylab="Residuo Padronizado",
     ylim=c(-5,5), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#
plot(fitted(modeloancova),tsi,xlab="Valor Ajustado", 
     ylab="Residuo Padronizado", ylim=c(-5,5), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1)
par(mfrow=c(1,1))

envelope <- function(fit0){
  Influence <- lm.influence(fit0)
  t.M <- resid(fit0)/(Influence$sigma*sqrt(1-Influence$hat))
  n <- length(t.M)
  X <- model.matrix(fit0)
  H <- X%*%solve(t(X)%*%X)%*%t(X) 
  I <- diag(n)
  yy <- matrix(rnorm(n*100),n,100)
  A <- matrix(0,n,100)
  LI = LS = numeric(n)
  A <- (I - H)%*%yy/sqrt(diag(I - H))
  A <- apply(A,2,sort)
  B <- t(apply(A,1,sort))
  LI <- (B[,2]+B[,3])/2
  LS <- (B[,97]+B[,98])/2
  med <- apply(B,1,mean)
  aux <- range(t.M,LS,LI)
  #par(pty="s")
  qqnorm(t.M,xlab="Percentil da N(0,1)", ylab="Ressíduo Studentizado modificado", ylim=aux,pch=16, main="")
  par(new=TRUE)
  qqnorm(LI,axes=F,xlab="",ylab="",type="l",lty=1,ylim=aux)
  par(new=TRUE)
  qqnorm(LS,axes=F,xlab="",ylab="", type="l", lty=1,ylim=aux)
  par(new=TRUE)
  qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=aux,lty=2)
}

envelope(modeloancova)

summary(modeloancova)
Anova(modeloancova,type="III")
m.vcov.estparancova<- vcov(modeloancova)
m.vcov.estparancova

# item c

modres <- lm(FertDepois~FertAntesM,data=df)

model.matrix(modres)

# item d

modres.anova <- anova(modres)

modcom <- lm(FertDepois~Fert2+Fert3+FertAntesM,data=df)
modcom.anova <- anova(modcom)

SQP <- modres.anova$Sum[2]-modcom.anova$Sum[4]
SQP
GLP <- modres.anova$Df[2]-modcom.anova$Df[4]
GLP
QMP <-SQP/GLP
QMP
FcalcP <- QMP/modcom.anova$Mean[4]
FcalcP
pvalorP <- 1 - pf(QMP, GLP, modcom.anova$Df[4])
pvalorP

# item e



# item f

fit1 <- lm(FertDepois ~ Fert2 + Fert3 + FertAntesM + Fert2M + Fert3M, data=df)
summary(fit1)

fit1.anova <- anova(fit1)
fit1.anova

fit1.anova$Df
fit1.anova$Sum

SQParalel <- modcom.anova$Sum[4]-fit1.anova$Sum[6]
SQParalel
GLParalel <- modcom.anova$Df[4]-fit1.anova$Df[6]
GLParalel
QMParalel <-SQParalel/GLParalel
QMParalel
FcalcParalel <- QMParalel/fit1.anova$Mean[6]
FcalcParalel
pvalorParalel <- 1 - pf(FcalcParalel, GLParalel, fit1.anova$Df[6])
pvalorParalel

