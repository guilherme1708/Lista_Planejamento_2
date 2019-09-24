# Lista 2 - Planejamento II

library(car)
library(nortest)
library(ExpDes.pt)
library(nlme)
library(multcomp)

# Exercício 1

# item a

pesos <- c(1.28, 1.08, 1.06, 1.36, 1.19,
           1.45, 1.15, 1.28, 1.50, 1.41,
           1.38, 1.08, 1.17, 1.43, 1.26)
raca <- c(rep(c("Norfolk","AngoráI","AngoráII","NovaZelândiaI","NovaZelândiaII"),3))
dieta <- c(rep(c("Padrão"),5),rep(c("PadrãoRami"),5),rep(c("PadrãoAlfafa"),5))

carcacas <- data.frame(pesos,raca,dieta)
attach(carcacas)
raca <- factor(raca)
dieta <- factor(dieta)
interaction.plot(raca,dieta,pesos, main="Efeito de Interação dos Fatores")

# item b

fit.model1 <- lm(pesos~dieta+raca)
summary(fit.model1)
round(anova(fit.model1),4)

# Diagnóstico----------------------------------------------------#

X <- model.matrix(fit.model1)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model1)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model1)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }
#
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
#
par(mfrow=c(2,2))
qqnorm(tsi,xlab="Percentil da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16, main="")
par(new=T)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=T)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
#identify(di, n=2)
#
plot(tsi,xlab="Índice", ylab="Resíduo Studentizado",
     ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#
plot(fitted(fit.model1),tsi,xlab="Valor Ajustado", 
     ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1)
par(mfrow=c(1,1))

# Teste para igualdade das variâncias--------------------------------------------#
leveneTest(tsi,dieta)

leveneTest(tsi,raca)

leveneTest(tsi,pesos)

# Testes de normalidade---------------------------------------------------------#

shapiro.test(tsi)

ad.test(tsi)

# item d

fit.model2 <- aov(pesos~dieta+raca)
fit1.tk <- TukeyHSD(fit.model2, "dieta")
plot(fit1.tk)

detach(carcacas)

# Exercício 2

# item a

avaliacao <- c(11 ,13 ,10 ,18 ,15,
               20 ,28 ,15 ,30 ,18,
               8 ,10 ,8 ,16 ,12,
               30 ,35 ,27 ,41 ,28,
               14 ,16 ,13 ,22 ,16,
               25 ,27 ,26 ,33 ,25,
               43 ,46 ,41 ,55 ,42,
               13 ,14 ,12 ,20 ,13)
sinal <- c(rep(1:5,8))
local <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(7,5),rep(8,5))
sinal <- factor(sinal)
local <- factor(local)

estradas <- data.frame(avaliacao,sinal,local)
attach(estradas)
interaction.plot(local,sinal,avaliacao, main="Efeito de Interação dos Fatores")

# item b
fit.model2 <- lme(avaliacao~sinal, data = estradas, random= ~ 1 | local)

summary(fit.model2)

# item c

round(anova(fit.model2),4)

# item e

IC <-intervals(fit.model2)$reStruct$local^2
rownames(IC) <- "IC of Variance"
round(IC,3)

# item f

confint(glht(fit.model2, linfct=mcp(sinal="Tukey")))$confint

# item g

confint(glht(fit.model2, linfct=mcp(sinal="Dunnet")))$confint

# item h


detach(estradas)
# Exercício 4

# item c

Peso <- c(93,93.5,115.4,115.8,116.9,116.3,110.2,110.5,110.4,110.6,
          110.6,110.9,96.5,96.7,108.9,109.0,97.6,98.1,112.0,110.8,
          102.1,101.8,108.6,108.0,77.9,78.5,102.0,102.5,111.7,111.0,
          115.4,114.9,94.9,95.3,114.0,114.7,100.2,99.9,118.5,118.0,
          117.6,118.3,114.1,113.8,118.7,119.2,108.8,108.1,80.2,79.7)

Leitegada <- c(rep(1,10),rep(2,10),rep(3,10),rep(4,10),rep(5,10))

Peso_inicial <- c(rep(c(rep(1,2),rep(2,2),rep(3,2),rep(4,2),rep(5,2)),5))

Trat <- c(rep(c("A","C","E","D","B",
                "C","E","B","A","D",
                "B","D","A","E","C",
                "D","A","C","B","E",
                "E","B","D","C","A"),2))

Castracao <- data.frame(Peso,Leitegada,Peso_inicial,Trat)
attach(Castracao)
Trat <- factor(Castracao$Trat)

fit.model<- lm(Peso~Peso_inicial+Leitegada+Trat)
summary(fit.model)
round(anova(fit.model),4)

# item d

X <- model.matrix(fit.model)
n <- nrow(X)
p <- ncol(X)
H <- X%*%solve(t(X)%*%X)%*%t(X)
h <- diag(H)
lms <- summary(fit.model)
s <- lms$sigma
r <- resid(lms)
ts <- r/(s*sqrt(1-h))
di <- (1/p)*(h/(1-h))*(ts^2)
si <- lm.influence(fit.model)$sigma
tsi <- r/(si*sqrt(1-h))
a <- max(tsi)
b <- min(tsi)
ident <- diag(n)
epsilon <- matrix(0,n,100)
e <- matrix(0,n,100)
e1 <- numeric(n)
e2 <- numeric(n)
#
for(i in 1:100){
  epsilon[,i] <- rnorm(n,0,1)
  e[,i] <- (ident - H)%*%epsilon[,i]
  u <- diag(ident - H)
  e[,i] <- e[,i]/sqrt(u)
  e[,i] <- sort(e[,i]) }
#
for(i in 1:n){
  eo <- sort(e[i,])
  e1[i] <- (eo[2]+eo[3])/2
  e2[i] <- (eo[97]+eo[98])/2 }
#
med <- apply(e,1,mean)
faixa <- range(tsi,e1,e2)
#
par(mfrow=c(2,2))
qqnorm(tsi,xlab="Percentil da N(0,1)",
       ylab="Residuo Studentizado", ylim=faixa, pch=16, main="")
par(new=T)
qqnorm(e1,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=1, main="")
par(new=T)
qqnorm(e2,axes=F,xlab="",ylab="", type="l",ylim=faixa,lty=1, main="")
par(new=T)
qqnorm(med,axes=F,xlab="",ylab="",type="l",ylim=faixa,lty=2, main="")
#------------------------------------------------------------#
plot(di,xlab="Índice", ylab="Distância de Cook", pch=16)
#identify(di, n=2)
#
plot(tsi,xlab="Índice", ylab="Resíduo Studentizado",
     ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(tsi, n=1)
#
plot(fitted(fit.model),tsi,xlab="Valor Ajustado", 
     ylab="Resíduo Studentizado", ylim=c(b-1,a+1), pch=16)
abline(2,0,lty=2)
abline(-2,0,lty=2)
#identify(fitted(fit.model),tsi, n=1)
par(mfrow=c(1,1))

# Teste para igualdade das variâncias--------------------------------------------#

leveneTest(tsi,Peso_inicial)

leveneTest(tsi,Leitegada)

leveneTest(tsi,Trat)

# Testes de normalidade---------------------------------------------------------#

shapiro.test(tsi)

ad.test(tsi)

# comparações multiplas

dql(Trat, Peso_inicial, Leitegada, Peso, quali = TRUE, mcomp = "ccf", sigT = 0.10, sigF = 0.05)

detach(Castracao)