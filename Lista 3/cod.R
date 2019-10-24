# Lista 3 Planejamento

library(lattice) 
library(car)
library(nlme)
library(lmerTest)
library(agricolae)
library(multcomp)

# Exercício 1
dados <- read.csv("dados.csv")
dados$Bloco <- as.factor(dados$Bloco)
dados$Formulacao <- as.factor(dados$Formulacao)
dados$Pia <- as.factor(dados$Pia)

attach(dados)

# analise descritiva
boxplot(Y~Formulacao,main = "Formulação x Reposta",xlab = "Formulação",data = dados)

with(dados, xyplot(Y ~ Formulacao | Pia,main="Disperção fomulação por pia",xlab="Formulação"))

tapply(Y,Formulacao,mean)
tapply(Y,Formulacao,sd)
tapply(Y,Formulacao,median)

model <- lm(Y~Bloco+Formulacao+Pia)

round(Anova(model,type="III"),3)

BIBSlide.aov <- aov(Y ~ Formulacao + Error(Bloco/Pia))
summary(BIBSlide.aov)

BIB.test(Bloco, Formulacao, Y, test = c("tukey"),
         alpha = 0.05, group = TRUE,console=T)
detach(dados)

# Exercício 3

# item b
Dados2 <- read.csv("dados2.csv")
Dados2$Temperatura <- as.factor(Dados2$Temperatura)
Dados2$Tempo <- as.factor(Dados2$Tempo)
Dados2$Rep <- as.factor(Dados2$Rep)

attach(Dados2)

boxplot(Y~Temperatura, main="Temperatura x Força de Atração",ylab = "Força de Atração",data = Dados2)

with(Dados2, xyplot(Y ~ Temperatura | Tempo,main="Disperção por unidades de tempo"))

with(Dados2, xyplot(Y ~ Temperatura | Tempo, groups = Rep, aspect = "xy", type = "o",main="Interação entre temperatura e força de atração"))

tapply(Y,Temperatura,mean)
tapply(Y,Temperatura,sd)
tapply(Y,Temperatura,median)

tapply(Y,Tempo,mean)
tapply(Y,Tempo,sd)
tapply(Y,Tempo,median)

tapply(Y,Rep,mean)
tapply(Y,Rep,sd)
tapply(Y,Rep,median)

# item d

fit <- lme(Y ~ Temperatura*Tempo, random = ~1| Rep, data = Dados2)

plot(fit, main="Resíduos x Preditos",ylab="Resíduos Studentizados",xlab="Valores preditos")

qqPlot(residuals(fit),envelope=.95,xlab="Quantis Normais", ylab = "Resíduos de Pearson", main="Evelope simulado com 95% de confiança")

# item e
summary(fit)
round(anova(fit),3)

# item f

# comparações multiplas
confint(glht(fit, linfct=mcp(Temperatura="Tukey",interaction_average = T)))$confint

contrast.matrix <- rbind("Temperatura1600:Tempo20 - Temperatura1600:Tempo30" =  c(0, 0, 0,  0, -1, 1))

confint(glht(fit, contrast.matrix))$confint

detach(Dados2)
