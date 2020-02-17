library(readxl)
library(ggplot2)
library(dplyr)
library(lattice) 
library(nlme)
library(car)
library(reshape2)
library(ggmosaic)
library(gridExtra)
library(RColorBrewer)

source('residdiag_nlme.R') 

data1 <- read_excel("dados_sorvete.xlsx",sheet = 1)
data2 <- read_excel("dados_sorvete.xlsx",sheet = 2)

# Preparação dos dados

# Nomeação da receitas
data1$Receita1 <- data1$Receita
data1$Receita[data1$Receita == "1"] <- "Morango"
data1$Receita[data1$Receita == "2"] <- "Chocolate"
data1$Receita[data1$Receita == "3"] <- "Açai"

data1$Receita <- as.factor(data1$Receita)
data1$Receita1 <- as.factor(data1$Receita1)
data1$Tempo <- as.factor(data1$Tempo)
data1$Juiz <- as.factor(data1$Juiz)
colnames(data1)[10] <- "Nota_geral"

# para os dados antes e depois (planilha 2)

# correção erro de digitação
data2$Juiz[31] <- 11

# Nomeação da receitas
data2$Receita[data2$Receita == "1"] <- "Morango"
data2$Receita[data2$Receita == "2"] <- "Chocolate"
data2$Receita[data2$Receita == "3"] <- "Açai"

data2$Juiz <- as.factor(data2$Juiz)
data2$Receita <- as.factor(data2$Receita)
data2$Ranking_antes <- as.factor(data2$Ranking_antes)
data2$Ranking_depois <- as.factor(data2$Ranking_depois)

attach(data1)
# Analise descritiva

# Histograma nota geral
data1 %>%
  ggplot(aes(x=Nota_geral)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Nota Geral") +
  ylab("Frequência") 

# histograma cor
data1 %>%
  ggplot(aes(x=Cor)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Cor") +
  ylab("Frequência") 

# histograma predominacia
data1 %>%
  ggplot(aes(x=Predominancia)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Predominância") +
  ylab("Frequência") 

# histograma maciez
data1 %>%
  ggplot(aes(x=Maciez)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Maciez") +
  ylab("Frequência") 

# histograma aroma
data1 %>%
  ggplot(aes(x=Aroma)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Aroma") +
  ylab("Frequência") 

# histograma amargor
data1 %>%
  ggplot(aes(x=Amargor)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Amargor") +
  ylab("Frequência") 

# histograma doçura
data1 %>%
  ggplot(aes(x=Doçura)) +
  geom_histogram(fill="blue", alpha=0.5, position="identity",color="Black",bins=8) +
  xlab("Doçura") +
  ylab("Frequência") 

# diagrama de disperção Nota geral x cor
data1 %>%
  ggplot(aes(y=Nota_geral,x=Cor,colour=Receita)) +
  geom_point(aes(shape=Receita, color=Receita)) +
  scale_color_manual(values=c('black','blue','red')) +
  labs(x="Cor",
       y="Nota Geral",
       colour="Receitas",
       shape="Receitas")

# diagrama de disperção Nota geral x Predominacia
data1 %>%
  ggplot(aes(y=Nota_geral,x=Predominancia,colour=Receita)) +
  geom_point(aes(shape=Receita, color=Receita)) +
  scale_color_manual(values=c('black','blue','red')) +
  labs(x="Predominancia",
       y="Nota Geral",
       colour="Receitas",
       shape="Receitas")

# diagrama de disperção Nota geral x maciez
data1 %>%
  ggplot(aes(y=Nota_geral,x=Maciez,colour=Receita)) +
  geom_point(aes(shape=Receita, color=Receita)) +
  scale_color_manual(values=c('black','blue','red')) +
  labs(x="Maciez",
       y="Nota Geral",
       colour="Receitas",
       shape="Receitas")

# diagrama de disperção Nota geral x aroma
data1 %>%
  ggplot(aes(y=Nota_geral,x=Aroma,colour=Receita)) +
  geom_point(aes(shape=Receita, color=Receita)) +
  scale_color_manual(values=c('black','blue','red')) +
  labs(x="Aroma",
       y="Nota Geral",
       colour="Receitas",
       shape="Receitas")

# diagrama de disperção Nota geral x amargor
data1 %>%
  ggplot(aes(y=Nota_geral,x=Amargor,colour=Receita)) +
  geom_point(aes(shape=Receita, color=Receita)) +
  scale_color_manual(values=c('black','blue','red')) +
  labs(x="Amargor",
       y="Nota Geral",
       colour="Receitas",
       shape="Receitas")

# diagrama de disperção Nota geral x douçura
data1 %>%
  ggplot(aes(y=Nota_geral,x=Doçura,colour=Receita)) +
  geom_point(aes(shape=Receita, color=Receita)) +
  scale_color_manual(values=c('black','blue','red')) +
  labs(x="Doçura",
       y="Nota Geral",
       colour="Receitas",
       shape="Receitas")

# Descritiva dos dados de preferencia (Data2)

tab <- table(data2$Ranking_antes,data2$Ranking_depois)
Total_linha<-margin.table(tab,2)  
Total_coluna<-margin.table(tab,1) 
tab.cont<-rbind(cbind(tab,Total_coluna),c(Total_linha, sum(Total_coluna)))
dimnames(tab.cont)[[1]][4]<-"Total_linha" 
tab.cont

# mosaico preferencia antes e depois
data2 %>%  
  ggplot() +
  scale_fill_manual(values=c('purple','saddlebrown','red')) +
  geom_mosaic(aes(x = product(Ranking_depois,Ranking_antes), fill=Receita), color="Black") +
  labs(y = "Percent") +
  labs(y="Receita Antes",
       x="Receita Depois",
       fill="Receitas")

# barras preferencia antes
data3 <- data.frame(table(data2$Receita,data2$Ranking_antes))

plot1 <- data3 %>%  
  ggplot(aes(Var1,Freq)) +
  geom_bar(stat = "identity", aes(fill = Var2), position = "dodge",show.legend = F,color="Black") +
  scale_y_continuous(limits=c(0,9)) +
  scale_fill_manual(values=brewer.pal(n=3,"Blues")) +
  labs(x="Antes",
       y="Frequência",
       fill="Receita")

# barras preferencia depois
data4 <- data.frame(table(data2$Receita,data2$Ranking_depois))

plot2 <- data4 %>%  
  ggplot(aes(Var1,Freq)) +
  geom_bar(stat = "identity", aes(fill = Var2), position = "dodge", color="Black") +
  scale_fill_manual(values=brewer.pal(n=3,"Blues")) +
  scale_y_continuous(limits=c(0,9)) +
  theme(legend.position = c(-0.45, 0.85)) + 
  labs(x="Depois",
       y="Frequência",
       fill="Preferência")

grid.arrange(plot1, plot2, ncol=2)

# selecionando apenas nota geral como varirável resposta

dados <- data1[,c(1,2,3,10,11)]

# Boxplot nota geral por receita e tempo de congelamento
dados %>%
  ggplot(aes(y=Nota_geral,x=Tempo,fill=Receita)) +
  geom_boxplot(color="Black") +
  scale_fill_manual(values=c('purple','saddlebrown','red')) +
  labs(y="Nota Geral", 
       x="Tempo de congelamento") 

# Boxplot nota geral por Juiz
dados %>%
  ggplot(aes(y=Nota_geral,x=Juiz)) +
  geom_boxplot( color="Black") +
  labs(y="Nota Geral", 
       x="Juízes") 

# interação  tempo de congelamento x receita
dados %>%
  ggplot(aes(x = factor(Tempo) , y=Nota_geral , group = Receita, color = Receita)) + 
  scale_y_discrete(limits=c(6,6.5,7,7.5,8,8.5,9)) +
  scale_color_manual(values=c('purple','saddlebrown','red')) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  xlab("Tempo de Congelamento") +
  ylab(" Média da Nota Geral")

# interação Juiz x receita
dados %>%
  ggplot(aes(x = factor(Juiz) , y=Nota_geral , group = Receita, color = Receita)) + 
  scale_y_discrete(limits=c(4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10)) +
  scale_color_manual(values=c('purple','saddlebrown','red')) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  xlab("Juiz") +
  ylab(" Média da Nota Geral")

# interação Juiz x tempo de congelamento
dados %>%
  ggplot(aes(x = factor(Juiz) , y=Nota_geral , group = Tempo, color = Tempo)) + 
  scale_y_discrete(limits=c(4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10)) +
  scale_color_manual(values=c('red','blue')) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(fun.y=mean, geom="line") +
  xlab("Juiz") +
  ylab(" Média da Nota Geral")

# notas por juiz
dados %>%
  with(xyplot(Nota_geral ~ factor(Tempo:Receita1) | Juiz, ylab = "Nota Geral",
              xlab = "Tempo:Receita"))

# Médias de notas
tapply(Nota_geral,Juiz,mean)
tapply(Nota_geral,Receita,mean)
tapply(Nota_geral,Tempo,mean)

# desvios padrões das notas
tapply(Nota_geral,Juiz,sd)
tapply(Nota_geral,Receita,sd)
tapply(Nota_geral,Tempo,sd)

# teste de normalidade da variável reposta
shapiro.test(Nota_geral)

# teste de homecedasticidade
leveneTest(Nota_geral,Juiz)
leveneTest(Nota_geral,Receita) 
leveneTest(Nota_geral,Tempo)


# Modelo
fit <- lme(Nota_geral ~ Receita*Tempo, random = ~1| Juiz, data=dados)
summary(fit)
anova(fit)

residdiag.nlme(fit, limit=2,plotid=6)
residdiag.nlme(fit, limit=2,plotid=4)
residdiag.nlme(fit, limit=2,plotid=5)
residdiag.nlme(fit, limit=2,plotid=7)

detach(data1)