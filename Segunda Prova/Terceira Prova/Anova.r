#########################################################################################
# Analise de Variancia (ANOVA)
#########################################################################################
setwd("H:\\Programas") #Setar o diretorio

dados <- read.csv("anova.csv",sep=";") # Material (Tipo de Material) / Perfil (Perfil do Material) / Medida (Durabilidade)

#################
# Tabela de ANOVA
#################

ANOVA <- aov(dados$Medida~dados$Perfil) #Faz a ANOVA
summary(ANOVA) #Tabela de ANOVA

TukeyHSD(ANOVA,conf.level=0.90) #Pos teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Residos
######################################
residos <- ANOVA$res #Retorna os residos do modelo

qqnorm(residos) #QQ-plot Normal
qqline(residos,col="red") #Coloca a linha teorica no QQ-plot

shapiro.test(residos) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Residuos
#############################################
preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Grafico dos Residuos vs Preditos

#install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independencia dos Residuos
#########################################
n <- nrow(dados)

plot(1:n,residos,type="b") #Grafico da ordem vs residuos

dwtest(ANOVA) #Teste de independencia de Durbin-Watson
