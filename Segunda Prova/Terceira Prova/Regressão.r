#########################################################################################
# Analise de Regressao
#########################################################################################
setwd("E:\\Programas") #Setar o diretorio
#########################################################################################
# Correlacao Linear Simples
#########################################################################################

correlacao <- read.csv("dados.csv",sep=";")

plot(correlacao$IdadeG,correlacao$Circunferencia)

cor.test(correlacao$IdadeG,correlacao$Circunferencia,method="pearson",conf.level=0.95)

modelo <- lm(correlacao$Circunferencia~correlacao$IdadeG) #Modelo Linear Simples
summary(modelo)

######################################
# Verificando Normalidade dos Residos
######################################
residos <- modelo$res #Retorna os residos do modelo

qqnorm(residos) #QQ-plot Normal
qqline(residos,col="red") #Coloca a linha teorica no QQ-plot

shapiro.test(residos) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Residos
#############################################
preditos <- predict(modelo) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Grafico dos Residos vs Preditos

#install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(modelo) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(modelo) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independencia dos Residos
#########################################
n <- nrow(correlacao)

plot(1:n,residos,type="b") #Grafico da ordem vs residos

dwtest(modelo) #Teste de independencia de Durbin-Watson
