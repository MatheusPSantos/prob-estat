setwd("C:\\Users\\chrys\\OneDrive\\Documentos\\Estatística")

dados <- read.csv("cancer.csv",sep=";")

################################################################################
################################ LETRA (A) #####################################
################################################################################

tratamentoPadrao <- dados[dados$Tratamento=="1",]
tratamentoTeste <- dados[dados$Tratamento=="2",]

qqnorm(tratamentoPadrao$Tempo) # Verificar se segue a distribuição normal.
qqline(tratamentoPadrao$Tempo,col="red")
shapiro.test(tratamentoPadrao$Tempo)

qqnorm(tratamentoTeste$Tempo) # Verificar se segue a distribuição normal.
qqline(tratamentoTeste$Tempo,col="red")
shapiro.test(tratamentoTeste$Tempo)

var.test(tratamentoPadrao$Tempo,tratamentoTeste$Tempo,conf.level=0.95)

t.test(tratamentoPadrao$Tempo,tratamentoTeste$Tempo,var.equal=FALSE,conf.level=0.95)

#######################################################
# Teste de Wilcoxon-Mann-Whitney Amostras Independentes
#######################################################

wilcox.test(dados$Tempo~dados$Tratamento)

################################################################################
################################ LETRA (B) #####################################
################################################################################

#################
# Tabela de ANOVA
#################

ANOVA <- aov(dados$Tempo~dados$Grau) #Faz a ANOVA
summary(ANOVA) #Tabela de ANOVA

TukeyHSD(ANOVA,conf.level=0.95) #Pós teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Resídos
######################################

residos <- ANOVA$res #Retorna os resídos do modelo

qqnorm(residos) #QQ-plot Normal
qqline(residos,col="red") #Coloca a linha teórica no QQ-plot

shapiro.test(residos) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Resíduos
#############################################

preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Gráfico dos Resíduos vc Preditos

install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independência dos Resíduos
#########################################

n <- nrow(dados)

plot(1:n,residos,type="b") #Gráfico da ordem vs residuos

dwtest(ANOVA) #Teste de independência de Durbin-Watson

##############################################################################
# Teste Não paramétrico 
##############################################################################

#########################
# Teste de Kruskal Wallis
#########################

install.packages("agricolae")
library(agricolae)

K.teste <- kruskal(dados$Tempo,dados$Grau)
K.teste$statistics
K.teste$groups

# Pelo teste de Kruskal Wallis, o tempo não é afetado pelo grau em que a célula 
# do tumor se assemelha a célula hospedeira, logo, a hipótese é verdadeira (u1 = u2 = u3). 


################################################################################
################################ LETRA (C) #####################################
################################################################################

#################
# Tabela de ANOVA
#################

ANOVA <- aov(dados$Tempo~dados$Local) #Faz a ANOVA
summary(ANOVA) #Tabela de ANOVA

TukeyHSD(ANOVA,conf.level=0.95) #Pós teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Resídos
######################################

residos2 <- ANOVA$res #Retorna os resídos do modelo

qqnorm(residos2) #QQ-plot Normal
qqline(residos2,col="red") #Coloca a linha teórica no QQ-plot

shapiro.test(residos2) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Resíduos
#############################################

preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Gráfico dos Resíduos vc Preditos

install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independência dos Resíduos
#########################################

n <- nrow(dados)

plot(1:n,residos2,type="b") #Gráfico da ordem vs residuos

dwtest(ANOVA) #Teste de independência de Durbin-Watson

##############################################################################
# Teste Não paramétrico 
##############################################################################

#########################
# Teste de Kruskal Wallis
#########################

install.packages("agricolae")
library(agricolae)

K.teste <- kruskal(dados$Tempo,dados$Grau)
K.teste$statistics
K.teste$groups

################################################################################
################################ LETRA (D) #####################################
################################################################################

#################
# Tabela de ANOVA
#################

ANOVA <- aov(dados$Tempo~dados$Idade) #Faz a ANOVA
summary(ANOVA) #Tabela de ANOVA

TukeyHSD(ANOVA,conf.level=0.95) #Pós teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Resídos
######################################

residos <- ANOVA$res #Retorna os resídos do modelo

qqnorm(residos) #QQ-plot Normal
qqline(residos,col="red") #Coloca a linha teórica no QQ-plot

shapiro.test(residos) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Resíduos
#############################################

preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Gráfico dos Resíduos vc Preditos

install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independência dos Resíduos
#########################################

n <- nrow(dados)

plot(1:n,residos,type="b") #Gráfico da ordem vs residuos

dwtest(ANOVA) #Teste de independência de Durbin-Watson

##############################################################################
# Teste Não paramétrico 
##############################################################################

#######################################
# Coeficiente de Correlação de Kendall
#######################################

cor.test(dados$Tempo, dados$Idade, method="kendall")

################################################################################
################################ LETRA (E) #####################################
################################################################################

tabela <- table(dados$Grau,dados$Local)

chisq.test(tabela)
