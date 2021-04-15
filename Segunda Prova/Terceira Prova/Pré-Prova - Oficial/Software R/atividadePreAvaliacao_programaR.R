dados <- read.csv("cancer.csv",sep=";")

################################################################################
################################ LETRA (A) #####################################
################################################################################

tratamentoPadrao <- dados[dados$Tratamento=="1",]
tratamentoTeste <- dados[dados$Tratamento=="2",]

qqnorm(tratamentoPadrao$Tempo) # Verificar se segue a distribui??o normal.
qqline(tratamentoPadrao$Tempo,col="red")
shapiro.test(tratamentoPadrao$Tempo)

qqnorm(tratamentoTeste$Tempo) # Verificar se segue a distribui??o normal.
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

TukeyHSD(ANOVA,conf.level=0.95) #P?s teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Res?dos
######################################

residos <- ANOVA$res #Retorna os res?dos do modelo

qqnorm(residos) #QQ-plot Normal
qqline(residos,col="red") #Coloca a linha te?rica no QQ-plot

shapiro.test(residos) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Res?duos
#############################################

preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Gr?fico dos Res?duos vc Preditos

install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independ?ncia dos Res?duos
#########################################

n <- nrow(dados)

plot(1:n,residos,type="b") #Gr?fico da ordem vs residuos

dwtest(ANOVA) #Teste de independ?ncia de Durbin-Watson

##############################################################################
# Teste N?o param?trico 
##############################################################################

#########################
# Teste de Kruskal Wallis
#########################

install.packages("agricolae")
library(agricolae)

K.teste <- kruskal(dados$Tempo,dados$Grau)
K.teste$statistics
K.teste$groups

# Pelo teste de Kruskal Wallis, o tempo n?o ? afetado pelo grau em que a c?lula 
# do tumor se assemelha a c?lula hospedeira, logo, a hip?tese ? verdadeira (u1 = u2 = u3). 


################################################################################
################################ LETRA (C) #####################################
################################################################################

#################
# Tabela de ANOVA
#################

ANOVA <- aov(dados$Tempo~dados$Local) #Faz a ANOVA
summary(ANOVA) #Tabela de ANOVA

TukeyHSD(ANOVA,conf.level=0.95) #P?s teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Res?dos
######################################

residos2 <- ANOVA$res #Retorna os res?dos do modelo

qqnorm(residos2) #QQ-plot Normal
qqline(residos2,col="red") #Coloca a linha te?rica no QQ-plot

shapiro.test(residos2) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Res?duos
#############################################

preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Gr?fico dos Res?duos vc Preditos

install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independ?ncia dos Res?duos
#########################################

n <- nrow(dados)

plot(1:n,residos2,type="b") #Gr?fico da ordem vs residuos

dwtest(ANOVA) #Teste de independ?ncia de Durbin-Watson

##############################################################################
# Teste N?o param?trico 
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

TukeyHSD(ANOVA,conf.level=0.95) #P?s teste de Tukey (Honest Significant Difference)

######################################
# Verificando Normalidade dos Res?dos
######################################

residos <- ANOVA$res #Retorna os res?dos do modelo

qqnorm(residos) #QQ-plot Normal
qqline(residos,col="red") #Coloca a linha te?rica no QQ-plot

shapiro.test(residos) #Teste de normalidade de Shapiro-Wilk

#############################################
# Verificando homoscedasticidade dos Res?duos
#############################################

preditos <- predict(ANOVA) #Retorna os valores preditos pelo modelo

plot(residos,preditos) #Gr?fico dos Res?duos vc Preditos

install.packages("lmtest")
library(lmtest) #Carrega a library lmtest

bptest(ANOVA) #Teste de homoscedasticidade de Breusch-Pagan
gqtest(ANOVA) #Teste de homoscedasticidade de Goldfeld-Quandt

#########################################
# Verificando independ?ncia dos Res?duos
#########################################

n <- nrow(dados)

plot(1:n,residos,type="b") #Gr?fico da ordem vs residuos

dwtest(ANOVA) #Teste de independ?ncia de Durbin-Watson

##############################################################################
# Teste N?o param?trico 
##############################################################################

#######################################
# Coeficiente de Correla??o de Kendall
#######################################

cor.test(dados$Tempo, dados$Idade, method="kendall")

################################################################################
################################ LETRA (E) #####################################
################################################################################

tabela <- table(dados$Grau,dados$Local)

chisq.test(tabela)
