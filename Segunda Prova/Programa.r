setwd("/home/matheus/Documentos/Universidade/3/Probabilidade/Segunda Prova/")

dados <- read.csv("Dados.csv",sep=";")
tempo <- c(1377,790,250,6890,150,11812,273,1140,1317,4713,1607,4193,2966,3570,1589,5089,
           5803,819,8439,5327,4034,1056,1416,10265,8318,1185,3409,2656,622,1149,9965,
           1908,6881,8911,2851,1017,5488,758,2358,3626,404,11269,7709,2142,6319,571,825,
           1200,1680,595)

#######################################################################################
# Distribuicao Exponencial
#######################################################################################

media <- mean(tempo)
x <- seq(0,12000,by=1)
fteorico <- dexp(x,rate=1/media)

hist(tempo,freq=F,xlab="Tempo (em dias)",ylab="Frequencia Relativa",main="")
lines(x,fteorico,col="red")

#######################################################################################
# Distribuicao Normal
#######################################################################################

idade <- dados$IdadeG
media <- mean(idade)
desvio <- sd(idade)
x <- seq(min(idade),max(idade),by=0.1)
fteorico <- dnorm(x,mean=media,sd=desvio)

hist(idade,freq=F,xlab="Idade Gestacional",ylab="Frequencia Relativa",main="")
lines(x,fteorico,col="red")

qqnorm(idade)
qqline(idade,col="red")

#######################################################################################
# Anaálise Circunferencia (Maes com toxemia)
#######################################################################################

ct <- dados[dados$Toxemia=="Sim",]

media1 <- mean(ct$Circunferencia)
desvio1 <- sd(ct$Circunferencia)
x1 <- seq(min(ct$Circunferencia),max(ct$Circunferencia),by=0.1)
fteoricoN1 <- dnorm(x1,mean=media1,sd=desvio1)

hist(ct$Circunferencia,freq=F,xlab="Circunferencia Cefalica",ylab="Frequencia Relativa",main="")
lines(x1,fteoricoN1,col="red")

qqnorm(ct$Circunferencia)
qqline(ct$Circunferencia,col="red")

#######################################################################################
# Analise Circunferencia (Maes sem toxemia)
#######################################################################################

st <- dados[dados$Toxemia=="Nao",]

media2 <- mean(st$Circunferencia)
desvio2 <- sd(st$Circunferencia)
x2 <- seq(min(st$Circunferencia),max(st$Circunferencia),by=0.1)
fteoricoN2 <- dnorm(x2,mean=media2,sd=desvio2)

hist(st$Circunferencia,freq=F,xlab="Circunferencia Cefalica",ylab="Frequencia Relativa",main="")
lines(x2,fteoricoN2,col="red")

qqnorm(st$Circunferencia)
qqline(st$Circunferencia,col="red") 

