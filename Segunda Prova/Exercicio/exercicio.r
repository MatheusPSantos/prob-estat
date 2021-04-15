dados <-read.csv("probabilidade.csv", sep=";", dec=",");
tempo <- dados$Tempo;
temperatura <- dados$Temperatura;
marca <- dados$Marca;
############## Globalmente ############
# para o tempo
#########################
#distribuição exponencial
#########################
mediaTempo <- mean(tempo)
x <-seq(0, 12000, by=1)

fteorico <- dexp(x, rate =1/ mediaTempo)

hist(tempo, freq = F, xlab="Tempo de duração (em dias)", ylab="Frequência relativa", main="")
lines(x, fteorico, col="blue")

#####################
# distribuição normal
#####################
desvioTempo <- sd(tempo)
x <-seq(min(tempo), max(tempo), by = 0.1)
fteorico <- dnorm(x, mean = mediaTempo, sd = desvioTempo)

hist(tempo, freq = F, xlab="Tempo (em dias)", ylab = "Frequencia relativa", main="")
lines(x, fteorico, col = "blue")

qqnorm(tempo)
qqnorm(tempo, col="red")
#####################

# para a temperatura
##########################
# Distribuição exponencial
##########################
mediaTemperatura <- mean(temperatura)
y <-seq(0,12000, by=1)

fteoricoTemperatura <- dexp(y, rate = 1/ mediaTemperatura)

hist(temperatura, freq = F, xlab = "Temperatura (em Cº)", ylab="Frequência relativa", main="")
lines(y, fteoricoTemperatura, col="blue")

######################
# Distribuição normal
######################
