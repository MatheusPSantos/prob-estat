#fixar o diretorio de trabalho
setwd("Primeira Prova/Exercicio 1/");
#ler o conjunto de dados
dados <- read.csv(file="cancer.csv", sep=";")
dados[dados$Tratamento]
# tempo médio de vida em dias separado pelo tratamento
mediaTempo <- tapply(dados$Tempo, dados$Tratamento, mean)
# mediana do tempo de vida separado pelo tratamento
medianaTempo <- tapply(dados$Tempo, dados$Tratamento, median)
# ampitude dos valores da variável tempo
amplitude <- tapply(dados$Tempo, dados$Tratamento, range)
  #diferença entre os extremos da variavel para cada tipo de tratamento
diff(amplitude$A)
diff(amplitude$B)
# desvio padrao da variavel tempo separada pelo tratamento
desvioTempo <- tapply(dados$Tempo, dados$Tratamento, sd)

# intervalo interquartil da variavel tempo em função do tipo de tratamento
intervaloIQ <- tapply(dados$Tempo, dados$Tratamento, IQR)
# Boxplot da variavel tempo e Tratamento
  #primeiro vem a variavel no eixo y depois a variavel no eixo X
boxplot(dados$Tempo~dados$Tratamento, 
        ylab = "Tempo de aobrevivência (dias)", 
        xlab="Tratamento", namoes = c("A","B"))
#

