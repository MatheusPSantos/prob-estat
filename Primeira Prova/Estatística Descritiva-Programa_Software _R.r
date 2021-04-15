setwd("H:\\Programas")

dados <- read.csv("dados.csv",sep=";")
#######################################################################
# Media
#######################################################################

mean(dados$IdadeG)
media <- tapply(dados$IdadeG,dados$Toxemia,mean)

#######################################################################
# Mediana
#######################################################################

median(dados$IdadeG)
mediana <- tapply(dados$IdadeG,dados$Toxemia,median)

#######################################################################
# Amplitude
#######################################################################

diff(range(dados$IdadeG))

amplitude <- tapply(dados$IdadeG,dados$Toxemia,range)
diff(amplitude$N?o)
diff(amplitude$Sim)

#######################################################################
# Desvio Padrao
#######################################################################

sd(dados$IdadeG)
desvio <- tapply(dados$IdadeG,dados$Toxemia,sd)

#######################################################################
# Intervalo Interquartil
#######################################################################

IQR(dados$IdadeG)

tapply(dados$IdadeG,dados$Toxemia,IQR)

#######################################################################
# Tabela de Frequencias (absoluta e relativa)
#######################################################################

f <- table(dados$Sexo)
round(f/length(dados$Sexo),4)

# Tabela Cruzada

f.absolutas <- table(dados$Toxemia,dados$Esteroides)
f.relativas <- f.absolutas/c(sum(f.absolutas[1,]),sum(f.absolutas[2,]))
round(f.relativas,4)

#######################################################################
# Grafico de Dispersao
#######################################################################

jpeg("figura1.jpg")
plot(dados$IdadeG,dados$Circunferencia,xlab="Idade Gestacional (semanas)",ylab="Circunferencia Cefalica (cm)")
graphics.off()

cor(dados$IdadeG,dados$Circunferencia,method="pearson")
cor(dados$IdadeG,dados$Circunferencia,method="spearman")
cor(dados$IdadeG,dados$Circunferencia,method="kendall")

#######################################################################
# Boxplot
#######################################################################

boxplot(dados$IdadeG,ylab="Idade Gestacional (semanas)")

boxplot(dados$IdadeG~dados$Toxemia,ylab="Idade Gestacional",xlab="Toxemia",names=c("Sem","Com"))

#######################################################################
# Histograma
#######################################################################

hist(dados$IdadeG,xlab="Idade Gestacional",ylab="Frequencia Absoluta",col="gray",main="Histograma para a variavel idade gestacional")

hist(dados$IdadeG,freq=F,xlab="Idade Gestacional",ylab="Frequencia Relativa",col="gray",main="")

#######################################################################
# Diagrama de Colunas
#######################################################################

barplot(table(dados$Sexo),ylab="Frequencia Absoluta",xlab="Sexo")

barplot(table(dados$Sexo)/nrow(dados),ylab="Frequencia Relativa",xlab="Sexo",col=c("pink","blue"))

# Grafico de barras Cruzados

jpeg("bar.jpg")
f.absolutas <- table(dados$Toxemia,dados$Esteroides)
f.relativas <- f.absolutas/c(sum(f.absolutas[1,]),sum(f.absolutas[2,]))
barplot(f.relativas,beside=T,col=c("blue","red"),xlab="Esteroides")
legend("topright",fill=c("blue","red"),legend=c("Toxemia (Nao)","Toxemia (Sim)"),bty="n")
graphics.off()

#######################################################################
# Grafico de Setores
#######################################################################

pie(table(dados$Sexo),col=c("pink2","skyblue"))
 
