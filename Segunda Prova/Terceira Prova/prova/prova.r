dados <- read.csv("lowbwt3.csv",sep=";")# setei os dados

# questão 1
pesoNormal <- dados[dados$LOW=="0",];  #peso normal da criança
pesoBaixo <- dados[dados$LOW=="1",];   #peso baixo da criança
# peso da mãe no último período menstrual
pesoMae <- dados[dados$LWT,];

qqnorm(pesoNormal$LWT) # Verificar se segue a distribui??o normal.
qqline(pesoNormal$LWT,col="red")
shapiro.test(pesoNormal$LWT)

