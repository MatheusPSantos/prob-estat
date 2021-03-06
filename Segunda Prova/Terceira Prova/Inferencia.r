dados <- read.csv("dados.csv",sep=";")

#######################################################################################
# Funcoes
#######################################################################################

var.IC <- function(data, conf.level = 0.95) 
  {
          df = length(data) - 1
    chilower = qchisq((1 - conf.level)/2, df)
    chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
           v = var(data)
  
    c(df * v/chiupper, df * v/chilower)
  }

var.TH <- function(data,var0) 
  {
           df = length(data) - 1
            v = var(data)
      testchi = df*v/(var0)
         less = pchisq(testchi,df)
      greater = pchisq(testchi,df,lower.tail=F)
    two.sided = 2*min(less,greater)
   
    print(paste("P-Value: ",round(two.sided,4)),quote=FALSE)
  }

#######################################################################################
# Intervalo de Confianca Para a Media e Variancia Populacionais
#######################################################################################

# Idade Gestacional

idade <- dados$IdadeG
qqnorm(idade)
qqline(idade,col="red")

t.test(idade,conf.level=0.95)$conf.int

#install.packages("BSDA")
library(BSDA)
z.test(idade,sigma.x=sqrt(7),conf.level=0.95)$conf.int

var.IC(idade,conf.level=0.95)
sqrt(var.IC(idade,conf.level=0.95))
#######################################################################################
# Intervalo de Confianca Para a Proporcao Populacional
#######################################################################################

tabela <- table(dados$Esteroide)

prop.test(tabela[2],sum(tabela))$estimate
prop.test(tabela[2],sum(tabela),conf.level=0.95)$conf.int

#######################################################################################
# Teste de Hipoteses Para a Media Populacional
#######################################################################################

idade <- dados[dados$Toxemia=="Sim",]$IdadeG
qqnorm(idade)
qqline(idade,col="red")

t.test(idade,mu=39,conf.level=0.95)

library(BSDA)
z.test(idade,sigma.x=sqrt(7),mu=39,conf.level=0.90)

#######################################################################################
# Teste de Hipoteses Para a Variancia Populacional
#######################################################################################

circ <- dados$Circunferencia
qqnorm(circ)
qqline(circ,col="red")
shapiro.test(circ)

var.TH(circ,var0=6)
var.IC(circ,conf.level=0.95)

#######################################################################################
# Teste de Hipoteses Para a Proporcao Populacional
#######################################################################################

tabela <- table(dados$Esteroide)
prop.test(tabela[2],sum(tabela),p=0.25,conf.level=0.90)

#######################################################################################
# Comparacao Entre Duas Medias
#######################################################################################

# Teste-t Pareado (Comparando pressao sistolica em mm/Hg)

          Placebo <- c(211,210,210,203,196,190,191,177,173,170,163)
Hidroclorotiazida <- c(181,172,196,191,167,161,178,160,149,119,156)

qqnorm(Placebo)
qqline(Placebo,col="red")
shapiro.test(Placebo)

qqnorm(Hidroclorotiazida)
qqline(Hidroclorotiazida,col="red")
shapiro.test(Hidroclorotiazida)

t.test(Placebo,Hidroclorotiazida,paired=TRUE,conf.level=0.99)

# Teste-z (Comparando preco/m2)

RC <- c(41.2,40.5,39.6,39.4,38.9,39.1,40.9,41.2,40.4,40.6,40.3,39.2,40.6,39.7,40.3,40.9,
        39.6,39.7,40.0,41.2)*100
RO <- c(37.2,34.9,38.1,35.4,35.7,37.7,36.4,36.6,36.1,37.4,36.1,35.9,36.9,37.4,37.5,38.0,
        36.8,36.4)*100

qqnorm(RC)
qqline(RC,col="red")
shapiro.test(RC)

qqnorm(RO)
qqline(RO,col="red")
shapiro.test(RO)

DP.RC <- 71
DP.RO <- 82

library(BSDA)
z.test(RC,RO,sigma.x=DP.RC,sigma.y=DP.RO,conf.level=0.90)

# Teste-t var1 = var2 (Comparando conteudo mineral osseo em g/cm)

    fumaram <- c(0.0977,0.0690,0.0990,0.0930,0.0868,0.1298,0.1377,0.0940,0.1075,0.1214,
                 0.1207,0.0929,0.1138,0.0987,0.1218,0.0705,0.0755,0.1213,0.0826,0.1672,
                 0.0719,0.1205,0.1029,0.1476,0.0457,0.0441,0.0768,0.1312,0.1297,0.0787,
                 0.1249,0.0644,0.0896,0.0988,0.0976,0.1377,0.0920,0.0950,0.0837,0.0976,
                 0.0713,0.0879,0.0756,0.1218,0.1163,0.1204,0.0956,0.1368,0.1177,0.0854,
                 0.0948,0.1038,0.1271,0.1107,0.0591,0.1143,0.0683,0.1251,0.0926,0.0982,
                 0.0923,0.0867,0.1180,0.0881,0.0923,0.1327,0.0947,0.0843,0.0939,0.1030,
                 0.0803,0.1002,0.1613,0.1001,0.1039,0.1164,0.0436)

nao.fumaram <- c(0.0898,0.0809,0.0718,0.0958,0.1073,0.0577,0.0781,0.0630,0.1094,0.0587,0.0703,
                 0.0380,0.0554,0.1380,0.0762,0.0675,0.1314,0.1209,0.0911,0.0947,0.0791,0.0925,
                 0.0939,0.0630,0.1432,0.0966,0.1153,0.0709,0.1161,0.0659,0.0916,0.0863,0.0846,
                 0.0860,0.0990,0.1713,0.0727,0.0963,0.1061,0.0862,0.0866,0.0849,0.0811,0.1215,
                 0.0999,0.0690,0.0982,0.0805,0.0729,0.0984,0.0652,0.1025,0.0812,0.1090,0.0993,
                 0.0915,0.0947,0.1131,0.1256,0.0901,0.1065,0.0974,0.0921,0.0915,0.0520,0.0412,
                 0.0789,0.1310,0.1413,0.0954,0.1146,0.1327,0.0982,0.1465,0.1202,0.1038,0.0825,
                 0.1210,0.1152,0.1250,0.1243,0.1347,0.1311,0.1038,0.1009,0.0819,0.1247,0.0513,
                 0.1066,0.1095,0.1201,0.1061,0.0907,0.0815,0.0888,0.1075,0.1520,0.1091,0.0868,
                 0.0904,0.1068,0.1249,0.0790,0.0701,0.1017,0.0775,0.0417,0.0714,0.0951,0.0732,
                 0.0520,0.1155,0.1006,0.0624,0.1288,0.0698,0.0935,0.0791,0.1229,0.0794,0.1197,
                 0.1345,0.0212,0.1108,0.0703,0.0892,0.0643,0.1200,0.0821,0.0907,0.1199,0.1140,
                 0.1235,0.0825,0.1222,0.0660,0.0906,0.1040,0.1040,0.0986,0.1158,0.1128,0.1197,
                 0.0753,0.0914,0.0785,0.0868,0.0990,0.0935,0.1372,0.0849,0.0982,0.0924,0.1098,
                 0.1165,0.1182,0.0959,0.0772,0.0791,0.0405,0.0646)

qqnorm(fumaram)
qqline(fumaram,col="red")
shapiro.test(fumaram)

qqnorm(nao.fumaram)
qqline(nao.fumaram,col="red")
shapiro.test(nao.fumaram)

var.test(fumaram,nao.fumaram,conf.level=0.95)

t.test(fumaram,nao.fumaram,var.equal=TRUE,conf.level=0.95)

# Teste-t var1 dif. var2 (Comparando nivel serico de ferro em umol/l)

fibrose.cistica <- c(13.78,18.01,19.65,19.66,12.64,11.53,13.44,18.31,15.4,14.92,10.06,14.62,13.97)
      saudaveis <- c(26.04,30.42,11.53,29.08,19.82,16.01,20.39,21.46,6.95,20.5,30.2)

qqnorm(fibrose.cistica)
qqline(fibrose.cistica)
shapiro.test(fibrose.cistica)

qqnorm(saudaveis)
qqline(saudaveis)
shapiro.test(saudaveis)

var.test(saudaveis,fibrose.cistica,conf.level=0.95)
t.test(saudaveis,fibrose.cistica,var.equal=FALSE,conf.level=0.95)

#######################################################################################
# Analise de Dados Reais
#######################################################################################

ct <- dados[dados$Toxemia=="Sim",]
st <- dados[dados$Toxemia=="Nao",]

qqnorm(ct$IdadeG)
qqline(ct$IdadeG,col="red")
shapiro.test(ct$IdadeG)
  
qqnorm(st$IdadeG)
qqline(st$IdadeG,col="red")
shapiro.test(st$IdadeG)

var.test(IdadeG~Toxemia,data=dados)

t.test(IdadeG~Toxemia,data=dados,var.equal=TRUE,conf.level=0.95)
