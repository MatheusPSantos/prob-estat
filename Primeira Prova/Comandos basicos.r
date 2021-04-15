#######################################################################
# Comandos Basicos
#######################################################################
help.start()

setwd("E:\\Programas") # Fixar o diretorio de trabalho

dados <- read.csv(file="dados.csv",sep=";") #Lendo o conjunto de dados

dados[18,2]
dados[ ,6]

circ <- dados$Circunferencia

min(circ)

dados[dados$Esteroides=="Sim", ]

##############################################################
# demo() (Comando para ver alguns exemplos)
##############################################################
demo(Hershey)
demo(Japanese)
demo(plotmath)
demo(graphics)
demo(image)
demo(persp)

##############################################################
# Bonus
##############################################################

PLOT<-function(h = 9){ 
  # set up plot  
  xrange=c(-15,15)  
  yrange=c(0,16)  
  plot(0,xlim=xrange,ylim=yrange,type='n', axes = F, xlab = "", ylab = "")  
  
  # draw outer envelope  
  yr=seq(yrange[1],yrange[2],len=50)  
  offsetFn=function(y){2*sin(0+y/3.5)}  
  offset=offsetFn(yr)  
  leftE = function(y){-10-offsetFn(y)}  
  rightE = function(y){10+offsetFn(y)}  
  
  xp=c(leftE(yr),rev(rightE(yr))) 
  yp=c(yr,rev(yr))  
  polygon(xp,yp,col="#ffeecc",border=NA) 
  
  # feasible region upper limit: 
  # left and right defined by triple-log function:  
  xt=seq(0,rightE(h),len=100)   
  yt=log(1+log(1+log(xt+1)))   
  yt=yt-min(yt)  
  yt=h*yt/max(yt)  
  x=c(leftE(h),rightE(h),rev(xt),-xt) 
  y=c(h,h,rev(yt),yt) 
  polygon(x,y,col="red",border=NA)  
}

PLOT()
 
