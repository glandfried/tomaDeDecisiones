
## 3.4

d=1.5
p_T=0.5
muR <- 0
muS <- d
sigma <- 1
lambda = d/2


# 3.4.A
# Simular 10000 trials del experimento. Hacer un histograma de la
# señal interna correspondiente a cada estímulo
signal <- rnorm(5000,muS,sigma)
noise <- rnorm(5000,muR,sigma)
hist(signal,col=rgb(0,0,0.75,0.4),xlim = c(min(noise),max(signal)))
hist(noise,col=rgb(0.75,0,0,0.4),add=T)

# 3.4.B 
# Suponga que el participante debe reportar, además de su decisión, la confianza en su decisión en una
# escala de tres unidades: {baja, media, alta}. Para eso, utiliza 4 criterios nuevos, 2 a cada lado del criterio de
# decisión ubicados en: Alta −0.5, Media 0.2, Baja (0.75) Baja 1.7 MEdia 2.5 Alta)
lambdas  = c(-0.5, 0.2, 0.75, 1.7, 2.5)

# 3.4.C
# Crear una matriz de 2x6 que contenga el número de respuestas de cada tipo:
res <-matrix(NA,nrow=2,ncol=6)
colnames(res)<-c('N_alta','N_media', 'N_baja', 'P_baja', 'P_media', 'P_alta')
rownames(res) <- c('T','F')
res['T','N_alta'] <- sum(signal < -0.5);res['F','N_alta'] <-  sum(noise < -0.5)
res['T','N_media'] <- sum(signal > -0.5&signal < 0.2); res['F','N_media'] <- sum(noise > -0.5&noise < 0.2)
res['T','N_baja'] <- sum(signal > 0.2&signal < 0.75);res['F','N_baja']<- sum(noise > 0.2&noise < 0.75)
res['T','P_baja']<- sum(signal > 0.75&signal < 1.7);res['F','P_baja']= sum(noise > 0.75&noise < 1.7)
res['T','P_media']= sum(signal > 1.7&signal < 2.5);res['F','P_media']=sum(noise > 1.7&noise < 2.5)
res['T','P_alta']= sum(signal > 2.5);res['F','P_alta']=sum(noise > 2.5)

# 3.4.D
# Graficar la curva ROC empírica. Para eso dividir cada entrada de la matriz anterior por el número total
# de trials de cada tipo y acumular los valores, de derecha a izquierda.
n <- 5000
hits <- c(1); fa <- c(1)
hits <- c(hits,sum(res['T',2:6])/n); fa <- c(fa,sum(res['F',2:6]/n)) 
hits <- c(hits,sum(res['T',3:6])/n); fa <- c(fa,sum(res['F',3:6]/n))
hits <- c(hits,sum(res['T',4:6])/n); fa <- c(fa,sum(res['F',4:6]/n))
hits <- c(hits,sum(res['T',5:6])/n); fa <- c(fa,sum(res['F',5:6]/n))
hits <- c(hits,sum(res['T',6:6])/n); fa <- c(fa,sum(res['F',6:6]/n))
hits <- c(hits,0); fa <- c(fa,0)

plot(fa,hits,ylim=c(0,1),xlim=c(0,1),type="l",axes = F)
lines(c(0,1),c(0,1))
polygon(c(rev(fa),fa),c(rev(fa),hits),col=rgb(0,0,0,0.3))
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)  

