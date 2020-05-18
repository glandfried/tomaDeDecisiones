library(MASS)
library(lestat)

fun_basicsdt <- function(h,f){
  # La función 'invcdf(normal(), p)' calcula z(p)
  d <- invcdf(normal(), h) - invcdf(normal(), f)
  c <- -0.5*(invcdf(normal(), h) + invcdf(normal(), f))
  return(c(d,c) )
}

conf_0 <- function(){
  return(matrix(0,2,2,dimnames = list(c("T", "F"),c("P", "N")) ))
}


muestra_y_deteccion <- function(s=100,r=100,cr,muS,muR,sigmaS,sigmaR){
  positivos_T <- rnorm(s,muS,sigmaS) > cr
  positivos_F <- rnorm(r,muR,sigmaR) > cr
  confusion <- matrix(0,2,2)
  colnames(confusion) <- c("P","N")
  rownames(confusion) <- c("T","F")
  confusion[1,1] <- sum(positivos_T)
  confusion[1,2] <- sum(!positivos_T)
  confusion[2,1] <- sum(positivos_F)
  confusion[2,2] <- sum(!positivos_F)
  return(confusion)
}

hits <- function(confusion){
  return(confusion['T','P']/sum(confusion['T',]))
}
fa <- function(confusion){
  return(confusion['F','P']/sum(confusion['F',]))
}

d0 <- function(confusion){
  h <- hits(confusion); f <- fa(confusion)
  return(invcdf(normal(), h) - invcdf(normal(), f))
}
bias <- function(confusion){
  h <- hits(confusion); f <- fa(confusion)
  return(-0.5*(invcdf(normal(), h) + invcdf(normal(), f)))
}



## 3.1
# d0 = 1.5. Igual cantidad ruido yruido+señal. 
# Calcular el porcentaje de respuestas correctas (Pc) para 100 criterios diferentes

muS <- 1.5
muR <- 0
sigma <- 1
criterio <- seq(muR-3*sigma,muS+3*sigma,length.out = 100)

p_hits <- 1-pnorm(criterio,muS,sigma)
p_rc <- pnorm(criterio,muR,sigma)
Pc <- 0.5*p_hits+0.5*p_rc
plot(criterio,Pc,type="line")
abline(v=(muS-muR)/2)

# 3.1.B
# ¿En qué lugar del script anterior se usó que el experimentador 
# presentó los dos tipos de estímulos con la misma probabilidad?
print("3.1.B: En el promedio pesado de detección correcta")

# 3.1.C
# Crear una función que tome como inputs los parámetros de las distribuciones
# la proporción de estímulos ruido y señal que se van a presentar y un vector 
# de criterios a evaluar. 
# La salida de la función debe ser un vector con el porcentaje de respuestas correctas 
# para cada criterio evaluado.

porcentaje_correctas <- function(criterio,p,muS,muR,sigma){
  p_hits <- 1-pnorm(criterio,muS,sigma)
  p_rc <- pnorm(criterio,muR,sigma)
  return(p*p_hits+(1-p)*p_rc)
}
porcentaje_correctas(criterio,0.5,muS,muR,sigma)

## 3.2
# 
respuestas <- conf_0()
respuestas["T",] <- c(496,104)
respuestas["F",] <- c(73,327)

d_prima <- d0(respuestas)
dist <- bias(respuestas)
log_beta <- d_prima*  bias(respuestas)
h <- hits(respuestas); f <- fa(respuestas)
log_beta <- 0.5*(invcdf(normal(), h)^2 - invcdf(normal(), f)^2)
lambda <- (log_beta/d_prima) + 0.5*d_prima

muS <- d_prima
muN <- 0
sigma <- 1
criterio <- lambda

# El sigma es correcto porque se cumple
p_h <- 1-pnorm(lambda,muS,sigma)
h == p_h

# Grafico
x <- seq(from = -3, to = d_prima+3, len = 300)
yN <- dnorm(x, muR, sigma)
yS <- dnorm(x, muS, sigma)
plot(x,yN,type = "l", lwd = 3, col="red", xlab = "respuesta interna")
lines(x,yS,lwd = 4, col="blue")
abline(v=lambda, lwd=3)""
legend(+3, 0.3, legend = c('ruido','señal'), col = c("red", "blue"), lty = 1, lwd = 3)

# 3.2.C
# Calcular el criterio, λ opt , con el que se obtendría un máximo porcentaje de respuestas correctas usando la
# función que crearon en el ejercicio 1C. Con el valor de criterio obtenido, calcular c y log(β) ¿A qué distancia
# está el criterio del participante del criterio ideal? Recordar que c es el criterio medido desde (μ n + μ s )/2 y
# que β = f s (λ)/f n (λ)
criterio <- seq(muR-3*sigma,muS+3*sigma,length.out = 10000)
lambda_opt <- criterio[which.max(porcentaje_correctas(criterio,0.6,muS,muR,sigma))]

opt_lambda <- function(p_s,d){
  beta_opt = (1-p_s)/p_s
  return( (log(beta_opt)/d) + 0.5*d)
}
lambda_opt <- opt_lambda(0.6,d_prima)

# 3.2.D
beta_opt = 0.4/0.6 
beta = exp(log_beta)
dist_opt = lambda_opt - d_prima/2 
# La distancia optima entre lambda y el centro es menor a la elegida
dist_opt < dist 

### 3.3
# Considerar la tarea de un observador que espera detectar un evento poco frecuente, que sucede sólo en el 1%
# de los trials. El evento, por si mismo, es detectable con relativamente alta sensibilidad, d 0 = 2.
beta = 0.99/0.01
d=2
# 3.3.A 
# Calcular el criterio de decisión que debería tener el obserador si quiere minimizar los errores de acuerdo al
# modelo de detección de señales de igual varianza.
lambda_opt <- (log(beta)/d) + 0.5*d
# 3.3.B
# Usando el criterio calculado en A, calcular qué fracción de los eventos se pierde de detectar.
pnorm(lambda_opt,2,1)
# 3,3.C
# Para inducir a un menor número de omisiones, se decide recompensar al observador con $50 con cada hit
# y $1 por cada rechazo correcto.
beta_u = (0.99/0.01) * (1/50)
lambda_opt_u <- (log(beta_u)/d) + 0.5*d
pnorm(lambda_opt_u,2,1)


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

# 3.5.B
A <- conf_0(); C <- conf_0(); E <- conf_0()
A['T',] <- c(294,6); A['F',] <- c(264,36) 
C['T',] <- c(252,48); C['F',] <- c(102,198)
E['T',] <- c(171,129); E['F',] <- c(17,283)
d0(A)
d0(C)
d0(E)

plot(c(0,fa(E),fa(C),fa(A),1),c(0,hits(E),hits(C),hits(A),1),type="l")
lines(c(0,1),c(0,1))

# 3.5.C
# Calcular la pendiente de z(hits) vs. z(f.a.) con la función lm de R.
print("No entiendo 3.5.C")
