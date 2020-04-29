
### Ejercicio 3
# Crear una función llamada fun_basicsdt que tenga como parámetros la fracción de hits (h) y de falsas
#alarmas (f) y devuelva el factor de sensibilidad (o discriminabilidad) d’ y el de sesgo en la respuesta, c:
# d = z(h) − z(f )
# c = −0.5 (z(h) + z(f ))

library(MASS)
library(lestat)

fun_basicsdt <- function(h,f){
  # La función 'invcdf(normal(), p)' calcula z(p)
  d <- invcdf(normal(), h) - invcdf(normal(), f)
  c <- -0.5*(invcdf(normal(), h) + invcdf(normal(), f))
  return(c(d,c) )
}

### Ejercicio 4
# El espiritista
# detectó una voz en el 92% de los fragmentos que de hecho contenían una voz frente a un 58% del participante
# escéptico. En cambio, en los fragmentos que no contenían ninguna voz, el espiritista reportó presencia de voz
# el 48% de las veces y el escéptico reportó haber escuchado una voz sólo en el 9% de los audios. Según los
# datos recolectados por el Dr. M: ¿Es cierto que el espiritista tiene una capacidad mayor para detectar voces
# en un contexto de ruido?

fun_basicsdt(0.93,0.48)[1] > fun_basicsdt(0.58,0.09)[1]

### Ejercicio 5
# 50 turistas perdidos 
# el vidente dijo que 31 de los turistas se encontraban a orillas de un río
# 17 de las personas que el vidente predijo que estarían cerca del río.
# 11 de los turistas que el vidente dijo que estarían lejos del río fueron encontrados también a menos de 500 metros de un río.
# ¿Tenía el detective una sensibilidad especial para discriminar la ubicación de los turistas perdidos?
#    c    l 
# c 17   11  28 
# l 14    8  22 
#   31   19  50

hit <- 17/28
falsa_alarma <- 14/22
fun_basicsdt(hit,fr)[1]>0  

### Ejercico 6
# a) 200 personas
#    25 reconocido correctamente de las 30 personas que desarrollaron el síntoma. 
# b) 150 personas
#    identificando exitosamente a 15 de las 28 que terminaron desarrollando el
#Explicar por qué estos datos son insuficientes para concluir cuál test es mejor para identificar a las personas que pueden desarrollar el síntoma.

# Porque no se puede recontruir la matriz de confusion
#   P  N
#T 25  5 30
#F       170

### Ejercicio 7
## 7A
# Fijar los parámetros del modelo de acuerdo a los siguientes datos. El participante tiene d = 1.5.
# ¿Qué desviación estándar elegiría para las distribuciones? Establecer un criterio no sesgado justo en la mitad de los
# valores medios de las distribuciones.

d <- 1.5
c <- 0.75
sigmaS <- 1
sigmaN <- 1
muS <- 1.5
muN <- 0

## 7B

conP <- rnorm(500,muS,sigmaS) > c
sinP <- rnorm(500,muN,sigmaN) > c

confusion <- matrix(0,2,2)
confusion[1,1] <- sum(conP)
confusion[1,2] <- sum(!conP)
confusion[2,1] <- sum(sinP)
confusion[2,2] <- sum(!sinP)

hits <- confusion[1,1]/sum(confusion[1,])
fa <- confusion[1,2]/sum(confusion[2,])

fun_basicsdt(hits,fa)
