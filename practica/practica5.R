
rw_decision <- function(n_pasos=1000,drift=0.1,umbral=10,x_ini=5,std=1,t0=500){
  x <- x_ini
  i <- 1
  while(i < n_pasos & x<umbral & x > 0){
    x <- x + rnorm(1,drift,std)
    i <- i + 1
  }
  return(c(i+t0, x))
}
rw_decision()

experimento <- function(n=1000,n_pasos=5000,x_ini=5, drift=0.1, std=1,umbral=10,t0=500){
  ts <- rep(NA,n)
  rs <- rep(NA,n)
  for(i in seq(1,n)){
    t_r <- rw_decision(n_pasos,drift,umbral,x_ini,std,t0)
    ts[i] <- t_r[1]
    rs[i] <- t_r[2]
  }
  return(list("rt"=ts, "res"=rs))
}

# 5.1 
data <- read.csv('datos.csv', header =T)

# 5.2 
colnames(data)

# 5.2.a
p_T = c()
for(i in unique(data[,"subject"])){
  n = sum(data[,"subject"]==i)
  p_T = c(p_T, sum(data[data[,"subject"]==i,"correct"])/n )
}

# 5.2.b
plot(abs(data[data[,"subject"]==i,"dots_left"]-data[data[,"subject"]==i,"dots_right"]))

# 5.3
plot(abs(data[data[,"subject"]==1,"dots_left"]-data[data[,"subject"]==1,"dots_right"]),type="l")
for(i in 2:54){
  lines(abs(data[data[,"subject"]==i,"dots_left"]-data[data[,"subject"]==i,"dots_right"]))
  
}
corte = 20

p_T = c()
for(i in unique(data[,"subject"])){
  n = sum(data[,"subject"]==i)
  p_T = c(p_T, sum(data[data[,"subject"]==i,"correct"][-seq(20)])/(n-20) )
}
# Se corri\'o para la izquierda

# 5.4
hist(log(data[,"rt"],10))
# m\'as de 10 segundos son utliers
# Si lo restrijo en 5 segundos salen 96 puntos.
96==sum(data[,"rt"]>5*1000)

# 5.5. 
hist(log(data[,"rt"][data[,"correct"] & data[,"rt"]<5*1000],10),freq = F)
hist(log(data[,"rt"][!data[,"correct"] & data[,"rt"]<5*1000],10),add=T,col=rgb(0,0,0,0.3),freq = F)
# Se corre hacia la derecha

# 5.6
10^2.8/1000
10^3.3/1000
# Los datos se generan entre 0.63 y 2 segundos.
sintetico = experimento(n=330,n_pasos=5000,x_ini=5, drift=0.005, std=.1,umbral=10)
sum(sintetico[["res"]]>10)/(330)
hist(log(sintetico[["rt"]],10),add=T,col=rgb(0.7,0.3,0.1,0.3),freq = F)


# 5.7 (TERMINAR)
# La capacidad metaconitiva de un participante puede definirse como la correspondencia entre la
# certeza de la respuesta y la confianza reportada. Una de las formas de medirla es usando una curva ROC de
# tipo 2 en la que “hits” corresponde a reportar alta confianza en los trials correctos y baja confianza en los
# trials incorrectos. Más detalles y código en el paper How to measure metacognition. Con los datos obtenidos
# de cada participante medir el area bajo la curva ROC de tipo 2. Luego hacer un histograma de los valores
# obtenidos

area_roc <- function(fas,hits){
  fas <- p_confianza_dado_incorrecto
  hits <- p_confianza_dado_correcto
  dx <- fas[-1]-fas[-length(fas)]
  dy <- hits[-1]-hits[-length(hits)]
  return(sum(dx*hits[-1] - (dx*dy)/2) -0.5)
}

aucs = c()
for( j in unique(data[,"subject"])){#j=10
  p_confianza_dado_correcto <- c()
  p_confianza_dado_incorrecto <- c()
  filtro <- data[,"subject"]==j & data[,"trial"]>20 
  #n = sum(data[filtro,"correct"])
  #m = sum(!data[filtro,"correct"])
  n = sum(filtro)
  for(c in rev(seq(0,6))){#c=2
    alta_confianza_dado_correctos <- sum(data[filtro & data[,"correct"],"confidence"] >c)
    alta_confianza_dado_incorrectos <- sum(data[filtro & !data[,"correct"],"confidence"] > c)
    hit <- alta_confianza_dado_correctos/sum(data[filtro,"correct"])
    fa <- alta_confianza_dado_incorrectos/sum(!data[filtro,"correct"])
    
    p_confianza_dado_correcto <- c(p_confianza_dado_correcto, hit)
    p_confianza_dado_incorrecto <- c(p_confianza_dado_incorrecto, fa)
  }
  #plot(p_confianza_dado_incorrecto,p_confianza_dado_correcto)
  #abline(0,1)
  #polygon(c(rev(p_confianza_dado_incorrecto),p_confianza_dado_incorrecto),c(rev(p_confianza_dado_incorrecto),p_confianza_dado_correcto),col=rgb(0,0,0,0.3))
  aucs <- c(aucs,area_roc(p_confianza_dado_incorrecto,p_confianza_dado_correcto))
}

hist(aucs)
