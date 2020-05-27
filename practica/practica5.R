
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
p_confianza_dado_correcto <- c()
p_confianza_dado_incorrecto <- c()
n = sum(data[,"correct"][data[,"subject"]==1][-seq(20)])
m = sum(!data[,"correct"][data[,"subject"]==1][-seq(20)])
for(c in seq(7)){
  
  a <- sum(data[,"confidence"][data[,"subject"]==1 & data[,"correct"]][-seq(20)] >=c)
  b <- sum(data[,"confidence"][data[,"subject"]==1 & !data[,"correct"]][-seq(20)] >=c)
  
  p_confianza_dado_correcto <- c(p_confianza_dado_correcto, a)
  p_confianza_dado_incorrecto <- c(p_confianza_dado_incorrecto, b)
}

