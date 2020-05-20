# Simular un trial usando el modelo de caminata al azar con los siguientes parámetros:
n_pasos <- 200
drift <- 0.0
umbral <- 10
x_ini <- umbral/2

x <- rep(NA,100)
x[1] <- x_ini
i <- 1; c<- 0
while(i < n_pasos){
  x[i+1] <- x[i] + rnorm(1,drift,1)
  i <- i + 1
}

plot(x, type = 'l', xlim = c(1,n_pasos), ylim = c(-1,umbral+1), lwd=3, col = "black")
abline(umbral, 0, lwd=3)
abline(0, 0, lwd=3)

# 4.2
# Crear una función que simule n = 1000 trials y devuelva el tiempo 
# de respuesta y la opción elegida en cada trial

n_pasos <- 200
drift <- 0.0
umbral <- 10
x_ini <- umbral/2

rw_decision <- function(n_pasos=1000,drift=0.1,umbral=10,x_ini=5,std=1,t0=50){
  x <- x_ini
  i <- 1
  while(i < n_pasos & x<umbral & x > 0){
    x <- x + rnorm(1,drift,std)
    i <- i + 1
  }
  return(c(i+t0, x))
}
rw_decision()

# 4.3
# Modificar la funci\'on anterior 

# 4.4 
experimento <- function(n=1000,n_pasos=200,x_ini=5, drift=0.1, std=1,umbral=10){
  ts <- rep(NA,n)
  rs <- rep(NA,n)
  for(i in seq(1,n)){
    t_r <- rw_decision(n_pasos,drift,umbral,x_ini,std)
    ts[i] <- t_r[1]
    rs[i] <- t_r[2]
  }
  return(list("ts"=ts,"rs"=rs))
}

drifts <- c(0.001,0.01,0.05,0.1,0.15,0.25,0.5)
p_P_dado_T <- c()
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.001)[["rs"]]>10)/1000)
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.01)[["rs"]]>10)/1000)
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.05)[["rs"]]>10)/1000)
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.1)[["rs"]]>10)/1000)
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.15)[["rs"]]>10)/1000)
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.25)[["rs"]]>10)/1000)
p_P_dado_T <- c(p_P_dado_T,sum(experimento(drift=0.5)[["rs"]]>10)/1000)

plot(drifts,p_P_dado_T)


# 4.5
bins = seq(0.05,0.25,0.002)
res = experimento(drift=0.2)
hist(res[["ts"]]/1000,breaks=bins,col=rgb(0,0,0,0.2))
res = experimento(drift=0.05)
hist(res[["ts"]]/1000,add=T,breaks = bins, col=rgb(0,0,0,0.2))
res = experimento(drift=0.01)
hist(res[["ts"]]/1000,add=T,breaks = bins, col=rgb(0,0,0,0.2))
