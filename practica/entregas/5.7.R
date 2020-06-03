data <- read.csv('datos.csv', header =T)

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
