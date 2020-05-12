##########
## 3.3.bis (ergodicity)
# Optimality and heuristics in perceptual neuroscience. Justin L. Gardner 
oldpar <- par(no.readonly = TRUE)
oldwd <- getwd()
this.dir <- dirname(parent.frame(2)$ofile)
nombre.R <-  sys.frame(1)$ofile
require(tools)
nombre <- print(file_path_sans_ext(nombre.R))
pdf(paste0(nombre,".pdf"), width = 8, height = 5  )
setwd(this.dir)
###############################
conf_0 <- function(){
  return(matrix(0,2,2,dimnames = list(c("T", "F"),c("P", "N")) ))
}
par(mar=c(3.75,3.75,0.25,0.25))

muS <- 1.5
muR <- 0
sigma <- 1
criterio <- seq(muR-3*sigma,muS+3*sigma,length.out = 100)

p_P_dado_T <- 1-pnorm(criterio,muS,sigma)
p_N_dado_T <- 1- p_P_dado_T
p_N_dado_F <- pnorm(criterio,muR,sigma)
p_P_dado_F <- 1- p_N_dado_F
p_T <- 0.5
p_F <- 1 - p_T

u <-conf_0()
u["T",] <- c(1,0)
u["F",] <- c(-0.99,0)


additive <- ( p_P_dado_T * p_T * u["T","P"] 
              + p_N_dado_T * p_T * u["T","N"]
              + p_N_dado_F * p_F * u["F","N"] 
              + p_P_dado_F * p_F * u["F","P"] )  
multiplicative <- (     p_P_dado_T * p_T * log((1+u["T","P"])) 
                        + p_N_dado_T * p_T * log((1+u["T","N"]))
                        + p_N_dado_F * p_F * log((1+u["F","N"])) 
                        + p_P_dado_F * p_F * log((1+u["F","P"])) )  
plot(criterio,additive,type="l",ylim=c(min(multiplicative),max(additive)),axes = F,ann = F,lwd=2)
lines(criterio,multiplicative,lty=6,lwd=2)
abline(v=criterio[which.max(additive)],lwd=1.25)
abline(v=criterio[which.max(multiplicative)],lty=6,lwd=1.25)

mtext("Criterio", side=1, line=1.75,cex = 2)
mtext("Expected growth rate", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)


criterio[which.max(additive)] == criterio[which.max(multiplicative)]
-0.27>multiplicative[which.max(additive)]
0.05<multiplicative[which.max(multiplicative)]

lambda_add <- criterio[which.max(additive)]
lambda_mult <- criterio[which.max(multiplicative)]

#positivos_T <- rnorm(s,muS,sigmaS) > cr
#positivos_F <- rnorm(r,muR,sigmaR) > cr

additive_history <- matrix(NA,100,1000)
multiplicative_history <- matrix(NA,100,1000)
multiplicative_history_opt <- matrix(NA,100,1000)
is_T <- sample(c(T,F),1000,replace=T )
for(i in 1:100){
  P_dado_T <- rnorm(1000,muS,sigma) > lambda_add
  P_dado_F <- rnorm(1000,muR,sigma) > lambda_add
  additive_history[i,] <- 1+cumsum(P_dado_T*is_T*u["T","P"] + P_dado_F*(!is_T)*u["F","P"] )
  multiplicative_history[i,] <- cumprod(c(1,1+(P_dado_T*is_T*u["T","P"] + P_dado_F*(!is_T)*u["F","P"])))[-1]
  P_dado_T <- rnorm(1000,muS,sigma) > lambda_mult
  P_dado_F <- rnorm(1000,muR,sigma) > lambda_mult
  multiplicative_history_opt[i,] <- cumprod(c(1,1+(P_dado_T*is_T*u["T","P"] + P_dado_F*(!is_T)*u["F","P"])))[-1]
}


plot(log(multiplicative_history[1,]),type="l",axes = F,ann = F,
     ylim=c(min(log(multiplicative_history)), max(log(multiplicative_history_opt))),col=rgb(1,0,0,0.3))
lines(log(additive_history[1,]),type="l",col=rgb(0,0,1,0.3))
lines(log(multiplicative_history_opt[1,]),type="l",col=rgb(0,1,0,0.3))
for (i in 2:100){
  lines(log(additive_history[i,]),type="l",col=rgb(0,0,1,0.3))
  lines(log(multiplicative_history[i,]),type="l",col=rgb(1,0,0,0.3))
  lines(log(multiplicative_history_opt[i,]),type="l",col=rgb(0,1,0,0.3))
}
mtext("Trials", side=1, line=1.75,cex = 2)
mtext("Retornos efectivo", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)

legend(-10,-175,col=c("blue","red","green"),pch=c(19,19,19),title="Proceso y criterio",
       legend = c(expression("Aditivo,"~lambda[add]),expression("Multiplicativo,"~lambda[add]),
                  expression("Multiplicativo,"~lambda[mul]) )
       ,bty = "n",cex = 1.5)
#######################################
# end 
dev.off()
#system(paste("pdfcrop -m '0 0 0 0'",paste0(nombre,".pdf") ,paste0(nombre,".pdf")))
setwd(oldwd)
par(oldpar, new=F)
#########################################



