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

muS <- 2
muR <- 0
sigmaS <- 1
sigmaR <- 1
criterio <- seq(muR-3*sigmaS,muS+3*sigmaS,length.out = 100)

x <- criterio
plot(x,dnorm(x,muR,sigmaR),type="l",axes = F,ann = F,lwd=3,col=rgb(0,0,0,0.6))
lines(x,dnorm(x,muS,sigmaS),lwd=3,lty=1,col=rgb(0,0,0,1))
mtext("Señal", side=1, line=1.75,cex = 2)
mtext("Densidad", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)
legend(-3.2,0.4,lwd=c(3,3),lty=c(1,1), col=c(rgb(0,0,0,1),rgb(0,0,0,0.6)),
       legend = c("N(señal | 2 , 1)","N(ruido | 0 , 1)")
       ,bty = "n",cex = 1.25)


p_P_dado_T <- 1-pnorm(criterio,muS,sigmaS)
p_N_dado_T <- 1- p_P_dado_T
p_N_dado_F <- pnorm(criterio,muR,sigmaS)
p_P_dado_F <- 1- p_N_dado_F
p_T <- 0.5
p_F <- 1 - p_T

#p_P_dado_T_0 <- 1-pnorm(2,muS,sigmaS)
#p_P_dado_F_0 <- 1-pnorm(2,muR,sigmaR)
#p_T_dado_P_0 <- (p_P_dado_T_0*p_T)/sum(p_P_dado_T_0*p_T + p_P_dado_F_0*p_F)


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
plot(criterio,additive,type="l",axes = F,ann = F,lwd=3)
#lines(criterio,multiplicative,lty=6,lwd=3)
points(criterio[which.max(additive)],additive[which.max(additive)],pch=19,col=rgb(0.75,0,0),cex=2)
points(criterio[which.max(multiplicative)],additive[which.max(multiplicative)],cex=2,col=rgb(0,0,0.75),pch=19)
mtext("Criterio", side=1, line=1.75,cex = 2)
mtext("Expected growth rate", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)
legend(-3.1,0.33,pch=c(19,19),col=c(rgb(0,0,0.75),rgb(0.75,0,0)),title="Criterio",
       legend = c("Conservador","Óptimo")
       ,bty = "n",cex = 1.75)

criterio[which.max(additive)] == criterio[which.max(multiplicative)]
-0.09>multiplicative[which.max(additive)]
0.09<multiplicative[which.max(multiplicative)]

lambda_add <- criterio[which.max(additive)]
lambda_mult <- criterio[which.max(multiplicative)]

p_P_dado_T_mul <- 1-pnorm(lambda_mult,muS,sigmaS)
p_P_dado_F_mul <- 1-pnorm(lambda_mult,muR,sigmaS)

multiplicative_likelihood_ratio <- p_P_dado_T_mul/p_P_dado_F_mul
19.6 < multiplicative_likelihood_ratio



plot(x,dnorm(x,muR,sigmaR),type="l",axes = F,ann = F,lwd=3,col=rgb(0,0,0,0.6))
lines(x,dnorm(x,muS,sigmaS),lwd=3,lty=1,col=rgb(0,0,0,1))
mtext("Señal", side=1, line=1.75,cex = 2)
mtext("Densidad", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)
which_mul = seq(which.max(multiplicative),length(x))
polygon(c(x[which_mul],rev(x[which_mul])),c(dnorm(x,muS,sigmaS)[which_mul],rep(0,length(x))[which_mul] ),col=rgb(0,0,0,0.3) )
polygon(c(x[which_mul],rev(x[which_mul])),c(dnorm(x,muR,sigmaR)[which_mul],rep(0,length(x))[which_mul] ),col=rgb(0,0,0,0.3) )
text(2.6,0.13,"~20",cex=1.5)

p_P_dado_T_add <- 1-pnorm(lambda_add,muS,sigmaS)
p_P_dado_F_add <- 1-pnorm(lambda_add,muR,sigmaS)

additive_likelihood_ratio <- p_P_dado_T_add/p_P_dado_F_add
5 < additive_likelihood_ratio

plot(x,dnorm(x,muR,sigmaR),type="l",axes = F,ann = F,lwd=3,col=rgb(0,0,0,0.6))
lines(x,dnorm(x,muS,sigmaS),lwd=3,lty=1,col=rgb(0,0,0,1))
mtext("Señal", side=1, line=1.75,cex = 2)
mtext("Densidad", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)
which_mul = seq(which.max(additive),length(x))
polygon(c(x[which_mul],rev(x[which_mul])),c(dnorm(x,muS,sigmaS)[which_mul],rep(0,length(x))[which_mul] ),col=rgb(0,0,0,0.3) )
polygon(c(x[which_mul],rev(x[which_mul])),c(dnorm(x,muR,sigmaR)[which_mul],rep(0,length(x))[which_mul] ),col=rgb(0,0,0,0.3) )
text(2,0.25,"~5",cex=1.5)




#positivos_T <- rnorm(s,muS,sigmaS) > cr
#positivos_F <- rnorm(r,muR,sigmaR) > cr

additive_history <- matrix(NA,100,1000)
multiplicative_history <- matrix(NA,100,1000)
multiplicative_history_opt <- matrix(NA,100,1000)
is_T <- sample(c(T,F),1000,replace=T )
for(i in 1:100){
  P_dado_T <- rnorm(1000,muS,sigmaS) > lambda_add
  P_dado_F <- rnorm(1000,muR,sigmaS) > lambda_add
  additive_history[i,] <- 1+cumsum(P_dado_T*is_T*u["T","P"] + P_dado_F*(!is_T)*u["F","P"] )
  multiplicative_history[i,] <- cumprod(c(1,1+(P_dado_T*is_T*u["T","P"] + P_dado_F*(!is_T)*u["F","P"])))[-1]
  P_dado_T <- rnorm(1000,muS,sigmaS) > lambda_mult
  P_dado_F <- rnorm(1000,muR,sigmaS) > lambda_mult
  multiplicative_history_opt[i,] <- cumprod(c(1,1+(P_dado_T*is_T*u["T","P"] + P_dado_F*(!is_T)*u["F","P"])))[-1]
}


plot(log(multiplicative_history[1,]),type="l",axes = F,ann = F,
     ylim=c(min(log(multiplicative_history)), max(log(multiplicative_history_opt))),col=rgb(1,0,0,0.3))
#lines(log(additive_history[1,]),type="l",col=rgb(0,0,1,0.3))
lines(log(multiplicative_history_opt[1,]),type="l",col=rgb(0,1,0,0.3))
for (i in 2:100){
#  lines(log(additive_history[i,]),type="l",col=rgb(0,0,1,0.3))
  lines(log(multiplicative_history[i,]),type="l",col=rgb(1,0,0,0.3))
  lines(log(multiplicative_history_opt[i,]),type="l",col=rgb(0,1,0,0.3))
}
mtext("Trials", side=1, line=1.75,cex = 2)
mtext("Retornos efectivo", side=2, line=1.75,cex = 2)
axis(lwd=1,lwd.ticks=1,side=2,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=2,cex.axis=1.33,line=-0.66)
axis(lwd=1,lwd.ticks=1,side=1,labels=NA,cex.axis=0.6,tck=0.02)
axis(lwd=0,side=1,las=1,cex.axis=1.33,line=-0.66)

#######################################
# end 
dev.off()
#system(paste("pdfcrop -m '0 0 0 0'",paste0(nombre,".pdf") ,paste0(nombre,".pdf")))
setwd(oldwd)
par(oldpar, new=F)
#########################################



