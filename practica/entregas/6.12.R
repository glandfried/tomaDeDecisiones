# p(sA) ~ N(sA|muA,sigmaA)
# p(sV) ~ N(sV|muV,sigmaV)
# p(xA|sA) ~ N(xA|sA,betaA)
# p(xV|sV) ~ N(xV|sV,betaV)

# 6.12.0
sA <- 5
sV <- 10
betaA <- 2
betaV <- 1

# 6.12.1
xA <- rnorm(1,sA,betaA)
xV <- rnorm(1,sV,betaV)

# 6.12.2
s_grilla <- seq(-6,20,by=0.1)
plot(s_grilla,dnorm(xV,s_grilla,betaV),type="l")
lines(s_grilla,dnorm(xA,s_grilla,betaA),type="l")

# 6.12.3
likelihood <- dnorm(xV,s_grilla,betaV)*dnorm(xA,s_grilla,betaA)
plot(s_grilla,likelihood,type="l")

# 6.12.4
area <- sum(likelihood)*0.1
posterior <- likelihood/area
plot(s_grilla,posterior,type="l",lwd=2)
lines(s_grilla,dnorm(xV,s_grilla,betaV),type="l")
lines(s_grilla,dnorm(xA,s_grilla,betaA),type="l")

# 6.12.5
s_map <- s_grilla[which.max(posterior)]

# 6.12.6
num <- (xA/betaA^2)+(xV/betaV^2)
denom <- (1/betaA^2)+(1/betaV^2)
s_map_true <- num/denom