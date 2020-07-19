
library(fOptions)
x = seq(50, 60, by=0.1)       
sigma <- 0.2   
K = 56       
r = 0.065    
T0 = 0
T1 = 1/252
T2 = 2/252
T3 = 3/252

# CALL #

C.0 <- GBSOption(TypeFlag="c", S=x, X=K, Time = T0, r=r, b=r, sigma=sigma)@price

C.1 = GBSOption(TypeFlag="c", S=x, X=K, Time = T1, r=r, b=r, sigma=sigma)@price

C.2 = GBSOption(TypeFlag="c", S=x, X=K, Time = T2, r=r, b=r, sigma=sigma)@price

C.3 <- GBSOption(TypeFlag="c", S=x, X=K, Time = T, type="l", col="green")


par(mfrow=c(1,2))
plot(x,C.1, type = "l", col = "red", ylab = "Option Value")
lines(x,C.2, type="l", col = "blue")
lines(x,C.3, type="l", col = "black")
lines(x,C.0, type="l", col="green")

# PUT #

P.0 = GBSOption(TypeFlag="p", S=x, X=K, Time=T0, r=r, b=r, sigma=sigma)@price

P.1 = GBSOption(TypeFlag="p", S=x, X=K, Time=T1, r=r, b=r, sigma=sigma)@price

P.2 = GBSOption(TypeFlag="p", S=x, X=K, Time=T2, r=r, b=r, sigma=sigma)@price

P.3 = GBSOption(TypeFlag="p", S=x, X=K, Time=T3, r=r, b=r, sigma=sigma)@price

plot(x,P.1, type = "l", col = "red", ylab = "Option Value")
lines(x,P.2, type="l", col = "blue")
lines(x,P.3, type="l", col = "black")
lines(x,P.0, type="l", col="green")
