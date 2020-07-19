############### DADOS ###################################################################

# x0 = preço inicial
# K = strike  
# r = taxa de juros 
# sigma = volatilidade 
# T = maturidade

x0 <- 1
K <- 1.1
r <- 0.03
sigma <- 0.15
T <- 1

require(plyr)
require(LSMonteCarlo)
require(fExoticOptions)
require(derivmkts)

################ Calculando por BMS  ###################################################

EU_call_bs = function(x0 = 1, K = 1.1, r = 0.03, sigma = 0.15, t = 1) 
{
  d1 = (log(x0/K)+(r+((sigma)^2)/2)*t)/(sigma*sqrt(t)) 
  d2 = d1-(sigma*sqrt(t))
  
  return((x0*pnorm(d1))-(K*exp(-r*t)*pnorm(d2)))
}

EU_put_bs = function(x0 = 1, K = 1.1, r = 0.03, sigma = 0.15, t = 1)
{ 
  d1 = (log(x0/K)+(r+((sigma)^2)/2)*t)/(sigma*sqrt(t)) 
  d2 = d1-(sigma*sqrt(t))
  
  return((K*exp(-r*t)*pnorm(-d2))-(x0*pnorm(-d1)))
}

EU_call_bs()
EU_put_bs()

################################ Simulação Monte Carlo  - CALCULANDO TEMPO

## PUTT Função

euro_putt <- function (x0, K, sigma, T, r, n){
  
  XT <- x0*exp((r-0.5*sigma^2)*T + sigma*rnorm(n, mean = 0, sd =1)*sqrt(T)) #preço
  
  payoff <- pmax(K-XT,0)*exp(-r*T)  #pay-off
  
  P.call <- mean(payoff)
  
  SE <- 1.96*sd(payoff)/sqrt(n)
  I <- P.call - SE
  S <- P.call + SE
  return(c(Putt=P.call,Erro=SE,Inferior=I,Superior=S))
}

## CALL Função

euro_call <- function (x0, K, sigma, T, r, n){
  
  XT <- x0*exp((r-0.5*sigma^2)*T + sigma*rnorm(n, mean = 0, sd =1)*sqrt(T)) #preço
  
  payoff <- pmax(XT-K,0)*exp(-r*T)  #pay-off
  
  P.call <- mean(payoff)
  
  SE <- 1.96*sd(payoff)/sqrt(n)
  I <- P.call - SE
  S <- P.call + SE
  return(c(Call=P.call,Erro=SE,Inferior=I,Superior=S))
}



## SIMULAÇÔES

#### PUTT

#Simulação 100
debut <- proc.time()
p.euro100 <- euro_putt(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=100)
debut - proc.time()
p.euro100

#Simulação 1000
debut <- proc.time()
p.euro1000 <- euro_putt(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=1000)
debut - proc.time()
p.euro1000

#Simulação 5000
debut <- proc.time()
p.euro1000 <- euro_putt(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=5000)
debut - proc.time()
p.euro5000

#Simulação 10000
debut <- proc.time()
p.euro10000 <- euro_putt(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=10000)
debut - proc.time()
p.euro10000

#Simulação 100000
debut <- proc.time()
p.euro100000 <- euro_putt(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=100000)
debut - proc.time()
p.euro100000


#### CALL

#Simulação 100
debut <- proc.time()
c.euro100 <- euro_call(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=100)
debut - proc.time()
c.euro100

#Simulação 1000
debut <- proc.time()
c.euro1000 <- euro_call(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=1000)
debut - proc.time()
c.euro1000

#Simulação 5000
debut <- proc.time()
c.euro5000 <- euro_call(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=5000)
debut - proc.time()
c.euro5000

#Simulação 10000
debut <- proc.time()
c.euro10000 <- euro_call(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=10000)
debut - proc.time()
c.euro10000

#Simulação 100000
debut <- proc.time()
c.euro100000 <- euro_call(x0 = 1, K = 1.1, sigma = 0.15 , T = 1, r = 0.03, n=100000)
debut - proc.time()
c.euro100000


################################## OPÇÃO AMERICANA

debut <- proc.time()
APut100 <- AmerPutLSM(Spot=1, Strike=1.1, sigma=0.15, r=0.03,  n=2, m=100)
summary(APut100)
price(APut100)
debut - proc.time()

debut <- proc.time()
APut1000<-AmerPutLSM(Spot=1, Strike=1.1, sigma=0.15, r=0.03,  n=2, m=1000)
summary(APut1000)
price(APut1000)
debut - proc.time()

debut <- proc.time()
APut5000<-AmerPutLSM(Spot=1, Strike=1.1, sigma=0.15, r=0.03,  n=2, m=5000)
summary(APut5000)
price(APut5000)
debut - proc.time()

debut <- proc.time()
APut10000<-AmerPutLSM(Spot=1, Strike=1.1, sigma=0.15, r=0.03,  n=2, m=10000)
summary(APut10000)
price(APut10000)
debut - proc.time()

debut <- proc.time()
APut100000<-AmerPutLSM(Spot=1, Strike=1.1, sigma=0.15, r=0.03,  n=2, m=100000)
summary(APut100000)
price(APut100000)
debut - proc.time()

################################## OPÇÃO ASIATICA

## Putt

debut <- proc.time()
arithasianmc(s = 1, k = 1.1, v = 0.15, r = 0.03, tt = 1, d = 0, m = 252, numsim=100, printsds=T)
debut - proc.time()

debut <- proc.time()
arithasianmc(s = 1, k = 1.1, v = 0.15, r = 0.03, tt = 1, d = 0, m = 252, numsim=1000, printsds=T)
debut - proc.time()

debut <- proc.time()
arithasianmc(s = 1, k = 1.1, v = 0.15, r = 0.03, tt = 1, d = 0, m = 252, numsim=5000, printsds=T)
debut - proc.time()

debut <- proc.time()
arithasianmc(s = 1, k = 1.1, v = 0.15, r = 0.03, tt = 1, d = 0, m = 252, numsim=10000, printsds=T)
debut - proc.time()

debut <- proc.time()
arithasianmc(s = 1, k = 1.1, v = 0.15, r = 0.03, tt = 1, d = 0, m = 252, numsim=100000, printsds=T)
debut - proc.time()



structure(arithasianmc)

