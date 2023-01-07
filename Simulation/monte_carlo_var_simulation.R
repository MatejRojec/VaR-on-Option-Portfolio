library(pracma)
# Inputs into the B-S model

K = 100
r = 0.01
sigma = 0.1
T = 1
S0 = 102

# call option
d1 <- (log(S0/K) + (r + sigma^2/2) * T)/(sigma * sqrt(T))
d2 <- d1 - sigma * sqrt(T)
phid1 <- pnorm(d1)
call_price <- S0 * phid1 - K * exp(-r * T) * pnorm(d2)

# put option
d11 <- (log(S0/K) + (r + sigma^2/2) * T)/(sigma * sqrt(T))
d22 <- d1 - sigma * sqrt(T)
phimd11 <- pnorm(-d11)
put_price <- -S0 * phimd11 + K * exp(-r * T) * pnorm(-d22)
c(call_price, put_price)

#

Z <- rnorm(100, mean=0, sd=1)
WT <- sqrt(T) * Z

ST = S0*exp((r - 0.5*sigma^2)*T + sigma*WT)
  













# price and standard error of call option
simulated_call_payoffs <- exp(-r*T)*pmax(ST-K,0)
# price and standard error of put option
simulated_put_payoffs <- exp(-r*T)*pmax(K-ST,0)
price_put <- simulated_put_payoffs

