library("dplyr") 

asset.paths <- function(s0, mu, sigma, nsims, periods) 
{
  nsteps = length(periods)
  dt = c(periods[1], diff(periods))
  drift = mu - 0.5 * sigma^2
  temp = matrix(exp(drift * dt + sigma * sqrt(dt) * rnorm(nsteps * nsims)), nc=nsims)
  for(i in 2:nsteps) temp[i,] = temp[i,] * temp[(i-1),]
  s0 * temp
} 


S =102
K = 100
mu = 0.02
r = 0.05
sigma = 0.2
N = 100
T = 1000
# Single Asset for 10 years
p = (0:T)/T
prices = asset.paths(S, mu, sigma, N, p)

# plot
matplot(prices, type='l', xlab='Time split of 1 year', ylab='Prices',
        main='Selected Price Paths')

call = array(numeric(), c(T+1, N)) 
put = array(numeric(), c(T+1, N)) 

t = (0:T)/T
tt = 1 - t
ttt = tt
for(i in 2:N){
  ttt = ttt = cbind(ttt, tt)
}
  
S = prices

ST = S[T+1,]
for(i in 1:T){
  ST = rbind(ST, S[T+1,])
}


# european call
d1 <- (log(S/K) + (r + sigma^2/2) * ttt)/(sigma * sqrt(ttt))
d2 <- d1 - sigma * sqrt(ttt)
phid1 <- pnorm(d1)
call_price <- S * phid1 - K * exp(-r * ttt) * pnorm(d2)
simulated_call_payoffs <- exp(-r*ttt)*pmax(ST-K,0)
pl_call <- simulated_call_payoffs - call_price


matplot(call_price, type='l', xlab='Time split of 1 year', ylab='Call prices',
        main='Selected Price Paths')

matplot(simulated_call_payoffs, type='l', xlab='Time split of 1 year', ylab='Simulated call payoffs',
        main='Selected Price Paths')

matplot(pl_call, type='l', xlab='Time split of 1 year', ylab='Simulated call profit & loss',
        main='Selected Price Paths')


# european put
d11 <- (log(S/K) + (r + sigma^2/2) * ttt)/(sigma * sqrt(ttt))
d22 <- d1 - sigma * sqrt(ttt)
phimd11 <- pnorm(-d11)
put_price <- -S * phimd11 + K * exp(-r * ttt) * pnorm(-d22)
simulated_put_payoffs <- exp(-r*ttt)*pmax(K-ST,0)
pl_put <- simulated_put_payoffs - put_price

matplot(pl_put, type='l', xlab='Time split of 1 year', ylab='Simulated put profit & loss',
        main='Selected Price Paths')


# portfoilio of 1 put & 1 call

price <- call_price + put_price
pay_off <- simulated_call_payoffs + simulated_put_payoffs
pl <- pay_off -  price

matplot(pl, type='l', xlab='Time split of 1 year', ylab='Simulated put profit & loss',
        main='Selected Price Paths')


alpha = 0.95
alpha_index = floor(length(pl[1, ]) * (1 - alpha))
s = rep( 
for(i in 1:length(pl[,1])){
  -sort(pl[1,])[alpha_index]
}









