### Price of a European vanilla option, using the Black-Scholes model
BSOptionPrice <- function(S, K, r, T_t=1, sigma=1, isput=F, ...) {
  #####
  # Arguments:
  # S := current asset price
  # K := strike price
  # r := risk-free annual interest rate, compounded continuously
  # T_t := time to maturity, in years
  # sigma := estimated volatility
  # isput := whether the option is a put (T) or a call (F)
  #####
  d1 <- (log(S/K) + (r + sigma^2/2) * T_t) / (sigma * sqrt(T_t))
  d2 <- d1 - sigma * sqrt(T_t)
  Ke <- K * exp(-r * T_t)
  if(isput) {
    Ke * pnorm(-d2) - S * pnorm(-d1)
  } else {
    S * pnorm(d1) - Ke * pnorm(d2)
  }
}


### Implied volatility
BSImplicitVol <- function(OptionPrice, ...) {
  #####
  # Arguments:
  # OptionPrice := current price of the option
  # ... := arguments required by the BSOptionPrice, except for 'sigma'
  #####
  uniroot(
    f = function(x) BSOptionPrice(sigma=x, ...) - OptionPrice,
    interval = c(0, 1e10)
  )$root
}


### Price of a European vanilla option, using the binomial-tree approximation

# This function helps avoid numerical issues when computing tail probabilities
log_nCr <- function(n, k) {
  # Returns the natural log of the number of combinations of size 'k' that
  # can be generated from a set of size 'n'
  if(k <= 1) log(n)
  else if(k == n) 0
  else {
    sum(log((k+1):n)) - sum(log(1:(n-k)))
  }
}

BinOptionPrice <- function(S, K, r, T_t=1, mu=0, sigma=1, n=1, isput=F) {
  #####
  # Arguments: apart from 'mu' and 'n', all arguments refer to the same
  # quantities as in the BSOptionPrice() function
  # mu := annualized drift (mean expected return)
  # n := number of increments into which to break down the time to maturity
  #####
  # Adjustments
  mu <- mu * T_t
  sigma <- sigma * sqrt(T_t)
  # Elementary transformations
  h <- 1 / n
  u <- exp(mu * h + sigma * sqrt(h))
  d <- exp(mu * h - sigma * sqrt(h))
  q <- (exp(r*h*T_t) - d) / (u - d)
  # Number of "up" price movements necessary for the option to be at-the-money
  mK <- (log(K/S) - n * log(d)) / (log(u) - log(d))
  # Binomial probability mass
  # NB: we evaluate its log to sidestep numerical issues
  f_k <- function(k) exp(log_nCr(n, k) + k*log(q) + (n-k)*log(1-q))
  # Value of the stock at time T
  S_T <- function(k) S * u^k * d^(n-k)
  # Payoff function
  payoff <- function(x) max(0, ifelse(isput, K-x, x-K))
  # Partial expectation
  E_payoff_k <- function(k) f_k(k) * payoff(S_T(k))
  # Value of in-the-money scenarios
  if(isput) scenarios <- sapply(0:floor(mK), E_payoff_k)
  else scenarios <- sapply(ceiling(mK):n, E_payoff_k)
  # Total discounted value
  exp(-r * T_t) * sum(scenarios)
}