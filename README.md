# QuantFinance

This repo consolidates code I created in the context of my undergraduate and graduate studies (with some minor adaptations). Both programs implement elementary algorithms based on the flawed assumption of normally-distributed returns; they should definitely not be used in real-world applications!  

## portfolio_optimizer.r

Takes a matrix of historical returns and generates a portfolio optimized to user-defined criteria, using a gradient-descent algorithm. I created this script to try to find a solution to a portfolio-optimization problem that turned out not to have any solution (there was an error in the problem statement), while taking UQAM's ACT4310 actuarial finance class.  

The original repo is archived [here](https://github.com/MathieuBeaudoin/SimplePortfolioOptimizer.git).

## options_pricing.r

Prices European vanilla options using either the Black-Scholes formula or its binomial-tree approximation. Includes a function to compute implied volatility from an option's price and other parameters.  

The original version included only the binomial approximation (but also allowed work on exotic options), and is archived [here](https://github.com/MathieuBeaudoin/BinomialOptionPricing.git). It should be noted that the latest version is taken from work I submitted for a *group* assignment in the context of Université de Montréal's ACT6230 class, but since, in the division of labour for that assignment, I was in charge of the coding exercises, I feel it is accurate to represent this code as my own work.
