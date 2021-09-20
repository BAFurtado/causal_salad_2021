library(rethinking)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

# 3e1
sum( samples < .2 )/1e4
# 3e2
sum( samples > .8 )/1e4
#3e3
sum( samples > .2 & samples < .8 )/1e4
#3e4
quantile(samples, .2)
quantile(samples, .8)
#3e6
# narrowest interval equal to 66% of the posterior probability
HPDI(samples, prob=.66)
# 3e7
# narrowest interval equal to 66% of the posterior probability
PI(samples, prob=.66)

#3m1
water <- 8
size <- 15
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( water , size=size , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot( p_grid , posterior , type="b" , xlab="probability of water" , ylab="posterior probability" )

samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
HPDI(samples, prob=.9)

#3m4
w <- rbinom(1e4, size = 9, prob = samples)
mean(w == 6)