# Vampires

# Pr(pos_test|vampire) = .95
# Pr(pos_test|mortal) = .01

# Pr(vampire) = .001

# BAYES
# Question: Tested positive, probability you are vampire: Pr(vampire|pos_test)
# Pr(vampire|pos_test) = Pr(pos_test|vampire) * Pr(vampire) / Pr(positive)

# !!! Pr(positive) = average prob positive test results
# !!! Pr(positive) = Pr(positive|vampire) * Pr(vampire) + Pr(positive|mortal) * (1 - Pr(positive))

p_pos_vampire <- .95
p_pos_mortal <- .01

p_vampire = .001

p_pos <- p_pos_vampire * p_vampire + p_pos_mortal * (1 - p_vampire)

p_vampire_pos <- p_pos_vampire * p_vampire / p_pos

# More intuition. Will test positive 999 mortals + 95 vampires out of 100,000 population.
# Thus, 95/(999+95) ~ .087

# Generating samples in R
# posterior mean THE PROBABILITY OF P CONDITIONAL ON THE DATA
value1 = 6
size = value1 + 3
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prob_p <- rep( 1 , 1000 )
prob_p <- rep( 1 , 1000 )
prob_data <- dbinom( value1 , size=size , prob=p_grid )
posterior <- prob_data * prob_p
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(samples)

library(rethinking)
dens( samples )

# Proportion of water is less than .5
# add up posterior probability where p < 0.5
sum( posterior[ p_grid < 0.5 ] )

# Using the sample instead of using actual distribution
sum( samples < 0.5 ) / 1e4

# Probability that lies between .5 and .75
sum( samples > 0.5 & samples < 0.75 ) / 1e4

# You want to know the boundaries of the lower 80% posterior prob.
quantile(samples, .8)

# Middle 80%
quantile(samples, c(.1, .9))

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )

# PI Percentile Intervals from rethinking
PI( samples , prob=0.5 )
# highest posterior density interval (HPDI):  the narrowest interval containing the specified
# probability mass.
HPDI( samples , prob=0.5 )
# Maximum a posteriori MAP estimate
p_grid[ which.max(posterior) ]
# OR for the case of samples
chainmode(samples, adj=.01)

# LOSS FUNCTION. Cost associated with using any particular point estimate.
guess <- .5
sum(posterior * abs(guess - p_grid))

# SAPPLY. Repeat this calculatio for every possible guess
# sapply is equivalent to map
loss <- sapply( p_grid , function(d) sum( posterior*abs( d - p_grid ) ) )

p_grid[which.min(loss)]

# Generative and simulation
# Two tosses. Possible to contain 0, 1 or 2 'water'. For a known probability of .7,
# then you can compute the probability of each
dbinom(0:2, size=2, prob = .7)

# Sampling. Random binomial
rbinom(1, size=2, prob=.7)

dummy_w <- rbinom(1e5 , size=9 , prob=0.7 )
table(dummy_w)/1e5

size <- 9
dummy_w <- rbinom(1e5 , size=size , prob=0.7 )
# dummy_w <- dummy_w/size
table(dummy_w)/1e5
simplehist(dummy_w, xlab="dummy water count" )
plot(table(dummy_w)/1e5)

# One p value.
w <- rbinom( 1e4 , size=9 , prob=0.6 )
simplehist(w)

# Loop through all values
# Redoing the samples
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep(1,1000)
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample( p_grid , size=1e4 , replace=TRUE , prob=posterior )
w <- rbinom( 1e4 , size=9 , prob=samples )
simplehist(w)
