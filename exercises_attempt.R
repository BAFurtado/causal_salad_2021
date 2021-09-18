# 2M1

# 1
# W <- 3
# L <- 0

# 2
# W <- 3
# L <- 1

# 3
W <- 5
L <- 2

# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# prior <- ifelse( p_grid < 0.5 , 0 , 2 )
# prior <- exp( -5*abs( p_grid - 0.5 ) )

# compute likelihood at each value in grid
likelihood <- dbinom( W , size=W+L , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( p_grid , posterior , type="b" ,
xlab="probability of water" , ylab="posterior probability" )
mtext( "20 points" )

# 2m3 tentative

# 3
# prob. land, given Pr(land|earth)
p_land_earth <- .3
# prob. land, given Pr(land|mars)
p_land_mars <- 1
# prob. earth
p_earth <- .5

# Prob(land) = Prob(earth)Prob(land|earth) + (1 - Prob(earth)) * Prob(land|mars)
p_land <- p_earth * p_land_earth + (1 - p_earth) * p_land_mars

# Prob(earth|land) = Prob(land|earth)Prob(earth)/Prob(land)
p_earth_land <- p_land_earth * p_earth / p_land
p_earth_land

# 2m4
# BB BW WW BB WB WW
card_bb_likelihood <- 2
card_bw_likelihood <- 1
card_ww_likelihood <- 0

likelihood <- c(card_bb_likelihood, card_bw_likelihood, card_ww_likelihood)
prior <- c(1, 1, 1)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

# 2M5
likelihood <- c(card_bb_likelihood, card_bw_likelihood, card_ww_likelihood,
                card_bb_likelihood)
prior <- c(1, 1, 1, 1)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

posterior[1] + posterior[4]

# 2M6
card_bb_likelihood <- 2
card_bw_likelihood <- 1
card_ww_likelihood <- 0

likelihood <- c(card_bb_likelihood, card_bw_likelihood, card_ww_likelihood)
prior <- c(1, 2, 3)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

# 2h1
a_twins <- .1
b_twins <- .2
# 1st twin, next twin?

likelihood <- c(a_twins, b_twins)
prior <- c(1, 1)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
posterior

# 2h2
# probability of species A
# Prob(speciesA)
p_species_a <- .5

# probability of twins, given species A
# Prob(twins|speciesA)
p_twins_a <- .1

# probability of twins, given species B
# Prob(twins|speciesB)
p_twins_b <- .2

# probability of twins
p_twins <- (p_species_a * p_twins_a) + ((1 - p_species_a) * p_twins_b)

# probability of species A, given twins (using Bayes' Theorem)
# (note this is equivalent to `posterior[1]` above)
# Prob(speciesA|twins)
p_species_a_twins = (p_twins_a * p_species_a) / p_twins
p_species_a_twins

# 2h4
# use Bayes' Theorem to determine the probability of species A, given a positive
# test

p_ap <- (0.8 * 0.5) / ((0.5 * 0.8) + (0.5 * 0.35))
p_ap
#> [1] 0.696