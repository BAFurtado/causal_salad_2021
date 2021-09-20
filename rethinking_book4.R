library(rethinking)
# runif <- random uniform
# sample to generate sequence of 1s and -1s
# Originally
pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
# More intuitively, to get the maximum number of actual steps
pos <- replicate( 1000 , sum( sample(c(-1,1), size = 16, replace = TRUE, prob = c(.5,.5)) ) )
plot(density(pos))

min(pos)
max(pos)

hist(pos)

prod( 1 + runif(12,0,.1) )

growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
dens( growth , norm.comp=TRUE )

big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
dens( big , norm.comp=TRUE )

small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )
dens( small , norm.comp=TRUE )

log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
dens( log.big , norm.comp=TRUE )

dnorm(0,0,.1)

w <- 6; n <- 9;
p_grid <- seq(from=0,to=1,length.out=100)
posterior <- dbinom(w,n,p_grid)*dunif(p_grid,0,1)
posterior <- posterior/sum(posterior)

data(Howell1)
d <- Howell1

str(d)
nrow(d)
precis(d)

d$height
d2 <- d[ d$age >= 18 , ]

dens(d2$height)

# Using priors for parameters to be estimated
# mu -> Normal(128,20)
# sigma -> Uniform(0,50)
# -> sigma has its own distribution.
# it remains positive and has a maximum at 100 cm.
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )
curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )


