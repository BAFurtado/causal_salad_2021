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

# First problem with two parameters
# Starting with brute force, later to substitute by quadratic approximation
# 1. Cria duas listas
mu.list <- seq( from=150, to=160 , length.out=100 )
sigma.list <- seq( from=7 , to=9 , length.out=100 )
# 2. Junta, todoso com toso
post <- expand.grid( mu=mu.list , sigma=sigma.list )
# 3. Soma o log
post$LL <- sapply( 1:nrow(post) , function(i) sum(
dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )
# 4. Soma o log com uma normal dos dados, junto com os parâmetros
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
dunif( post$sigma , 0 , 50 , TRUE )
# 5. Exponential do produto - máxio
post$prob <- exp( post$prod - max(post$prod) )
# 6. Plot
contour_xyz( post$mu , post$sigma , post$prob )
# Heatmap
image_xyz( post$mu , post$sigma , post$prob )

# Sampling
sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]
# Plot
plot( sample.mu , sample.sigma , cex=0.4 , pch=16 , col=col.alpha("red",.1) )

# Characterizing the distributions
dens(sample.mu)
dens(sample.sigma)

PI(sample.mu)
PI(sample.sigma)

# Analyzing sample data to check height problem
d3 <- sample( d2$height , size=20 )
# Doing just for the small samples, same steps.
mu.list <- seq( from=150, to=170 , length.out=200 )
sigma.list <- seq( from=4 , to=20 , length.out=200 )
post2 <- expand.grid( mu=mu.list , sigma=sigma.list )
post2$LL <- sapply( 1:nrow(post2) , function(i)
sum( dnorm( d3 , mean=post2$mu[i] , sd=post2$sigma[i] ,
log=TRUE ) ) )
post2$prod <- post2$LL + dnorm( post2$mu , 178 , 20 , TRUE ) +
dunif( post2$sigma , 0 , 50 , TRUE )
post2$prob <- exp( post2$prod - max(post2$prod) )
sample2.rows <- sample( 1:nrow(post2) , size=1e4 , replace=TRUE ,
prob=post2$prob )
sample2.mu <- post2$mu[ sample2.rows ]
sample2.sigma <- post2$sigma[ sample2.rows ]
plot( sample2.mu , sample2.sigma , cex=0.5 ,
col=col.alpha("red",0.1) ,
xlab="mu" , ylab="sigma" , pch=16 )

dens( sample2.sigma , norm.comp=TRUE )

# Using QUADRATIC APPROXIMATION
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

# DEFINING THE MODEL
flist <- alist(
height ~ dnorm( mu , sigma ) ,
mu ~ dnorm( 178 , 20 ) ,
sigma ~ dunif( 0 , 50 )
)

# FITTING THE MODEL
m4.1 <- quap( flist , data=d2 )

# Taking a look at the model
precis(m4.1)

# Another model with quite a narrow prior
m4.2 <- quap(
alist(
height ~ dnorm( mu , sigma ) ,
mu ~ dnorm( 178 , 0.1 ) ,
sigma ~ dunif( 0 , 50 )
) , data=d2 )
precis( m4.2 )

vcov(m4.1)
diag( vcov( m4.1 ) )
cov2cor( vcov( m4.1 ) )

post <- extract.samples( m4.1 , n=1e4 )
head(post)
precis(post)
plot(post)

# L I N E A R  R E G R E S S I O N
plot( d2$height ~ d2$weight )

library(rethinking)
# PRIORS -- getting a hand of what the choice of prior imply
set.seed(2971)
N <- 100  # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rnorm( N , 0 , 10 )
# Plot
plot( NULL , xlim=range(d2$weight) , ylim=c(-100,400) , xlab="weight" , ylab="height" )
abline( h=0 , lty=2 )
abline( h=272 , lty=1 , lwd=0.5 )
mtext( "b ~ dnorm(0,10)" )
xbar <- mean(d2$weight)
for ( i in 1:N ) curve( a[i] + b[i]*(x - xbar) ,
from=min(d2$weight) , to=max(d2$weight) , add=TRUE ,
col=col.alpha("black",0.2) )

# Using the \beta ~ log-normal(0,1)
b <- rlnorm( 1e4 , 0 , 1 )
dens( b , xlim=c(0,5) , adj=0.1 )

set.seed(2971)
N <- 100
 # 100 lines
a <- rnorm( N , 178 , 20 )
b <- rlnorm( N , 0 , 1 )
# Then use plot above again

# Finding the posterior distribution
# load data again, since it's a long way back
library(rethinking)
data(Howell1); d <- Howell1; d2 <- d[ d$age >= 18 , ]

# define the average weight, x-bar
xbar <- mean(d2$weight)

# fit model
m4.3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - xbar ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d2 )

precis(m4.3)
round(vcov(m4.3), 3)
pairs(m4.3)

# Doing the plotting and completing analysis of the posterior
plot( height ~ weight , data=d2 , col="red" )
post <- extract.samples( m4.3 )
a_map <- mean(post$a)
b_map <- mean(post$b)
curve( a_map + b_map*(x - xbar) , add=TRUE )

# Restimating model with just 10 data cases and plotting
# And then, 50, 150, 352
N <- 352
dN <- d2[ 1:N , ]
mN <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b*( weight - mean(weight) ) ,
    a ~ dnorm( 178 , 20 ) ,
    b ~ dlnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=dN )

# extract 20 samples from the posterior
post <- extract.samples( mN , n=20 )
# display raw data and sample size
plot( dN$weight , dN$height ,
      xlim=range(d2$weight) , ylim=range(d2$height) ,
      col="red" , xlab="weight" , ylab="height" )
mtext(concat("N = ",N))
# plot the lines, with transparency
for ( i in 1:20 )
curve( post$a[i] + post$b[i]*(x-mean(dN$weight)) ,
col=col.alpha("black",0.3) , add=TRUE )

# Getting somebody who weights 50
post <- extract.samples( m4.3 )
mu_at_50 <- post$a + post$b * ( 50 - xbar )
dens( mu_at_50 , col="red" , lwd=2 , xlab="mu|weight=50" )
# Very restrict: narrow with max and min distant 2.68 cm of height
PI( mu_at_50 , prob=0.89 )
max(mu_at_50)
min(mu_at_50)

#  take your quap approximation, sample from the posterior
# distribution, and then compute μ
# for each case in the data and sample from the posterior
# distribution.
mu <- link( m4.3 )
str(mu)

# Now actually doing that for values: from 25 to 75
# define sequence of weights to compute predictions for
# these values will be on the horizontal axis
weight.seq <- seq( from=25 , to=70 , by=1 )

# use link to compute mu
# for each sample from posterior
# and for each weight in weight.seq
mu <- link( m4.3 , data=data.frame(weight=weight.seq) )
str(mu)

# use type="n" to hide raw data
plot( height ~ weight , d2 , type="n" )

# loop over samples and plot each mu value
for ( i in 1:100 )
points( weight.seq , mu[i,] , pch=16 , col=col.alpha("red",0.1) )

# summarize the distribution of mu
mu.mean <- apply( mu , 2 , mean )
mu.PI <- apply( mu , 2 , PI , prob=0.89 )

# plot raw data
# fading out points to make line and interval more visible
plot( height ~ weight , data=d2 , col=col.alpha("red",0.5) )
# plot the MAP line, aka the mean mu for each weight
lines( weight.seq , mu.mean )
# plot a shaded region for 89% PI
shade( mu.PI , weight.seq )

