# Just for fun
library(rethinking)
data(WaffleDivorce)
d <- WaffleDivorce
# standardize variables
d$D <- standardize( d$Divorce )
d$M <- standardize( d$Marriage )
d$A <- standardize( d$MedianAgeMarriage )

formula <- alist(
  D ~ dnorm(mu, sigma),
  mu <- a + b * A,
  a ~ dnorm(0, .2),
  b ~ dnorm(0, .5),
  sigma ~ dexp(1)
)

(m <- map(formula, data = d))

# A L T E R N A T I V E L Y
m5.1 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bA * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
) , data = d )

# TO SIMULATE FROM PRIORS and get plausible lines implied by priors
prior <- extract.prior( m5.1 )
mu <- link( m5.1 , post=prior , data=list( A=c(-2,2) ) )
plot( NULL , xlim=c(-2,2) , ylim=c(-2,2) )
for ( i in 1:50 ) lines( c(-2,2) , mu[i,] , col=col.alpha("red",0.4) )

# compute percentile interval of mean
A_seq <- seq( from=-3 , to=3.2 , length.out=30 )
mu <- link( m5.1 , data=list(A=A_seq) )
mu.mean <- apply( mu , 2, mean )
mu.PI <- apply( mu , 2 , PI )

# plot it all
plot( D ~ A , data=d , col="red" )
lines( A_seq , mu.mean , lwd=2 )
shade( mu.PI , A_seq )

m5.2 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM * M ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
) , data = d )

# DRAWING DAT EASILY
library(dagitty)
dag5.1 <- dagitty( "dag{ A -> D; A -> M; M -> D }" )
coordinates(dag5.1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag( dag5.1 , col.alpha("red", alpha = .8))

# alternatively
DMA_dag1 <- dagitty('dag{ D <- A -> M -> D }')
coordinates(DMA_dag1) <- list( x=c(A=0,D=1,M=2) , y=c(A=0,D=1,M=0) )
drawdag( DMA_dag1 , col.alpha("red", alpha = .8))
impliedConditionalIndependencies(DMA_dag1)

# NOT ONLY DRAWING, but finding implied conditional dependencies
DMA_dag2 <- dagitty('dag{ D <- A -> M }')
impliedConditionalIndependencies( DMA_dag2 )

# The Marriage model from the DAG
m5.3 <- quap(
  alist(
    D ~ dnorm( mu , sigma ) ,
    mu <- a + bM*M + bA*A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bM ~ dnorm( 0 , 0.5 ) ,
    bA ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
  ) , data = d )

precis( m5.3 )

plot( coeftab(m5.1,m5.2,m5.3), par=c("bA","bM") )

# Predictor RESIDUAL plots
m5.4 <- quap(
  alist(
    M ~ dnorm( mu , sigma ) ,
    mu <- a + bAM * A ,
    a ~ dnorm( 0 , 0.2 ) ,
    bAM ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
) , data = d )

mu <- link(m5.4)
mu_mean <- apply( mu , 2 , mean )
mu_resid <- d$M - mu_mean

# Posterior PREDICTION Plots
# call link without specifying new data
# so it uses original data
mu <- link( m5.3 )
# summarize samples across cases
mu_mean <- apply( mu , 2 , mean )
mu_PI <- apply( mu , 2 , PI )
# simulate observations
# again no new data, so uses original data
D_sim <- sim( m5.3 , n=1e4 )
D_PI <- apply( D_sim , 2 , PI )

plot( mu_mean ~ d$D , col="red", ylim=range(mu_PI) ,
xlab="Observed divorce" , ylab="Predicted divorce" )
abline( a=0 , b=1 , lty=2 )
for ( i in 1:nrow(d) ) lines( rep(d$D[i],2) , mu_PI[,i] , col="red" )
# label points -- not working for PyCharm
identify( x=d$D , y=mu_mean , labels=d$Loc )

# Counterfactual plots
# Full model with two regressions
data(WaffleDivorce)
d <- list()
d$A <- standardize( WaffleDivorce$MedianAgeMarriage )
d$D <- standardize( WaffleDivorce$Divorce )
d$M <- standardize( WaffleDivorce$Marriage )

m5.3_A <- quap(
  alist(## A -> D <- M
          D ~ dnorm( mu , sigma ) ,
          mu <- a + bM*M + bA*A ,
          a ~ dnorm( 0 , 0.2 ) ,
          bM ~ dnorm( 0 , 0.5 ) ,
          bA ~ dnorm( 0 , 0.5 ) ,
          sigma ~ dexp( 1 ),
        ## A -> M
          M ~ dnorm( mu_M , sigma_M ),
          mu_M <- aM + bAM*A,
          aM ~ dnorm( 0 , 0.2 ),
          bAM ~ dnorm( 0 , 0.5 ),
          sigma_M ~ dexp( 1 )
) , data = d )

precis(m5.3_A)

# range of values for A
A_seq <- seq( from=-2 , to=2 , length.out=30 )

# Simulating M first, then D
# prep data
sim_dat <- data.frame( A=A_seq )
# simulate M and then D, using A_seq
s <- sim( m5.3_A , data=sim_dat , vars=c("M","D") )

# Plot
plot( sim_dat$A , colMeans(s$D) , ylim=c(-2,2) , type="l" ,
xlab="manipulated A" , ylab="counterfactual D" )
shade( apply(s$D,2,PI) , sim_dat$A )
mtext( "Total counterfactual effect of A on D" )

# Numerical findings
#Expected causal effect of increasig median age at marriage from 20 to 30.
# new data frame, standardized to mean 26.1 and std dev 1.24
sim2_dat <- data.frame( A=(c(20,30)-26.1)/1.24 )
s2 <- sim( m5.3_A , data=sim2_dat , vars=c("M","D") )
mean( s2$D[,2] - s2$D[,1] )

sim_dat <- data.frame( M=seq(from=-2,to=2,length.out=30) , A=0 )
s <- sim( m5.3_A , data=sim_dat , vars="D" )
plot( sim_dat$M , colMeans(s) , ylim=c(-2,2) , type="l" ,
xlab="manipulated M" , ylab="counterfactual D" )
shade( apply(s,2,PI) , sim_dat$M )
mtext( "Total counterfactual effect of M on D" )

# Another examplo -- milk dataset
data(milk)
d <- milk
str(d)

d$K <- standardize( d$kcal.per.g )
d$N <- standardize( d$neocortex.perc )
d$M <- standardize( log(d$mass) )

# MISSING DATA MISSING DATA MISSING DATA
# New data.frame with no missing data. NOTE. Only 17 observations
dcc <- d[ complete.cases(d$K,d$N,d$M) , ]

m5.5_draft <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N ,
    a ~ dnorm( 0 , 1 ) ,
    bN ~ dnorm( 0 , 1 ) ,
    sigma ~ dexp( 1 )
) , data=dcc )

# Testing to see if PRIORS are reasonable
prior <- extract.prior( m5.5_draft )
xseq <- c(-2,2)
mu <- link( m5.5_draft , post=prior , data=list(N=xseq) )

plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("black",0.3) )

m5.5 <- quap(
  alist(
    K ~ dnorm( mu , sigma ) ,
    mu <- a + bN*N ,
    a ~ dnorm( 0 , 0.2 ) ,
    bN ~ dnorm( 0 , 0.5 ) ,
    sigma ~ dexp( 1 )
) , data=dcc )

# Plotting on my own
prior <- extract.prior( m5.5 )
xseq <- c(-2,2)
mu <- link( m5.5 , post=prior , data=list(N=xseq) )

plot( NULL , xlim=xseq , ylim=xseq )
for ( i in 1:50 ) lines( xseq , mu[i,] , col=col.alpha("red",.7) )

# CHECKING THE POSTERIOR
precis(m5.5)

# Plotting
xseq <- seq( from=min(dcc$N)-0.15 , to=max(dcc$N)+0.15 , length.out=30 )
mu <- link( m5.5 , data=list(N=xseq) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( K ~ N , data=dcc )
lines( xseq , mu_mean , lwd=2 )
shade( mu_PI , xseq )

m5.6 <- quap(
alist(
K ~ dnorm( mu , sigma ) ,
mu <- a + bM*M ,
a ~ dnorm( 0 , 0.2 ) ,
bM ~ dnorm( 0 , 0.5 ) ,
sigma ~ dexp( 1 )
) , data=dcc )
precis(m5.6)


m5.7 <- quap(
alist(
K ~ dnorm( mu , sigma ) ,
mu <- a + bN*N + bM*M ,
a ~ dnorm( 0 , 0.2 ) ,
bN ~ dnorm( 0 , 0.5 ) ,
bM ~ dnorm( 0 , 0.5 ) ,
sigma ~ dexp( 1 )
) , data=dcc )
precis(m5.7)

plot( coeftab( m5.5 , m5.6 , m5.7 ) , pars=c("bM","bN") )

# MARKOV EQUIVALENCE OF ALL DAGS
library(dagitty)
dag5.7 <- dagitty( "dag{
M -> K <- N
M -> N }" )
coordinates(dag5.7) <- list( x=c(M=0,K=1,N=2) , y=c(M=0.5,K=1,N=0.5) )
MElist <- equivalentDAGs(dag5.7)
drawdag(MElist)

# CATEGORICAL VARIABLES
data(Howell1)
d <- Howell1
str(d)

mu_female <- rnorm(1e4,178,20)
mu_male <- rnorm(1e4,178,20) + rnorm(1e4,0,10)
precis( data.frame( mu_female , mu_male ) )

# Creating a dummy non-numerical variable
d$sex <- ifelse( d$male==1 , 2 , 1 )
str( d$sex )

m5.8 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a[sex] ,
    a[sex] ~ dnorm( 178 , 20 ) ,
    sigma ~ dunif( 0 , 50 )
) , data=d )
precis( m5.8 , depth=2 )

# Difference between sexes
post <- extract.samples(m5.8)
post$diff_fm <- post$a[,1] - post$a[,2]
precis( post , depth=2 )

data(milk)
d <- milk
str(d)
levels(d$clade)
# Forcing dummy as integers to USE AS INDEX
d$clade_id <- as.integer( d$clade )

d$K <- standardize( d$kcal.per.g )
m5.9 <- quap(
  alist(
    K ~ dnorm( mu , sigma ),
    mu <- a[clade_id],
    a[clade_id] ~ dnorm( 0 , 0.5 ),
    sigma ~ dexp( 1 )
) , data=d )
labels <- paste( "a[" , 1:4 , "]:" , levels(d$clade), sep="")
plot( precis( m5.9 , depth=2 , pars="a" ) , labels=labels ,
xlab="expected kcal (std)" )
