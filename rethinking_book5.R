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


