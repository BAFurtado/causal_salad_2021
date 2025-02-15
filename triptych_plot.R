library(rethinking)
data(tulips)
d <- tulips
str(d)

# blooms of the tulips are the outcome
# water (low 1 to high 3) and shade (high ligh exposure 3 to low)
# are predictors.
# bed are the same cluster inside the greenhouse

# center W and S and scale B by its maximum
d$blooms_std <- d$blooms / max(d$blooms)
d$water_cent <- d$water - mean(d$water)
d$shade_cent <- d$shade - mean(d$shade)

# Just testing a normal distribution (.5, 1)
a <- rnorm( 1e4 , 0.5 , 1 )
sum( a < 0 | a > 1 ) / length( a )
a <- rnorm( 1e4 , 0.5 , 0.25 );
sum( a < 0 | a > 1 ) / length( a )

m8.4 <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw*water_cent + bs*shade_cent ,
    a ~ dnorm( 0.5 , 0.25 ) ,
    bw ~ dnorm( 0 , 0.25 ) ,
    bs ~ dnorm( 0 , 0.25 ) ,
    sigma ~ dexp( 1 )
) , data=d )

m8.5 <- quap(
  alist(
    blooms_std ~ dnorm( mu , sigma ) ,
    mu <- a + bw*water_cent + bs*shade_cent + bws*water_cent*shade_cent ,
    a ~ dnorm( 0.5 , 0.25 ) ,
    bw ~ dnorm( 0 , 0.25 ) ,
    bs ~ dnorm( 0 , 0.25 ) ,
    bws ~ dnorm( 0 , 0.25 ) ,
    sigma ~ dexp( 1 )
) , data=d )

# PLOTTING
par(mfrow=c(1,3)) # 3 plots in 1 row
for ( s in -1:1 ) {
  idx <- which( d$shade_cent==s )
  plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(0,1) ,
    xlab="water" , ylab="blooms" , pch=16 , col='red' )
  mu <- link( m8.5 , data=data.frame( shade_cent=s , water_cent=-1:1 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("red",0.3) )
}

# extract the priors
set.seed(7)
prior <- extract.prior(m8.5)

# replacing prior=post
par(mfrow=c(1,3)) # 3 plots in 1 row
for ( s in -1:1 ) {
  idx <- which( d$shade_cent==s )
  plot( d$water_cent[idx] , d$blooms_std[idx] , xlim=c(-1,1) , ylim=c(-1,2) ,
    xlab="water" , ylab="blooms" , pch=16 , col="red" )
  mu <- link( m8.5 , post=prior, data=data.frame( shade_cent=s , water_cent=-1:1 ) )
  for ( i in 1:20 ) lines( -1:1 , mu[i,] , col=col.alpha("red",0.3) )
}





