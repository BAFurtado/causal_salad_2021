# 4e1
# likelihood is the y_i variable that follows a Normal(mu, sigma)
# 4e2
# tow parameters in the posterior distribution
# 4e3
# See README.md figure
# 4e4
# mu_i = alpha + beta x_i
# 4e5
# Three. Parameters: sigma alpha beta. Mu is now determined by alpha and beta
# 4m1
size <- 1e6
mu <- rnorm(n = size, mean = 0, sd = 10)
sigma <- rexp(n = size, rate = 1)
sim <- rnorm(n = size , mean = mu, sd = sigma)
plot(density(sim), col="red")

# 4m2
ex.4m2 <- quap(
  alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0, 10),
    sigma ~ dexp(1))
)

# 4m3
ex.4m3 <- quap(
  alist(
    y ~ dnorm( mu , sigma ),
    mu <- a + b*x,
    a ~ dnorm( 0 , 10 ),
    b ~ dunif( 0 , 1 ),
    sigma ~ dexp( 1 )
  )
)

# Weighted basis function
post <- extract.samples( m4.7 )
w <- apply( post$w , 2 , mean )
plot( NULL , xlim=range(d2$year) , ylim=c(-6,6) ,
      xlab="year" , ylab="basis * weight" )
for ( i in 1:ncol(B) ) lines( d2$year , w[i]*B[,i] )

# Posterior interval at 97%
mu <- link( m4.7 )
mu_PI <- apply(mu,2,PI,0.97)
plot( d2$year , d2$doy , col=col.alpha("red",0.3) , pch=16 )
shade( mu_PI , d2$year , col=col.alpha("black",0.5) )

