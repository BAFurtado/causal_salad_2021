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
library(rethinking)
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

# 4m4
ex.4m3 <- quap(
  alist(
    height ~ dnorm( mu , sigma ), # h_i ~ normal(mu_i, sigma)
    mu <- a + b(x - xbar),        # m_i = alpha + beta(x_i - x_bar)
    a ~ dnorm( 169, 20),
    b ~ dnorm(0, 8 ),
    sigma ~ dexp( 1 )
  )
)

# 4h1
#### Following https://jmgirard.com/statistical-rethinking-ch4/
# First estimate the model
d <- Howell1
formula <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * weight,
  a ~ dnorm(140, 30),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 50)
)

(m <- map(formula, data = d))

# Then, calculate the posterior for each weight
new_weight <- c(46.95, 43.72, 64.78, 32.59, 54.63)
pred_height <- link(m, data = data.frame(weight = new_weight))
expected <- apply(pred_height, 2, mean)
interval <- apply(pred_height, 2, HPDI, prob = 0.89)

# Results
data.frame(
  individual = 1:5,
  weight = new_weight,
  expected = expected,
  lower = interval[1, ],
  upper = interval[2, ]
)

#4h2
d2 <- Howell1[Howell1$age < 18, ]
nrow(d2)

formula <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * weight,
  a ~ dnorm(110, 30),
  b ~ dnorm(0, 10),
  sigma ~ dunif(0, 60)
)
m <- map(formula, data = d2)
precis(m, corr = TRUE)

# Plot
plot(height ~ weight, data = d2, col = col.alpha("red", 0.3))
weight.seq <- seq(from = min(d2$weight), to = max(d2$weight), by = 1)

# Using the same model we fit, but now imposing a new, crescent, ordered sequence of weights
mu <- link(m, data = data.frame(weight = weight.seq))

mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.89)
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)

# PREDICTING HEIGHTS, GIVEN THE WEIGHTS
sim.height <- sim(m, data = list(weight = weight.seq))

height.HPDI <- apply(sim.height, 2, HPDI, prob = 0.89)
shade(height.HPDI, weight.seq)

# 4h3
d <- Howell1
formula <- alist(
  height ~ dnorm(mu, sigma),
  mu <- a + b * log(weight),
  a ~ dnorm(178, 100),
  b ~ dnorm(0, 100),
  sigma ~ dunif(0, 50)
)
m <- map(formula, data = d)
precis(m, corr = TRUE)

plot(height ~ weight, data = d, col = col.alpha("red", 0.4))
weight.seq <- seq(from = min(d$weight), to = max(d$weight), by = 1)
mu <- link(m, data = data.frame(weight = weight.seq))
mu.mean <- apply(mu, 2, mean)
mu.HPDI <- apply(mu, 2, HPDI, prob = 0.97)
lines(weight.seq, mu.mean)
shade(mu.HPDI, weight.seq)
# Estimate and plot the 89% HPDI for the predicted heights
sim.height <- sim(m, data = list(weight = weight.seq))
height.HPDI <- apply(sim.height, 2, HPDI, prob = 0.97)
shade(height.HPDI, weight.seq)