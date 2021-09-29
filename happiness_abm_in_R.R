# From the RETHINKING library
library(rethinking)

abm_happiness <-
function (seed=1977, N_years=1000, max_age=65, N_births=20, aom=18) {
  set.seed(seed)
  H <- M <- A<- c()
  for (t in 1: N_years) {
    A <- A + 1
    A <- c(A, rep(1, N_births))
    H <- c(H, seq(from = -2, to = 2, length.out = N_births))
    M <- c(M, rep(0, N_births))
    for (i in 1: length(A)) {
      if (A[i] >= aom & M[i]== 0) {
        M[i] <- rbern(1, inv_logit(H[i] - 4))
    }
  }
  deaths <- which(A > max_age)
    if (length(deaths) > 0){
      A <- A[-deaths]
      H <- H[-deaths]
      M <- M[-deaths]
    }
  }
  d <- data.frame(age = A, married=M, happiness=H)
  return(d)
}

d <- abm_happiness()
print(precis(d))