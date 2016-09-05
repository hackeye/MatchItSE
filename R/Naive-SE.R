# Zelig/King Estimator
sourceCpp("lib/ffastlm.cpp")
zelig.se <- function(obj, Y){
  ww <- obj$weights
  tt <- obj$treat
  XX <- cbind(1,obj$X)
  ic <- ww > 0 & tt == 0  # Index used control units
  it <- ww > 0 & tt == 1  # Index used treatment units
  #fitc <- lm.wfit(x = XX[ic,], y = Y[ic], w = ww[ic])
  #beta.hat <- mvrnorm(1000, fitc$coefficients, .vcov.lm.fit(fitc))
  fitc <- fastReg(Y[ic], XX[ic,], ww[ic])
  beta.hat <- MASS::mvrnorm(1000, fitc$coef, fitc$vcov)
  pool.expected <- beta.hat %*% t(XX[it,])
  att.expected  <- colMeans(Y[it] - t(pool.expected))
  return(c(est = mean(pool.expected), sd = sd(pool.expected),
           att.est = mean(att.expected), att.se = sd(att.expected)))
}