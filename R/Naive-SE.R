#' Calculates the Standard Error for the  Average Treatment Effect
#' for the Treated with a
#' - parametric
#' _ Zelig like
#' @param obj MatchIt Object
#' @param Y Response Vector
#' @return SE for the ATT of \code{Y}
#' @examples
#' data("lalonde")
#' m.out  <- matchit(treat ~ educ + black, data = lalonde)
#' att(obj = m.out, Y = lalonde$re78)
#' zelig_se(obj = m.out, Y = lalonde$re78)
#' @export
zelig_se <- function(obj, Y){
  stopifnot(methods::is(obj, "matchit"))
  ww <- obj$weights
  tt <- obj$treat
  XX <- cbind(1,obj$X)
  ic <- ww > 0 & tt == 0  # Index used control units
  it <- ww > 0 & tt == 1  # Index used treatment units
  fitc <- fastReg(Y[ic], XX[ic,], ww[ic])
  beta.hat <- matrix(stats::rnorm(1000 * ncol(fitc$vcov)), nrow = 1000, byrow = TRUE) %*% chol(fitc$vcov)
  beta.hat <- sweep(beta.hat, 2, fitc$coef, "+")  # Add mean
  pool.expected <- beta.hat %*% t(XX[it,])
  att.expected  <- colMeans(Y[it] - t(pool.expected))
  return(c(est = mean(pool.expected), sd = stats::sd(pool.expected),
           att.est = mean(att.expected), att.se = sd(att.expected)))
}
