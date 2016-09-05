# Bootstrapping z.B. Becker, Baumert, etc.
sourceCpp("lib/Rcpp_boot.cpp")
bootstrap.cpp.se <- function(obj, Y, max.iter = 1e4){
  ww <- obj$weights
  tt <- obj$treat
  Ys <- Y[ww > 0]
  ws <- ww[ww > 0]
  ts <- tt[ww > 0]
  Ns <- length(Ys)
  out <- cppWeightedBootstrap(Ys, ts, ws, Ns, MaxIter = max.iter)
  names(out) <- c("mean", "se")
  return(out)
}