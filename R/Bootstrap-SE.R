#' Non-parametric bootstrap Standard Error for the ATT
#'
#' Calculate the SE for the ATT with a non-parametric bootstrap method.
#'
#' @param obj MatchIt Object
#' @param Y Response Vector
#' @param max.iter Maximum bootstrap Iterations. Default is 1000.
#' @return SE for the ATT of \code{Y}
#' @examples
#' library(MatchIt)
#' data("lalonde")
#' m.out  <- matchit(treat ~ educ + black, data = lalonde)
#' att(obj = m.out, Y = lalonde$re78)
#' bootstrap.se(obj = m.out, Y = lalonde$re78)
#' @export
bootstrap.se <- function(obj, Y, max.iter = 1e3){
  stopifnot(methods::is(obj, "matchit"))
  # Quelle??? Bootstrapping z.B. Becker, Baumert, etc.
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
