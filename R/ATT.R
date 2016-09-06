#' Calculates the Average Treatment Effect for the Treated
#' for a given MathIt object and a response vector.
#' @param obj MatchIt Object
#' @param Y Response Vector
#' @return The ATT for \code{Y}
#' @examples
#' data("lalonde")
#' m.out  <- matchit(treat ~ educ + black, data = lalonde)
#' att(obj = m.out, Y = lalalonde$re78)
att <- function(obj, Y){
  stopifnot(is(obj, "matchit"))
  ww <- obj$weights
  tt <- obj$treat
  mut <- weighted.mean(Y[ww > 0 & tt == 1], ww[ww > 0 & tt == 1])
  muc <- weighted.mean(Y[ww > 0 & tt == 0], ww[ww > 0 & tt == 0])
  mut - muc
}