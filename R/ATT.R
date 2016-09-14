#' Calculates the Average Treatment Effect for the Treated
#' for a given MathIt object and a response vector.
#' @param obj MatchIt Object
#' @param Y Response Vector
#' @return The ATT for \code{Y}
#' @examples
#' data("lalonde")
#' m.out  <- matchit(treat ~ educ + black, data = lalonde)
#' att(obj = m.out, Y = lalalonde$re78)
#' @export
att <- function(obj, Y){
  stopifnot(methods::is(obj, "matchit"))
  ww <- obj$weights
  tt <- obj$treat
  mut <- stats::weighted.mean(Y[ww > 0 & tt == 1], ww[ww > 0 & tt == 1])
  muc <- stats::weighted.mean(Y[ww > 0 & tt == 0], ww[ww > 0 & tt == 0])
  mut - muc
}