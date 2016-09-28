#' Lechner's Standard Error for the ATT
#'
#' Calculates the Standard Error for the  Average Treatment Effect with Lechner's
#' method.
#'
#' @param obj MatchIt Object
#' @param Y Response Vector
#' @return SE for the ATT of \code{Y}
#' @references
#' Lechner, M. (2001). Identification and estimation of causal effects of multiple treatments under the conditional indepence assumption. In M. Lechner & F. Pfeiffer (Eds.), Econometric Evaluation of Labour Market Policies (pp. 43-58). Physica-Verlag: Heidelberg.
#' @examples
#' \dontrun{
#'   library(MatchIt)
#'   data("lalonde")
#'   m.out  <- matchit(treat ~ educ + black, data = lalonde)
#'   att(obj = m.out, Y = lalonde$re78)
#'   lechner_se(obj = m.out, Y = lalonde$re78)
#' }
#' @export
lechner_se <- function(obj, Y){
  stopifnot(methods::is(obj, "matchit"))
  # Lechner 2001
  ww <- obj$weights
  tt <- obj$treat
  ct <- sum(ww > 0 & tt == 1)  # Count treated
  cc <- sum(ww > 0 & tt == 0)  # Count control
  kk <- (ct/cc) * ww[ww > 0 & tt == 0] # How often "used" as a match?
  vart <- stats::var(Y[ww > 0 & tt == 1])  # raw variance treated
  varc <- stats::var(Y[ww > 0 & tt == 0])  # raw variance control
  sqrt( vart/ct + (sum(kk^2)*varc)/ct^2 )  # Lechner's formula
}
