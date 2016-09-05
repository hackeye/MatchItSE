# Lechner
lechner.se <- function(obj, Y){
  # Lechner 2001
  ww <- obj$weights
  tt <- obj$treat
  ct <- sum(ww > 0 & tt == 1)  # Count treated
  cc <- sum(ww > 0 & tt == 0)  # Count control
  kk <- (ct/cc) * ww[ww > 0 & tt == 0] # How often "used" as a match?
  vart <- var(Y[ww > 0 & tt == 1])  # raw variance treated
  varc <- var(Y[ww > 0 & tt == 0])  # raw variance control
  sqrt( vart/ct + (sum(kk^2)*varc)/ct^2 )  # Lechner's formula
}