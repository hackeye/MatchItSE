att <- function(obj, Y){
  ww <- obj$weights
  tt <- obj$treat
  mut <- weighted.mean(Y[ww > 0 & tt == 1], ww[ww > 0 & tt == 1])
  muc <- weighted.mean(Y[ww > 0 & tt == 0], ww[ww > 0 & tt == 0])
  mut - muc
}