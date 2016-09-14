#' Calculates the Standard Error for the  Average Treatment Effect
#' for the Treated with a
#' @param obj MatchIt Object
#' @param Y Response Vector
#' @return SE for the ATT of \code{Y}
#' @references
#' Abadie, A., Drukker, D., Herr, J.L. & Imbens, G. (2004).Implementing matching estimators for average treatment effects in Stata. The Stata Jorunal (4), pp. 290 - 311.
#'
#' @examples
#' library(MatchIt)
#' data("lalonde")
#' m.out  <- matchit(treat ~ educ + black, data = lalonde)
#' att(obj = m.out, Y = lalonde$re78)
#' abadie_imbens_se(obj = m.out, Y = lalonde$re78)
#' @export
abadie_imbens_se <- function(obj, Y){
  stopifnot(methods::is(obj, "matchit"))
  # Abadie, Drukker, Herr & Imbens 2004
  tt <- obj$treat
  ww <- obj$weights
  mm <- as.matrix(obj$match.matrix[ww[tt==1]>0, ])
  nm <- rownames(obj$X)
  nt <- sum(ww > 0 & tt == 1)  # Count treated
  #nc <- sum(ww > 0 & tt == 0)  # Count control
  tau.hat <- att(obj, Y)
  if (ncol(mm)>1){
    yi <- Y[match(rownames(mm), nm)]
    yl <- sapply(1:ncol(mm), function(j){ Y[match(mm[,j], nm)] })
    sigma.t <- sum(rowMeans( (yi - yl - tau.hat)^2, na.rm=T)/(2*nt))
    #scale.sample.t <- (nt + sum(as.vector(table(mm))^2))/(nt^2)
    scale.sample.t <- (nt + cppKMsqSum(mm, unique(mm[!is.na(mm)]), 1/rowSums(!is.na(mm))))/(nt^2)
  } else {
    yi <- Y[match(rownames(mm), nm)]  # Response treated
    yl <- Y[match(mm[,1], nm)]  # Response control
    sigma.t <- sum( ((yi - yl - tau.hat)^2) / (2*nt) )  # Formula 10ff
    scale.sample.t <- (nt + sum(as.vector(table(mm[,1]))^2))/(nt^2)
    #scale.sample.t <- sum((c(rep(1,nt), as.vector(table(mm[,1])))^2)/(nt^2)) # Formula 5 | geht effizienter (rep usw)
  }
  return( sqrt(scale.sample.t * sigma.t) )
}
