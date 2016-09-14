#' @examples
#' data('lalonde')
#' m.out  <- matchit(treat ~ educ + black, data = lalonde, method = 'full')
#' att(obj = m.out, Y = lalonde$re78)
#' abadie_imbens_se(m.out, lalonde$re78)  # FAILS!
#' m.out <- add_model_matrix(m.out)
#' abadie_imbens_se(m.out, lalonde$re78)
#' @export
add_model_matrix <- function(fit){
  warning("\nThis function is experimental!
Most methods for estimating standard errors are only
documented for NN matching.\nUse with caution!")
  if (is.null(fit$call$method) || fit$call$method != "full") {
    stop("This function only works for Full Matching")
  }
	p <- max(table(fit$subclass))
	n <- sum(fit$treat)
	mm <- matrix(NA, nrow = n, ncol = p)
	ts <- fit$subclass[fit$treat == 1]
	cs <- fit$subclass[fit$weights > 0 & fit$treat == 0]
	for (i in 1:n){
		if (!is.na(ts[i])){
			indic.names <- names(cs)[which(cs == ts[i])]
			mm[i,1:length(indic.names)] <- indic.names
		}
	}
	rownames(mm) <- names(ts)
	fit$match.matrix <- mm
	return(fit)
}