brunhild <- function(fit){
	# Rekonstruiert die Modelmatrix
  # Nur bei Fullmatching!
  # model[[1]]$call$method == "full"
	p <- max(table(fit$subclass))
	n <- sum(fit$treat)
	mm <- matrix(NA, nr=n, nc=p)
	ts <- fit$subclass[fit$treat == 1]
	cs <- fit$subclass[fit$weights > 0 & fit$treat == 0]
	for (i in 1:n){
		if (!is.na(ts[i])){
			indic.names <- names(cs)[which(cs == ts[i])]
			mm[i,1:length(indic.names)] <- indic.names
		}
	}
	rownames(mm) <- names(ts)
	return(mm)
}