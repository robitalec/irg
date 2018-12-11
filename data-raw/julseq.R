jul_seq <- function(fullyear = TRUE, scaled = TRUE) {
	if (fullyear && scaled) {
		return(data.table(data.table(jul = 1:366,
																 t = seq(0, 1, length.out = 366))))
	}
}

julseq <- jul_seq()
