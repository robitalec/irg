#' IRG
#'
#' Instantaneous rate of green-up.
#'
#'
#' The DT argument expects a data.table of model estimated parameters for double logistic function of NDVI for each year and individual. Since it is the rate of green-up, model parameters required are only xmidS and scalS.
#'
#'
#' The scaled argument is used to optionally rescale the IRG result to 0-1, for each year and individual.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' @inheritParams model_ndvi
#' @inheritParams model_params
#' @param scaled boolean indicating if irg should be rescaled between 0-1 within id and year. If TRUE, provide id and year. Default is TRUE.
#'
#' @return
#'
#' Extended data.table 'irg' column of instantaneous rate of green up calculated for each day of the year, for each individual and year.
#'
#' @export
#'
#' @examples
calc_irg <- function(DT, scaled = TRUE, id = 'id', year = 'yr') {
	# NSE error
	xmidS <- scalS <- irg <- NULL

	check_col(DT, 'xmidS')
	check_col(DT, 'scalS')

	if (any(unlist(DT[, lapply(.SD, function(x) any(is.na(x)))]))) {
		warning('NAs found in DT, IRG will be set to NA.')
	}

	DT <- DT[rep(1:.N, each = 366)][, t := julseq$t]

	DT[, irg :=
				(exp((t + xmidS) / scalS)) /
				(2 * scalS * (exp(1) ^ ((t + xmidS) / scalS)) +
				 	(scalS * (exp(1) ^ ((2 * t) / scalS))) +
				 	(scalS * exp(1) ^ ((2 * xmidS) / scalS)))]

	if (scaled) {
		check_col(DT, id, 'id')
		check_col(DT, year, 'year')

		DT[, irg := (irg - min(irg))/(max(irg) - min(irg)),
			 by = c(id, year)]
	}

	return(DT)
}
