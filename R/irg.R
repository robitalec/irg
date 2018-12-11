#' IRG
#'
#' Instantaneous rate of green-up.
#'
#'
#'
#' The id parameter is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' @inheritParams model_ndvi
#' @inheritParams model_params
#' @param scaled boolean indicating if irg should be rescaled between 0-1 within id and year. If TRUE, provide id and year. Default is TRUE.
#'
#' @return
#'
#' Model parameter data.table appended with 'irg' column of double logistic model of NDVI for a full year. Calculated at the daily scale with the following formula from Bischoff et al. (2012).
#'
#' @export
#'
#' @examples
calc_irg <- function(DT, scaled = TRUE, id = 'id', year = 'yr') {
	# NSE error
	xmidS <- xmidA <- scalS <- scalA <- NULL

	check_col(DT, 'xmidS')
	check_col(DT, 'xmidA')
	check_col(DT, 'scalS')
	check_col(DT, 'scalA')

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
