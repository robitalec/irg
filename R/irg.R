#' IRG
#'
#' Calculate the instantaneous rate of green-up.
#'
#' The DT argument expects a data.table of model estimated parameters for double logistic function of NDVI for each year and individual. Since it is the rate of green-up, model parameters required are only xmidS and scalS.
#'
#' The scaled argument is used to optionally rescale the IRG result to 0-1, for each year and individual.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' @inheritParams model_ndvi
#' @inheritParams model_params
#' @param fitted boolean indicating if input DT is the result of model_ndvi.
#' @param scaled boolean indicating if irg should be rescaled between 0-1 within id and year. If TRUE, provide id and year. Default is TRUE.
#'
#' @return
#'
#' Extended data.table 'irg' column of instantaneous rate of green-up calculated for each day of the year, for each individual and year.
#'
#' @export
#'
#' @family irg
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read in example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' # Filter and scale NDVI time series
#' filter_ndvi(ndvi)
#' scale_doy(ndvi)
#' scale_ndvi(ndvi)
#'
#' # Double logistic model parameters given starting parameters for nls
#' mods <- model_params(
#'   ndvi,
#'   xmidS = 0.44,
#'   xmidA = 0.80,
#'   scalS = 0.05,
#'   scalA = 0.01
#' )
#'
#' ## Option 1: fitted = FALSE
#' # Calculate IRG for each day of the year
#' #   directly from model_params
#' calc_irg(mods, fitted = FALSE)
#'
#' ## Option 2: fitted = TRUE
#' # Fit double log to NDVI
#' fittedNDVI <- model_ndvi(mods)
#'
#' # Calculate IRG for each day of the year
#' calc_irg(fittedNDVI, fitted = TRUE)
calc_irg <-
	function(DT,
					 fitted = TRUE,
					 scaled = TRUE,
					 id = 'id',
					 year = 'yr') {
		# NSE error
		xmidS <- scalS <- irg <- NULL

		check_col(DT, 'xmidS')
		check_col(DT, 'scalS')

		if (fitted) {
			if (!anyDuplicated(DT, by = c(id, year, 'xmidS', 'scalS'))) {
				stop('did not find duplicates, did you model_ndvi? - see Details.')
			}
		} else if (!fitted) {
			if (anyDuplicated(DT, by = c(id, year, 'xmidS', 'scalS'))) {
				stop('duplicates found, are you sure it is "fitted"? - see Details.')
			} else {
				DT <- DT[rep(1:.N, each = 366)][, t := julseq$t]
			}
		}

		if (any(unlist(DT[, lapply(.SD, function(x)
			any(is.na(x)))]))) {
			warning('NAs found in DT, IRG will be set to NA.')
		}

		DT[, irg :=
			 	(exp((t + xmidS) / scalS)) /
			 	(2 * scalS * (exp(1) ^ ((t + xmidS) / scalS)) +
			 	 	(scalS * (exp(1) ^ ((
			 	 		2 * t
			 	 	) / scalS))) +
			 	 	(scalS * exp(1) ^ ((2 * xmidS) / scalS)))]

		if (scaled) {
			check_col(DT, id, 'id')
			check_col(DT, year, 'year')

			DT[, irg := (irg - min(irg)) / (max(irg) - min(irg)),
				 by = c(id, year)]
		}

		return(DT)
	}



#' IRG
#'
#'
#' @return
#'
#' Extended data.table 'irg' column of instantaneous rate of green-up calculated for each day of the year, for each individual and year.
#'
#' @export
#'
#' @family irg
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read in example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' # Calculate IRG for each day of the year
#' out <- irg(ndvi)
irg <- function(DT) {
	# NSE error

	filter_ndvi(ndvi)
	scale_doy(ndvi)
	scale_ndvi(ndvi)
	m <- model_params(ndvi, )
	calc_irg(model_ndvi(m))

}
