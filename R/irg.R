#' IRG
#'
#' Calculate the instantaneous rate of green-up.
#'
#' The DT argument expects a data.table of model estimated parameters for double logistic function of NDVI for each year and individual. Since it is the rate of green-up, model parameters required are only xmidS and scalS.
#'
#' The scaled argument is used to optionally rescale the IRG result to 0-1, for each year and individual.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#' The formula used is described in Bischoff et al. (2012):
#'
#' \deqn{IRG = (exp((t + xmidS) / scalS)) / (2 * scalS * (exp(1) ^ ((t + xmidS) / scalS)) + (scalS * (exp(1) ^ ((2 * t) / scalS))) + (scalS * exp(1) ^ ((2 * xmidS) / scalS)))}
#'
#' (See the "Getting started with irg vignette" for a better formatted formula.)
#'
#' @inheritParams model_ndvi
#' @inheritParams model_params
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
#' ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))
#'
#' # Filter and scale NDVI time series
#' filter_ndvi(ndvi)
#' scale_doy(ndvi)
#' scale_ndvi(ndvi)
#'
#' # Guess starting parameters
#' model_start(ndvi)
#'
#' # Double logistic model parameters given starting parameters for nls
#' mods <- model_params(
#'   ndvi,
#'   return = 'models',
#'   xmidS = 'xmidS_start',
#'   xmidA = 'xmidA_start',
#'   scalS = 0.05,
#'   scalA = 0.01
#' )
#'
#' # Fit double logistic curve to NDVI time series
#' fit <- model_ndvi(mods, observed = FALSE)
#'
#' # Calculate IRG for each day of the year
#' calc_irg(fit)
calc_irg <-
	function(DT,
					 id = 'id',
					 year = 'yr',
					 scaled = TRUE) {
		# NSE error
		xmidS <- scalS <- irg <- NULL

		check_truelength(DT)
		check_col(DT, 'xmidS')
		check_col(DT, 'scalS')
		check_col(DT, 't')

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

			DT[!is.na(irg), irg :=
				 	(irg - min(irg, na.rm = TRUE)) /
				 	(max(irg, na.rm = TRUE) - min(irg, na.rm = TRUE)),
				 by = c(id, year)]
		}

		return(DT)
	}



#' IRG
#'
#' Wrapper function for one step IRG calculation. Only defaults.
#'
#' data.table must have columns:
#'
#' \itemize{
#'   \item 'id' - individual identifier
#'   \item 'yr' - year of observation
#'   \item 'NDVI' - NDVI value
#'   \item 'DayOfYear' - day of year/julian day of observation
#'   \item 'SummaryQA' - summary quality value for each sample (provided by MODIS)
#' }
#'
#' @inheritParams filter_qa
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
#' ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))
#'
#' # Calculate IRG for each day of the year and individual
#' out <- irg(ndvi)
irg <- function(DT) {
	check_truelength(DT)

	filter_ndvi(DT)
	scale_doy(DT)
	scale_ndvi(DT)
	model_start(DT)
	model_params(DT, returns = 'columns',
							 xmidS = 'xmidS_start', xmidA = 'xmidA_start',
							 scalS = 0.05, scalA = 0.1)
	return(calc_irg(model_ndvi(DT, observed = FALSE)))
}
