#' Estimate model parameters
#'
#' Estimate parameters for fitting double logistic curve.
#'
#' Arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} allow users to provide global starting estimates to be used for all models. Alternatively, leave NULL and provide those arguments as columns (with matching names) in DT, to provide starting estimates for each id and year. See \code{\link{nls}} for more details on starting parameters.
#'
#' Default value for the year column is 'yr'. If you only have one year of data, set to NULL.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' Formula and arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} following this from Bischoff et al. (2012).
#'
#' \deqn{\frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#' @inheritParams filter_winter
#' @param DT data.table of NDVI time series. Also optionally starting estimates. See Details.
#' @param year year column name. default is 'yr'.
#' @param xmidS starting estimates. see Details. - "spring inflection point"
#' @param xmidA starting estimates. see Details. - "fall inflection point"
#' @param scalS starting estimates. see Details. - "scale parameter for spring green-up portion of the NDVI curve"
#' @param scalA starting estimates. see Details. - "scale parameter for fall dry-down portion of the NDVI curve"
#'
#' @family model
#'
#' @return
#'
#' data.table of model estimated parameters for double logistic model.
#'
#' @references
#'   \url{https://www.journals.uchicago.edu/doi/abs/10.1086/667590}
#'
#'
#' @export
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
model_params <- function(DT,
												 id = 'id',
												 year = 'yr',
												 xmidS = NULL,
												 xmidA = NULL,
												 scalS = NULL,
												 scalA = NULL) {
	# NSE errors
	. <- yr <- NULL

	check_col(DT, 'scaled', extra = ' - did you filter and scale?')

	check_col(DT, id, 'id')
	check_col(DT, year, 'year')

	if (is.null(xmidS)) {
		check_col(DT, 'xmidS')
	}

	if (is.null(xmidA)) {
		check_col(DT, 'xmidA')
	}

	if (is.null(scalS)) {
		check_col(DT, 'scalS')
	}

	if (is.null(scalA)) {
		check_col(DT, 'scalA')
	}

	comb <- unique(
		DT[, .SD, .SDcols =
			 	c(id, year, intersect(colnames(DT),
			 												c('xmidS', 'xmidA', 'scalS', 'scalA')))])

	if (any(comb[, .(checkdup = .N > 1),
							 by = c(id, year)]$checkdup)) {
		stop('non unique values for id (and year),
				  check duplicate starting parameters')
	}

	m <- mapply(function(i, y) {
		tryCatch(
			c(list(id = i, yr = y),
				stats::coef(
					stats::nls(
						formula = scaled ~
							(1 / (1 + exp((xmidS - t) / scalS))) -
							(1 / (1 + exp((xmidA - t) / scalA))),
						data = DT[id == i & yr == y],
						start = list(
							xmidS =
								ifelse(is.null(xmidS),
											 comb[id == i & yr == y][['xmidS']],
											 xmidS),
							xmidA =
								ifelse(is.null(xmidA),
											 comb[id == i & yr == y][['xmidA']],
											 xmidA),
							scalS =
								ifelse(is.null(scalS),
											 comb[id == i & yr == y][['scalS']],
											 scalS),
							scalA =
								ifelse(is.null(scalA),
											 comb[id == i & yr == y][['scalA']],
											 scalA)
						)
					)
				)),
			error = function(e)
				list(id = i, yr = y)
		)
	},
	i = comb$id,
	y = comb$yr,
	SIMPLIFY = FALSE)

	data.table::rbindlist(m, fill = TRUE)
}


#' Model NDVI time series
#'
#' Fit double logistic model to NDVI time series given parameters estimated with model_params.
#'
#' @param DT data.table of model parameters (output from model_params).
#' @return
#'
#' Model parameter data.table appended with 'fitted' column of double logistic model of NDVI for a full year. Calculated at the daily scale with the following formula from Bischoff et al. (2012).
#'
#' \deqn{\frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#'
#' @references
#'   \url{https://www.journals.uchicago.edu/doi/abs/10.1086/667590}
#'
#' @export
#'
#' @family model
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
#' # Fit double logistic curve to NDVI time series for the whole year
#' fittedNDVI <- model_ndvi(mods)
model_ndvi <- function(DT) {
	# NSE error
	xmidS <- xmidA <- scalS <- scalA <- fitted <- NULL

	check_col(DT, 'xmidS')
	check_col(DT, 'xmidA')
	check_col(DT, 'scalS')
	check_col(DT, 'scalA')


	fitDT <- DT[rep(1:.N, each = 366)][, t := julseq$t]

	fitDT[, fitted :=
					(1 / (1 + exp((xmidS - t) / scalS))) -
					(1 / (1 + exp((xmidA - t) / scalA)))]

	return(fitDT)
}




#' Model starting parameters
#'
#' Try guessing starting parameters for model_params and model_ndvi.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' @inheritParams model_params
#'
#' @family model
#'
#' @return
#' @export
#'
#' @examples
model_start <- function(DT, id = 'id', year = 'yr') {
	# NSE errors
	difS <- difA <- NULL

	check_col(DT, 'scaled', extra = ' - did you filter and scale?')
	check_col(DT, 't', extra = ' - did you filter and scale?')


	setkey(DT, 't')
	DT[, difS := scaled - shift(scaled, type = 'lag'),
		 by = c(id, year)]
	DT[, difA := scaled - shift(scaled, type = 'lead'),
		 by = c(id, year)]

	setkey(DT, 'scaled')
	DT[difS > 0, xmidS := .SD[J(0.5), t, roll = 'nearest'],
		 by = c(id, year), .SDcols = c('scaled', 't')]
	DT[difS < 0, xmidA := .SD[J(0.5), t, roll = 'nearest'],
		 by = c(id, year), .SDcols = c('scaled', 't')]

	DT[, xmidS := .SD[!is.na(xmidS), xmidS[1]], by = c(id, year)]
	DT[, xmidA := .SD[!is.na(xmidA), xmidA[1]], by = c(id, year)]

}
