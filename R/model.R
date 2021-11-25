#' Model starting parameters
#'
#' Try guessing starting parameters for model_params and model_ndvi.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' @param DT filtered and scaled data.table of NDVI time series. Expects columns 'scaled' and 't' are present.
#' @inheritParams model_params
#'
#' @family model
#'
#' @return
#'
#' The input DT `data.table` appended with \code{xmidS_start} and \code{xmidA_start} columns. Note - we curently do not attempt to guess appropriate starting values for \code{scalS} and \code{scalA}.
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
#' # Guess starting parameters for xmidS and xmidA
#' model_start(ndvi)
model_start <- function(DT, id = 'id', year = 'yr') {
	# NSE errors
	difS <- difA <- scaled <- xmidS_start <- xmidA_start <- NULL

	check_col(DT, 'scaled', extra = ' - did you filter and scale?')
	check_col(DT, 't', extra = ' - did you scale doy?')
	check_col(DT, id, 'id')
	check_col(DT, year, 'year')

	overwrite_col(DT, 'xmidS_start')
	overwrite_col(DT, 'xmidA_start')

	data.table::setkey(DT, 't')
	DT[, difS := scaled - data.table::shift(scaled, type = 'lag'),
		 by = c(id, year)]
	DT[, difA := scaled - data.table::shift(scaled, type = 'lead'),
		 by = c(id, year)]

	data.table::setkey(DT, 'scaled')
	DT[difS > 0, xmidS_start := .SD[list(0.5), t, roll = 'nearest'][[1]],
		 by = c(id, year), .SDcols = c('scaled', 't')]
	DT[difS < 0, xmidA_start := .SD[list(0.5), t, roll = 'nearest'][[1]],
		 by = c(id, year), .SDcols = c('scaled', 't')]

	DT[, xmidS_start := .SD[!is.na(xmidS_start), xmidS_start[1]],
		 by = c(id, year)]
	DT[, xmidA_start := .SD[!is.na(xmidA_start), xmidA_start[1]],
		 by = c(id, year)]


	DT[xmidA_start < xmidS_start, xmidA_start := xmidA_start + 0.3]

	data.table::set(DT, j = c('difS', 'difA'), value = NULL)

	return(DT)
}


#' Estimate model parameters
#'
#' Model estimated parameters for fitting double logistic curve.
#'
#' Arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} allow users to provide either group level or global starting estimates to be used for all models.
#'
#' Either: a character indicating the column name which stores a group level starting parameter (possibly created by \code{\link{model_start}} OR a numeric value used as a global value for all models. See \code{\link{nls}} for more details on starting parameters.
#'
#' Default value for the year column is 'yr'. If you only have one year of data, set to NULL.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' Formula and arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} following this from Bischoff et al. (2012).
#'
#' \deqn{fitted = \frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#' @inheritParams filter_winter
#' @param DT data.table of NDVI time series. Also optionally starting estimates. See Details.
#' @param year year column name. default is 'yr'.
#' @param xmidS starting estimates. see Details. - "spring inflection point"
#' @param xmidA starting estimates. see Details. - "fall inflection point"
#' @param scalS starting estimates. see Details. - "scale parameter for spring green-up portion of the NDVI curve"
#' @param scalA starting estimates. see Details. - "scale parameter for fall dry-down portion of the NDVI curve"
#' @param returns either 'models' or 'columns'. 'models' will return a data.table of model outcomes by id and year. 'columns' will append model estimate parameters to the input DT.
#'
#' @family model
#'
#' @return
#'
#' data.table of model estimated parameters for double logistic model. If any rows are NULL, `nls` could not fit a model given starting parameters to the data provided.
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
#' # Guess starting parameters for xmidS and xmidA
#' model_start(ndvi)
#'
#' # Double logistic model parameters
#' #   given global starting parameters for scalS, scalA
#' #   and output of model_start for xmidS, xmidA
#' mods <- model_params(
#'   ndvi,
#'   returns = 'models',
#'   xmidS = 'xmidS_start',
#'   xmidA = 'xmidA_start',
#'   scalS = 0.05,
#'   scalA = 0.01
#' )
model_params <- function(DT,
												 returns = NULL,
												 id = 'id',
												 year = 'yr',
												 xmidS = NULL,
												 xmidA = NULL,
												 scalS = NULL,
												 scalA = NULL) {
	# NSE errors
	. <- yr <- NULL

	check_truelength(DT)

	check_col(DT, 'scaled', extra = ' - did you filter and scale?')

	check_col(DT, id, 'id')
	check_col(DT, year, 'year')

	if (is.character(xmidS)) {
		check_col(DT, xmidS, 'xmidS')
	}

	if (is.character(xmidA)) {
		check_col(DT, xmidA, 'xmidA')
	}

	if (is.character(scalS)) {
		check_col(DT, scalS, 'scalS')
	}

	if (is.character(scalA)) {
		check_col(DT, scalA, 'scalA')
	}

	if (is.null(returns)) {
		stop('argument "returns" is NULL, must provide one of "models" or "columns"')
	}

	if (is.null(xmidS) | is.null(xmidA) | is.null(scalS) | is.null(scalA)) {
		stop('starting parameters must be provided.
				 either a column name or global value. ')
	}

	params <- list(xmidS = xmidS, xmidA = xmidA,
								 scalS = scalS, scalA = scalA)
	hasit <- params[params %in% colnames(DT)]
	doesnt <- params[!(params %in% colnames(DT))]
	comb <- unique(DT[, .SD, .SDcols = unlist(c(id, year, hasit))])
	comb[, (names(doesnt)) := (doesnt)]
	whichchar <- params[unlist(lapply(params, is.character))]
	setnames(comb,
					 c(id, year, unlist(whichchar)),
					 c('id', 'yr', names(whichchar)))

	if (any(comb[, .(checkdup = .N > 1),
							 by = c(id, year)]$checkdup)) {
		stop('non unique values for id (and year),
				  check duplicate starting parameters')
	}

	m <- mapply(function(i, y) {
		tryCatch({
			key <- as.data.table(setNames(list(i, y), c(id, year)))
			c(setNames(list(i, y), c(id, year)),
				stats::coef(
					stats::nls(
						formula = scaled ~
							(1 / (1 + exp((xmidS - t) / scalS))) -
							(1 / (1 + exp((xmidA - t) / scalA))),
						data = DT[id == i & yr == y],
						start = list(
							xmidS = comb[id == i & yr == y, xmidS],
							xmidA = comb[id == i & yr == y, xmidA],
							scalS = comb[id == i & yr == y, scalS],
							scalA = comb[id == i & yr == y, scalA]
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

	m <- data.table::rbindlist(m, fill = TRUE)

	if (returns == 'models') {
		return(m)
	} else if (returns == 'columns') {
		setnames(m, c('id', 'yr'), c(id, year))

		return(DT[m, c('xmidS', 'xmidA', 'scalS', 'scalA') :=
								.(xmidS, xmidA, scalS, scalA),
							on = c(id, year)])
	}
}


#' Model NDVI time series
#'
#' Fit double logistic model to NDVI time series given parameters estimated with model_params.
#'
#' @param DT data.table of model parameters (output from model_params).
#' @param observed boolean indicating if a full year of fitted values should be returned (observed = FALSE) or if only observed values will be fit (observed = TRUE)
#' @return
#'
#' Model parameter data.table appended with 'fitted' column of double logistic model of NDVI for a full year. Calculated at the daily scale with the following formula from Bischoff et al. (2012).
#'
#' \deqn{fitted = \frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#' (See the "Getting started with irg vignette" for a better formatted formula.)
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
#' # Guess starting parameters for xmidS and xmidA
#' model_start(ndvi)
#'
#' ## Two options: fit to full year or observed data
#' # Option 1 - returns = 'models'
#'
#' # Double logistic model parameters
#' #   given global starting parameters for scalS, scalA
#' #   and output of model_start for xmidS, xmidA
#' mods <- model_params(
#'   ndvi,
#'   returns = 'models',
#'   xmidS = 'xmidS_start',
#'   xmidA = 'xmidA_start',
#'   scalS = 0.05,
#'   scalA = 0.01
#' )
#'
#' # Fit to the whole year (requires assignment)
#' fit <- model_ndvi(mods, observed = FALSE)
#'
#' # Option 2 - returns = 'columns'
#' model_params(
#'   ndvi,
#'   returns = 'columns',
#'   xmidS = 'xmidS_start',
#'   xmidA = 'xmidA_start',
#'   scalS = 0.05,
#'   scalA = 0.01
#' )
#'
#' # Fit double logistic curve to NDVI time series for the observed days
#' model_ndvi(ndvi, observed = TRUE)
#'
model_ndvi <- function(DT, observed = TRUE) {
	# NSE error
	xmidS <- xmidA <- scalS <- scalA <- fitted <- NULL

	check_truelength(DT)

	check_col(DT, 'xmidS')
	check_col(DT, 'xmidA')
	check_col(DT, 'scalS')
	check_col(DT, 'scalA')

	if (observed) {
		DT[, fitted :=
			 	(1 / (1 + exp((xmidS - t) / scalS))) -
			 	(1 / (1 + exp((xmidA - t) / scalA)))]
		return(DT)
	} else if (!observed) {
		fitDT <- DT[rep(1:.N, each = 366)][, t := rep(julseq$t, length.out = .N)]

		fitDT[, fitted :=
						(1 / (1 + exp((xmidS - t) / scalS))) -
						(1 / (1 + exp((xmidA - t) / scalA)))]

		return(fitDT)
	} else{
		stop('missing observed - must be TRUE/FALSE')
	}
}
