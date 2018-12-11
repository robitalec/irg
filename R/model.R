#' Model parameters
#'
#' Estimated parameters for fitting double logistic curve.
#'
#' Arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} allow users to provide global starting estimates to be used for all models. Alternatively, leave NULL and provide those arguments as columns (with matching names), to provide starting estimates for each id and year.
#'
#' Default value for the year column is 'yr'. If you only have one year of data, set to NULL.
#'
#' The id parameter is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' Formula and arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} following this from Bischoff et al. (2012).
#'
#' \deqn{\frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#' @inheritParams filter_winter
#' @param year year column name. default is 'yr'.
#' @param xmidS global starting estimates. see Details. - "spring inflection point"
#' @param xmidA global starting estimates. see Details. - "fall inflection point"
#' @param scalS global starting estimates. see Details. - "scale parameter for spring green-up portion of the NDVI curve"
#' @param scalA global starting estimates. see Details. - "scale parameter for fall dry-down portion of the NDVI curve"
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

	# are there >1 unique xmidS, etc in an id, year?
	# put it all into the comb?

	comb <- unique(
		DT[, .SD, .SDcols =
			 	c(id, year, intersect(colnames(DT),
			 												c('xmidS', 'xmidA', 'scalS', 'scalA')))])

	if (any(comb[, .(checkdup = .N > 1), by = c(id, year)]$checkdup)) {
		stop('non unique values for id (and year),
				  check duplicate starting parameters')
	}

	m <- mapply(function(i, y) {
		tryCatch(
			c(list(id = i, yr = y),
				coef(
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

	rbindlist(m, fill = TRUE)
}


#' Model NDVI time series
#'
#' Fit double logistic model to NDVI time series given parameters estimated with fit_params
#'
#' @param DT
#'
#' @return
#' @export
#'
#' @examples
model_ndvi <- function(DT) {

}
