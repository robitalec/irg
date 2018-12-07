#' Scale NDVI
#'
#' Using filtered NDVI time series, scale it to 0-1
#'
#' @inheritParams filter_qa
#'
#' @return data.table with appended 'scaled' column of 0-1 scaled NDVI.
#' @import data.table
#'
#' @export
#'
#' @family scale
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' filter_ndvi(ndvi)
#' scale_ndvi(ndvi)
scale_ndvi <-
	function(DT) {
		# NSE Errors
		rolled <- winter <- top <- scaled <- NULL

		if (any(!c('rolled', 'winter', 'top') %in% colnames(DT))) {
			stop('missing one of "rolled", "winter", "top". did you filter?')
		}

		DT[, scaled := ((rolled - winter) / (top - winter))]

		DT[rolled > top, scaled := 1]
	}


#' Scale DOY
#'
#' Scale the day of the year to 0-1 (like NDVI).
#'
#' @inheritParams filter_winter
#'
#' @return data.table with appended 'scaled' column of 0-1 scaled NDVI.
#' @import data.table
#'
#' @export
#'
#' @family scale
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' scale_doy(ndvi, doy = 'DayOfYear')
scale_doy <-
	function(DT, doy = 'DayOfYear') {
		# NSE Errors
		t <- NULL

		if (!(doy %in% colnames(DT))) {
			stop('doy column not found in DT')
		}

		jul01 <- data.table(jul = 1:366,
												t = seq(0, 1, length.out = 366))
		DT[, t := jul01$t[.SD][[1]], .SDcols = doy]
	}
