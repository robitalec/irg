#' Scale NDVI
#'
#' Using filtered NDVI time series, scale it to 0-1.
#'
#' This functions expects the input \code{DT} is the output of previous four filtering steps, or \code{filter_ndvi}.
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
#' # Read in example data
#' ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))
#'
#' # Filter and scale NDVI time series
#' filter_ndvi(ndvi)
#' scale_ndvi(ndvi)
scale_ndvi <- function(DT) {
	# NSE Errors
	rolled <- winter <- top <- scaled <- NULL

	check_truelength(DT)

	chk::check_names(DT, 'filtered')
	chk::check_names(DT, 'winter')
	chk::check_names(DT, 'top')

	DT[, scaled := ((rolled - winter) / (top - winter))]

	DT[rolled > top, scaled := 1]
	DT[between(scaled, 0, -0.01), scaled := 0]
	return(DT)
}


#' Scale DOY
#'
#' Scale the day of the year to 0-1 (like NDVI).
#'
#' @inheritParams filter_winter
#'
#' @return data.table with appended 't' column of 0-1 scaled day of year.
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
#' # Read in example data
#' ndvi <- fread(system.file("extdata", "sampled-ndvi-MODIS-MOD13Q1.csv", package = "irg"))
#'
#' # Scale DOY
#' scale_doy(ndvi)
scale_doy <- function(DT, doy = 'DayOfYear') {
	# NSE Errors
	t <- NULL

	check_truelength(DT)
	chk::check_names(DT, doy)
	overwrite_col(DT, 't')

	DT[, t := seq(0, 1, length.out = 366)[.SD[[1]]], .SDcols = c(doy)]
	return(DT)
}
