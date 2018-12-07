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
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' filter_ndvi(ndvi)
#'
#' scale_ndvi(ndvi)
scale_ndvi <-
	function(DT) {
		# NSE Errors
		rolled <- winter <- top <- NULL

		if (all(c('rolled', 'winter', 'top') %in% colnames(DT))) {
			stop('missing one of "rolled", "winter", "top". did you filter?')
		}

		DT[, scaled := ((rolled - winter) / (top - winter))]

		DT[rolled > top, scaled := 1]
	}


