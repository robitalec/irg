#' Filter with QA Band
#'
#' Using MODIS QA band information, filter the NDVI time series.
#'
#' @param DT data.table of NDVI time series
#' @param qa QA column. default is 'SummaryQA'.
#' @param good values which correspond to quality pixels. default is 0 and 1.
#' @param ndvi ndvi column name. default is 'NDVI'.
#'
#' @return filtered data.table with appended 'filtered' column of "quality" NDVI.
#' @import data.table
#'
#' @family filter
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
#' filter_qa(ndvi, ndvi = 'NDVI', qa = 'SummaryQA', good = c(0, 1))
filter_qa <-
	function(DT,
					 ndvi = 'NDVI',
					 qa = 'SummaryQA',
					 good = c(0, 1)) {
	# NSE Errors
	NDVI <- filtered <- NULL

	check_truelength(DT)

	if (length(qa) != 1) {
		stop('qa must be length 1')
	}

	check_col(DT, ndvi, 'NDVI')
	check_col(DT, qa, 'qa')

	if (typeof(DT[[ndvi]]) != 'integer') {
		warning('casting ', ndvi, ' column as integer')
		DT[, (ndvi) := as.integer(.SD[[1]]), .SDcols = c(ndvi)]
	}

	DT[, good_bool := .SD[[1]] %in% good, .SDcols = c(qa)]
	DT[(good_bool), filtered := .SD[[1]], .SDcols = c(ndvi)]
	DT[!(good_bool), filtered := NA]
	set(DT, j = 'good_bool', value = NULL)
}


#' Filter winter NDVI
#'
#' Using lower quantile (default = 0.025) of multi-year MODIS data, determine the "winterNDVI" for each id.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis.
#'
#' @inheritParams filter_qa
#' @param probs quantile probability to determine "winterNDVI". default is 0.025.
#' @param limits integer vector indicating limit days of absolute winter (snow cover, etc.). default is c(60, 300): 60 days after Jan 1 and 65 days before Jan 1.
#' @param doy julian day column. default is 'DayOfYear'. integer type.
#' @param id id column. default is 'id'. See details.
#'
#'
#' @return filtered data.table with appended 'winter' column of each id's "winterNDVI" baseline value.
#' @import data.table
#'
#' @export
#'
#' @family filter
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#' filter_qa(ndvi, ndvi = 'NDVI', qa = 'SummaryQA', good = c(0, 1))
#' filter_winter(ndvi, probs = 0.025, limits = c(60L, 300L), doy = 'DayOfYear', id = 'id')
filter_winter <-
	function(DT,
					 probs = 0.025,
					 limits = c(60L, 300L),
					 doy = 'DayOfYear',
					 id = 'id') {
		# NSE Errors
		filtered <- winter <- NULL

		check_truelength(DT)

		if (length(probs) != 1) {
			stop('probs must be length 1')
		}

		check_col(DT, doy, 'doy')
		check_col(DT, id, 'id')
		check_col(DT, 'filtered', extra = ', did you run filter_qa?')
		overwrite_col(DT, 'winter')

		if (typeof(DT[[doy]]) != 'integer') {
			DT[, (doy) := as.integer(.SD[[1]]), .SDcols = doy]
		}

		if (typeof(limits) != 'integer') {
			limits <- as.integer(limits)
		}

		bys <- id

		DT[, winter := as.integer(stats::quantile(filtered,
																			 probs = probs,
																			 na.rm = TRUE)),
			 by = bys]

		DT[filtered < winter, filtered := winter]

		DT[get(doy) <= limits[1] | get(doy) >= limits[2],
			 filtered := winter]
}


#' Filter with rolling median
#'
#' Using a rolling median, filter the NDVI time series for each id.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis.
#'
#' @inheritParams filter_qa
#' @inheritParams filter_winter
#' @param method median. no other options yet. let me know if you are looking for something else.
#' @param window window size. default is 3.
#'
#' @return filtered data.table with appended 'rolled' column of each id's rolling median, filtered NDVI time series.
#' @import data.table
#'
#' @export
#'
#' @family filter
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' filter_qa(ndvi, ndvi = 'NDVI', qa = 'SummaryQA', good = c(0, 1))
#' filter_winter(ndvi, probs = 0.025, limits = c(60L, 300L), doy = 'DayOfYear', id = 'id')
#' filter_roll(ndvi, window = 3L, id = 'id')
filter_roll <-
	function(DT,
					 window = 3L,
					 id = 'id',
					 method = 'median'
					 ) {
		# NSE Errors
		filtered <- winter <- rolled <- NULL

		check_truelength(DT)
		check_col(DT, id, 'id')
		check_col(DT, 'filtered', extra = ', did you run filter_qa?')
		check_col(DT, 'winter', extra = ', did you run filter_winter?')
		overwrite_col(DT, 'rolled')

		bys <- id

		DT[, rolled :=
			 	as.integer(RcppRoll::roll_median(filtered, n = 3,
			 																	 fill = -3000L)),
			 by = bys]
		DT[rolled == -3000, rolled := winter]
	}

#' Filter top NDVI
#'
#' Using upper quantile (default = 0.925) of multi-year MODIS data, determine the top NDVI for each id.
#'
#' The id argument is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis.
#'
#' @inheritParams filter_winter
#' @param probs quantile probability to determine top. default is 0.925.
#'
#' @return filtered data.table with appended 'top' column of each id's top (quantile) NDVI value.
#' @import data.table
#'
#' @export
#'
#' @family filter
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
#'
#' filter_qa(ndvi, ndvi = 'NDVI', qa = 'SummaryQA', good = c(0, 1))
#' filter_winter(ndvi, probs = 0.025, limits = c(60L, 300L), doy = 'DayOfYear', id = 'id')
#' filter_roll(ndvi, window = 3L, id = 'id')
#' filter_top(ndvi, probs = 0.925, id = 'id')
filter_top <-
	function(DT,
					 probs = 0.925,
					 id = 'id') {
		# NSE Errors
		top <- filtered <- NULL

		check_truelength(DT)


		if (length(probs) != 1) {
			stop('probs must be length 1')
		}

		check_col(DT, id, 'id')
		check_col(DT, 'filtered', extra = ', did you run filter_qa?')
		check_col(DT, 'winter', extra = ', did you run filter_winter?')
		overwrite_col(DT, 'top')

		bys <- id

		DT[, top := stats::quantile(filtered, probs, na.rm = TRUE),
			 by = bys]

	}


#' Filter NDVI
#'
#' Meta function, calling all filtering steps, in order. Only defaults.
#'
#' @inheritParams filter_qa

#' @return filtered NDVI time series.
#'
#' @import data.table
#'
#' @family filter
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
#' # Use filter_ndvi to apply all filtering steps (with defaults)
#' filter_ndvi(ndvi)
filter_ndvi <-
	function(DT) {

		check_truelength(DT)

		overwrite_col(DT, 'filtered')
		overwrite_col(DT, 'winter')
		overwrite_col(DT, 'rolled')
		overwrite_col(DT, 'top')

		filter_qa(DT)
		filter_winter(DT)
		filter_roll(DT)
		filter_top(DT)
	}
