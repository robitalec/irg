#' Filter with QA Band
#'
#' Using MODIS QA band information, filter the NDVI time series.
#'
#' @param DT data.table of NDVI time series
#' @param qa QA column. default is 'SummaryQA'.
#' @param good values which correspond to quality pixels. default is 0 and 1.
#'
#' @return filtered data.table with appended
#' @import data.table
#'
#' @export
#'
#' @examples
#' filter_qa(DT, qa = 'SummaryQA', good = c(0, 1))
filter_qa <- function(DT, qa = 'SummaryQA', good = c(0, 1)) {
	# NSE Errors
	NDVI <- NULL

	if (length(qa) != 1) {
		stop('qa must be length 1')
	}

	if (!('NDVI' %in% colnames(DT))) {
		stop('NDVI column not found in DT')
	}

	if (!(qa %in% colnames(DT))) {
		stop('QA column not found in DT')
	}

	DT[get(qa) %in% good, good := TRUE][is.na(good), good := FALSE]
	DT[(good), filtered := NDVI]
	DT[!(good), filtered := NA]
	set(DT, j = 'good', value = NULL)
}


#' Filter winter NDVI
#'
#' Using lower 0.025 quantile of multi-year MODIS data, determine the "winterNDVI" for each id.
#'
#' The id parameter is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis.
#'
#' @inheritParams filter_qa
#' @param prob quantile probability to determine winterNDVI. default is 0.025.
#' @param winter days indicating absolute winter.
#' @param doy julian day column. default is 'DayOfYear'.
#' @param id id column. default is 'id'. See details.
#'
#'
#' @return filtered data.table with appended 'winter' column of each id's "winterNDVI" baseline value.
#' @import data.table
#'
#' @export
#'
#' @examples
#' filter_winter(DT, qa = 'SummaryQA', good = c(0, 1))
filter_winter <-
	function(DT,
					 probs = 0.025,
					 days = c(60, 300),
					 doy = 'DayOfYear',
					 id = 'id') {
		# NSE Errors
		filtered <- NULL

		if (length(probs) != 1) {
			stop('probs must be length 1')
		}

		if (!(doy %in% colnames(DT))) {
			stop('doy column not found in DT')
		}

		if (!(id %in% colnames(DT))) {
			stop('id column not found in DT')
		}

		if (!('filtered' %in% colnames(DT))) {
			stop('filtered column not found in DT, did you run filter_qa?')
		}



		DT[, winter := quantile(filtered, probs = 0.025, na.rm = TRUE),
			 by = id]

		# If below winter, set to winter
		DT[filtered < winter, filtered := winter]

		# If between those dates, set to winter value
		DT[DayOfYear <= 60 | DayOfYear >= 300, filtered := winter]


	}
