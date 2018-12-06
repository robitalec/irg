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
