#' check col
#' @param DT data.table
#' @param col column name
#' @param arg argument name
#' @param extra extras
check_col <- function(DT = NULL, col = NULL, arg = NULL, extra = NULL) {

	if (is.null(arg)) {
		it <- col
	} else {
		it <- arg
	}

	if (!(col %in% colnames(DT))) {
		stop(paste0(it, ' column not found in DT', extra))
	}
}

#' check type
#' @param DT data.table
#' @param col column name
#' @param type
check_type <- function(DT = NULL, col = NULL, type = NULL) {
	if (!(typeof(DT[[col]]) %in% type)) {
		stop(paste0(col, ' does not match required type: ', type))
	}
}


#' overwite_col
#' @param DT data.table
#' @param col column name
overwrite_col <- function(DT = NULL, col = NULL) {

	if (col %in% colnames(DT)) {
		warning(paste0('overwriting ', col, ' column'))
		set(DT, j = eval(col), value = NULL)
	}
}

#' check_truelength
#' @param DT data.table
check_truelength <- function(DT) {
	if (truelength(DT) == 0) {
		stop('please run data.table::alloc.col on your DT to allocate columns')
	}
}
