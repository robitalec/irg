## overwite_col
## @param DT data.table
## @param col column name
## @keywords internal
overwrite_col <- function(DT = NULL, col = NULL) {
	if (col %in% colnames(DT)) {
		warning(paste0('overwriting ', col, ' column'))
		set(DT, j = eval(col), value = NULL)
	}
}

## check_truelength
## @param DT data.table
## @keywords internal
check_truelength <- function(DT) {
	if (truelength(DT) == 0) {
		stop('please run data.table::setalloccol on your DT to allocate columns')
	}
}
