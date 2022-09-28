#' Use an example sampling script for Earth Engine
#'
#' Provides an example script for use in Earth Engine, as a preceeding step
#' to using the `irg` package. Use the script to sample NDVI in Earth
#' Engine, then use the `irg` package to calculate the instantaneous rate of green-up.
#'
#' @param sensor either 'MODIS' or 'Landsat'
#' @param filepath file path relative to current working director, indicating
#' where to save the example script. default is NULL, simply printing lines
#' to the console.
#' @param overwrite boolean indicating if the file should overwrite existing
#' files. default is FALSE.
#'
#' @return
#'
#' `use_example_ee_script` prints an example NDVI extraction script or
#' if `filepath` is provided, saves it at the location specified.
#'
#'
#' @export
#'
#' @examples
#' library(irg)
#' use_example_ee_script(sensor = 'MODIS')
use_example_ee_script <- function(sensor = 'MODIS',
																	filepath = NULL,
																	overwrite = FALSE) {
	if (sensor == 'MODIS') {
		source_file <- system.file('javascript', 'Sample-NDVI-For-IRG-MODIS.js',
															 package = 'irg')

		if (is.null(filepath)) {
			writeLines(readLines(source_file))
		} else {
			if (!is.null(filepath) && file.exists(filepath) && !overwrite) {
				stop('found file at ', filepath,
						 '\n set overwrite to TRUE if you would like to save there anyways')
			}

			result <- file.copy(from = source_file, to = filepath, overwrite = overwrite)

			if (result) {
				message('Example script saved to: ', '"', filepath, '"')
			}
		}
	} else if (sensor == 'Landsat') {
		source_file <- system.file('javascript', 'Sample-NDVI-For-IRG-Landsat.js',
															 package = 'irg')

		if (is.null(filepath)) {
			writeLines(readLines(source_file))
		} else {
			if (!is.null(filepath) && file.exists(filepath) && !overwrite) {
				stop('found file at ', filepath,
						 '\n set overwrite to TRUE if you would like to save there anyways')
			}

			result <- file.copy(from = source_file, to = filepath, overwrite = overwrite)

			if (result) {
				message('Example script saved to: ', '"', filepath, '"')
			}
		}



	} else {
		stop('sensor must be one of: "MODIS" or "Landsat"')
	}
}
