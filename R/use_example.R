#' Use an example sampling script for Earth Engine
#'
#' Provides an example script for use in Earth Engine, as a preceeding step
#' to using the `irg` package. Use the script to sample NDVI in Earth
#' Engine, then use the `irg` package to calculate the instantaneous rate of green-up.
#'
#' @param sensor either 'MODIS' or 'Landsat'
#' @param filename file name indicating where to save the example script.
#' Default is './Sample-NDVI-For-IRG-MODIS.js' if sensor is 'MODIS' or
#' 'Sample-NDVI-For-IRG-Landsat.js' if sensor is 'Landsat'.
#' @param overwrite boolean indicating if the file should overwrite existing
#' files. default is FALSE.
#'
#' @return
#'
#' `use_example_ee_script` saves an example NDVI extraction script in the
#' current working directory using a default filename,
#' or if `filename` is provided, at the location specified.
#'
#'
#' @export
#'
#' @examples
use_example_ee_script <- function(sensor = 'MODIS',
																	filename = NULL,
																	overwrite = FALSE) {
	if (sensor == 'MODIS') {
		if (is.null(filename)) {
			filename <- 'Sample-NDVI-For-IRG-MODIS.js'
		}

		if (!is.null(filename) && file.exists(filename) && !overwrite) {
			stop('found file at ', filename,
					 '\n set overwrite to TRUE if you would like to save there anyways')
		}

		source_file <- system.file('javascript', 'Sample-NDVI-For-IRG-MODIS.js',
															 package = 'irg')
	} else if (sensor == 'Landsat') {
		if (is.null(filename)) {
			filename <- 'Sample-NDVI-For-IRG-Landsat.js'
		}

		if (!is.null(filename) && file.exists(filename) && !overwrite) {
			stop('found file at ', filename,
					 '\n set overwrite to TRUE if you would like to save there anyways')
		}

		source_file <- system.file('javascript', 'Sample-NDVI-For-IRG-Landsat.js',
															 package = 'irg')
	} else {
		stop('sensor must be one of: "MODIS" or "Landsat"')
	}

  result <- file.copy(from = source_file, to = filename, overwrite = overwrite)

  if (result) {
  	message('Example script saved to: ', '"', filename, '"')
  }

}
