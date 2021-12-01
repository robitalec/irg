#' Use an example MODIS MOD13Q1 sampling script for Earth Engine
#'
#' Provides an example script for use in Earth Engine, as a preceeding step
#' to using the `irg` package. Sample NDVI with MODIS or Landsat, then
#' use the `irg` package to calculate the instantaneous rate of greenup.
#'
#' @param sensor either 'MODIS' or 'Landsat'
#' @param filename file name indicating where to save the example script.
#' Default is "./Sample-NDVI-For-IRG-MODIS.js"
#' @param overwrite
#'
#' @return
#'
#' `use_example_ee_script` saves an example NDVI extraction script in the
#' current working directory, or if `filename` is provided, at the location
#' provided.
#'
#'
#' @export
#'
#' @examples
use_example_ee_script <- function(sensor = 'MODIS',
																	filename = NULL, overwrite = FALSE) {


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

  file.copy(from = source_file, to = filename, overwrite = overwrite)
}
