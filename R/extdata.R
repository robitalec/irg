#' Raw Landsat NDVI data
#'
#' A data.table containing NDVI samples for seven points over ten years (2005-2010).
#' Data extracted using Earth Engine with the example script provided by the
#' `use_example_ee_script()` function with sensor set to 'Landsat'.
#'
#' Note: these are the same locations as in the example 'MODIS' data.
#'
#' @format A data.table with 1652 rows and 5 variables:
#' \itemize{
#'   \item id - individual identifier
#'   \item ndvi - sampled NDVI value
#'   \item mask - mask value, see details below
#'   \item doy - julian day/day of year of sample
#'   \item year - year of sample
#' }
#'
#' mask details:
#'
#' \itemize{
#'   \item 0 - Good data
#'   \item 1 - if QA_PIXEL indicates unwanted pixels OR if QA_RADSAT indicates saturated pixels
#'   \item 2 - if QA_PIXEL indicates unwanted pixels AND if QA_RADSAT indicates saturated pixels
#'   }
#'
#' @name sampled-ndvi-Landsat-LC08-T1-L2.csv
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file('extdata', 'sampled-ndvi-Landsat-LC08-T1-L2.csv', package = 'irg'))
NULL



#' Raw MODIS MOD13Q1 NDVI data
#'
#' A data.table containing NDVI samples for seven points over ten years (2005-2010).
#' Data extracted using Earth Engine with the example script provided by the
#' `use_example_ee_script()` function with sensor set to 'MODIS'.
#'
#' Note: these are the same locations as in the example 'Landsat' data.
#'
#' @format A data.table with 805 rows and 5 variables:
#' \itemize{
#'   \item id - individual identifier
#'   \item NDVI - sampled value
#'   \item SummaryQA - Summary quality assessment value, see details below
#'   \item DayOfYear - julian day/day of year of sample
#'   \item yr - year of sample
#' }
#'
#' SummaryQA details:
#'
#' \itemize{
#'   \item 0 - Good data, use with confidence
#'   \item 1 - Marginal data, useful but look at detailed QA for more information
#'   \item 2 - Pixel covered with snow/ice
#'   \item 3 - Pixel is cloudy
#'   }
#'
#' @name sampled-ndvi-MODIS-MOD13Q1.csv
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file('extdata', 'sampled-ndvi-MODIS-MOD13Q1.csv', package = 'irg'))
NULL
