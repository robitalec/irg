#' Raw NDVI Time Series
#'
#' A data.table containing NDVI samples for ten points over ten years (2002-2012).
#'
#' @format A data.table with 2530 rows and 5 variables:
#' \itemize{
#'   \item id - individual identifier
#'   \item yr - year of sample
#'   \item DayOfYear - julian day/day of year of sample
#'   \item NDVI - sampled value
#'   \item SummaryQA - Summary quality assessment value
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
#' @name ndvi
#'
#' @examples
#' # Load data.table
#' library(data.table)
#'
#' # Read example data
#' ndvi <- fread(system.file("extdata", "ndvi.csv", package = "irg"))
NULL
