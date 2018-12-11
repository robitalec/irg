#' Model parameters
#'
#' Estimated parameters for fitting double logistic curve.
#'
#' Default value for the year column is 'yr'. If you only have one year of data, set to NULL.
#'
#' The id parameter is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' Formula and arguments \code{xmidS}, \code{xmidA}, \code{scalS}, \code{scalA} following this from Bischoff et al. (2012).
#'
#' \deqn{\frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#'
#' @inheritParams filter_winter
#' @param year year column name. default is 'yr'.
#' @param xmidS "spring inflection point"
#' @param xmidA "fall inflection point"
#' @param scalS "scale parameter for spring green-up portion of the NDVI curve"
#' @param scalA "scale parameter for fall dry-down portion of the NDVI curve"
#'
#' @return
#'
#' data.table of model estimated parameters for double logistic model
#'
#' @references
#'   \url{https://www.journals.uchicago.edu/doi/abs/10.1086/667590}
#'
#'
#' @export
#'
#' @examples
model_params <- function(DT) {

}


#' Model NDVI time series
#'
#' Fit double logistic model to NDVI time series given parameters estimated with fit_params
#'
#' @param DT
#'
#' @return
#' @export
#'
#' @examples
model_ndvi <- function(DT) {

}
