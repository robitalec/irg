#' Model parameters
#'
#' Estimated parameters for fitting double logistic curve.
#'
#' Default value for the year column is 'yr'.
#'
#' The id parameter is used to split between sampling units. This may be a point id, polygon id, pixel id, etc. depending on your analysis. This should match the id provided to filtering functions.
#'
#' Arguments xmidS, xmidA, scalS and scalA refer:
#'
#' \deqn{\frac{1}{1 + \exp{\frac{xmidS - t}{scalS}}} - \frac{1}{1 + \exp{\frac{xmidA - t}{scalA}}}}
#'
#'
#' @inheritParams filter_winter
#' @param year year column name. default is 'yr'.
#' @param xmidS
#' @param xmidA
#' @param scalS
#' @param scalA
#'
#' @return
#' @references
#'   \url{https://www.journals.uchicago.edu/doi/abs/10.1086/667590}
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
