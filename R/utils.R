#' @useDynLib StatComp21091
#' @importFrom Rcpp sourceCpp
NULL
#> NULL

#' Model Averaging MArginal Regression for timeseries forecasting
#'
#' @param ts array, timeseries to be forecast
#' @param num_lags int,
#' @param reg_type type of regression
#' @param num_samples int, the number of samples that is used in the whole process
#' @param step_forward how many steps forward to forecast
#' @param use_soft_max whether to use softmax
#' @param bw bandwidth, estimate method will be used by default
#' @param ckertype the type of kernel
#'
#' @return the forecasting result
#' @export
#'
#' @examples
mamar <- function(ts, num_lags, reg_type, num_samples, step_forward=1, use_soft_max=False, bw=None, ckertype="gaussian"){
  return(0)
}
