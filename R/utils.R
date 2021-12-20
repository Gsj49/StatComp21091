#' @useDynLib StatComp21091
#' @importFrom Rcpp sourceCpp
NULL
#> NULL


#' Model Averaging MArginal Regression for single step timeseries forecasting
#'
#' @param ts timeseries to be forecast
#' @param num_lags the number of models to apply model averaging
#' @param num_samples the number of samples that is used in the whole process
#' @param step_forward the number of steps forward to forecast
#' @param use_soft_max whether to add a softmax layer to final weights
#' @param bandwidth bandwidth of estimate method
#' @param kertype the type of kernel, gaussian kernel is used by default
#'
#' @return single step forecasting result
#' @importFrom stats ksmooth
#' @export
#'
#' @examples
#' \dontrun{
#' forecast <- MamarSingleStepforecast(ts=ts, num_lags=3, num_samples=50,step_forward=1)
#' }
#'
MamarSingleStepforecast <- function(ts, num_lags, num_samples, step_forward=1,use_soft_max=F, bandwidth=0.5, kertype="normal"){
  len_ts <-  length(ts)
  enough_len <- T
  min_num_samples <- num_samples

  split_sample <- function(gap){
    x <- vector()
    y <- vector()
    i <- len_ts
    while (i-gap >= 1 && len_ts-i+1<= num_samples) {
      y = append(y,ts[i])
      x = append(x,ts[i-gap])
      i = i-1
    }
    if(i-gap==0 && len_ts-i+1<= num_samples){
      print("length of ts is not enough for num_samples samples")
      enough_len <- F
      min_num_samples = min(length(x),min_num_samples)
    }
    return(list(x=x,y=y))
  }

  kernel_model_set = list()
  n = min_num_samples
  X = matrix(0,n,num_lags)
  for (i in 1:n) {
    X[i,] = ts[(len_ts-step_forward-i+1) : (len_ts-step_forward-(num_lags-1)-i+1)]
  }
  Y = ts[len_ts:(len_ts-n+1)]

  for (gap in 1:num_lags) {
    temp = split_sample(gap+step_forward-1)
    exog = temp$x
    endog = temp$y
    kernel_model = ksmooth(x=exog,y=endog,kernel=kertype,bandwidth=bandwidth,x.points=c(exog,ts[len_ts-gap+1]))
    kernel_model_set = append(kernel_model_set,kernel_model[2])
  }
  M = matrix(0,n,num_lags)
  for(j in 1:num_lags){
    M[,j] = kernel_model_set[j]$y[1:n]
  }

  weight = tryCatch({
    solve(t(M) %*% M) %*% t(M) %*% Y
  }, error = function(e) {
    weight = rep(1/num_lags,num_lags)
  })

  if(use_soft_max){
    weight = exp(weight)/sum(exp(weight))
  }
  pred = vector()
  for(j in 1:num_lags){
    pred = append(pred,kernel_model_set[j]$y[n+1])
  }

  weight_pred = t(weight) %*% pred
  return(weight_pred)
}


#' Model Averaging MArginal Regression for multi-step timeseries forecasting
#'
#' @param ts timeseries to be forecast
#' @param num_lags the number of models to apply model averaging
#' @param num_samples the number of samples that is used in the whole process
#' @param max_forward_step the maximun step need to forecast
#' @param use_soft_max whether to add a softmax layer to final weights
#' @param bandwidth bandwidth of estimate method
#' @param kertype the type of kernel, gaussian kernel is used by default
#'
#' @return mulit step forecasting result
#' @importFrom stats ksmooth
#' @export
#'
#' @examples
#' \dontrun{
#' forecast <- MamarMultiStepforecast(ts=ts, num_lags=3, num_samples=50, max_forward_step=7)
#' }
MamarMultiStepforecast <- function(ts, num_lags, num_samples, max_forward_step=4, use_soft_max=F, bandwidth=0.5, kertype="normal"){
  res <- vector()
  for(t in 1:max_forward_step){
    res[t] = MamarSingleStepforecast(ts=ts, num_lags=num_lags, num_samples=num_samples, step_forward=t,use_soft_max=use_soft_max, bandwidth=bandwidth, kertype=kertype)
  }
  return(res)
}
