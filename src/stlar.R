library(forecast)
# forecasting using stl-ar method

stlar <- function(x, h=10, s.window=11,robust=FALSE,modelfunction=ar,specmethod="psd")
{
  if(!is.ts(x))
    x <- ts(x, frequency=findfrequency(x))
  if(frequency(x)==1 | length(x) <= 2*frequency(x))
    return(forecast(auto.arima(x, max.q=0), h=h))
  
  fit <- stlm(x,s.window=s.window, robust=robust, modelfunction=modelfunction)
  fc <- forecast(fit, h=h)
  return(fc)
}
