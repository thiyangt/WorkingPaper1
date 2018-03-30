rank_yearly <- function(predictions, tsList, h){ # h=6
  
  total_ts <- length(tsList)
  mase <- matrix(NA, total_ts, h)
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    test <- tsList[[i]]$xx
    m <- frequency(training)
    
    if (predictions[i] == "ARIMA") {
      fit_arima <- auto.arima(training)
      fcast <- forecast(fit_arima,h)$mean
    } else if (predictions[i] == "ARMA/AR/MA") { 
      fit_arma <- auto.arima(training,d=0, stationary=TRUE)
      fcast <- forecast(fit_arma,h)$mean
    } else if (predictions[i] == "ETS-dampedtrend") { 
      fit_ets <- ets(training, model= "ZZN", damped = TRUE)
      fcast <- forecast(fit_ets,h)$mean
    } else if (predictions[i] == "ETS-notrend") { 
      fit_ets <- ets(training, model= "ZNN", damped = FALSE)
      fcast <- forecast(fit_ets,h)$mean
    } else if (predictions[i] == "ETS-trend") { 
      fit_ets <- ets(training, model= "ZZN", damped = FALSE)
      fcast <- forecast(fit_ets,h)$mean
    } else if (predictions[i] == "Random Walk") { 
      fit_rw <- rwf(training, drift = FALSE)
      fcast <- forecast(fit_rw,h)$mean
    } else if (predictions[i] == "Random Walk with Drift") { 
      fit_rwd <- rwf(training, drift = TRUE)
      fcast <- forecast(fit_rwd,h)$mean[1:h]
    } else if (predictions[i] == "STL-AR") { 
      STLAR <- stlar(training,h=h)
      fcast <- forecast(STLAR,h)$mean
    } else if (predictions[i] == "Theta") {
      if (m > 1){
        # using stheta method with seasonal adjustment
        # require(forecTheta)
        fitsTheta <- stheta(training,h=h, s='additive')
        fcast <- fitsTheta$mean
      }else{
        # using thetaf method
        fitTheta <-thetaf(training,h=h)
        fcast <- fitTheta$mean
      }
    } else {
      fit_wn <- Arima(training,order=c(0,0,0))
      fcast <- forecast(fit_wn,h)$mean
    }
  
    mase[i,1:h] <- abs(fcast-test)/mean(abs(diff(training)))
  }
  
  colMeans(mase,na.rm=TRUE)
}

#--------------------------------------------------------------
rank_yearly_benchmark <- function(tsList, h){
  
  total_ts <- length(tsList)
  mase_arima <- matrix(NA, total_ts, h)
  mase_ets <- matrix(NA, total_ts, h)
  mase_wn <- matrix(NA, total_ts, h)
  mase_rw <- matrix(NA, total_ts, h)
  mase_rwd <- matrix(NA, total_ts, h)  
  mase_stlar <- matrix(NA, total_ts, h)
  mase_theta <- matrix(NA, total_ts, h)
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    test <- tsList[[i]]$xx
    m <- frequency(training)  
    
    fit_arima <- auto.arima(training)
    fcast_arima <- forecast(fit_arima,h)$mean
    mase_arima[i,1:h] <- abs(fcast_arima-test)/mean(abs(diff(training)))
  
    fit_ets <- ets(training)
    fcast_ets <- forecast(fit_ets,h)$mean
    mase_ets[i,1:h] <- abs(fcast_ets-test)/mean(abs(diff(training)))
  }
  
  return(list(colMeans(mase_arima), colMeans(mase_ets)))
}
