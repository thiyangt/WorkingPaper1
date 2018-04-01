rank_yearly_rf <- function(predictions, tsList, h){ # h=6
  
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


#-----------------------------------------------------------

rank_seasonal_rf <- function(predictions, tsList, h, m){ # h=8 for quaterly and h=18 monthly
  # m is frequency
  total_ts <- length(tsList)
  mase <- matrix(NA, total_ts, h)
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    test    <- tsList[[i]]$xx

    if (predictions[i] == "ARIMA") {
      fit_arima <- auto.arima(training, seasonal = FALSE)
      fcast <- forecast(fit_arima,h)$mean

    } else if (predictions[i] == "ARMA/AR/MA") { 
      fit_arma <- auto.arima(training,d=0, stationary=TRUE, seasonal = FALSE)
      fcast <- forecast(fit_arma,h)$mean

    }else if (predictions[i] == "SARIMA") { 
      fit_sarima <- auto.arima(training, seasonal=TRUE)
      fcast <- forecast(fit_sarima,h)$mean

    }  else if (predictions[i] == "ETS-dampedtrend") { 
      fit_ets <- ets(training, model= "ZZN", damped = TRUE)
      fcast <- forecast(fit_ets,h)$mean

    } else if (predictions[i] == "ETS-notrendnoseasonal") { 
      fit_ets <- ets(training, model= "ZNN", damped = FALSE)
      fcast <- forecast(fit_ets,h)$mean

    } else if (predictions[i] == "ETS-trend") { 
      fit_ets <- ets(training, model= "ZZN", damped = FALSE)
      fcast <- forecast(fit_ets,h)$mean

    } else if (predictions[i] == "ETS-trendseasonal") { 
      fit_ets <- ets(training, model= "ZZZ", damped = FALSE)
      fcast <- forecast(fit_ets,h)$mean

    }else if (predictions[i] == "ETS-dampedtrendseasonal") { 
      fit_ets <- ets(training, model= "ZZZ", damped = TRUE)
      fcast <- forecast(fit_ets,h)$mean

    }else if (predictions[i] == "ETS-seasonal") { 
      fit_ets <- ets(training, model= "ZNZ")
      fcast <- forecast(fit_ets,h)$mean

    }else if (predictions[i] == "SNAIVE") { 
      fit_snaive <- snaive(training, h=h)
      fcast <- forecast(fit_snaive, h)$mean
      
    }else if (predictions[i] == "RW") { 
      fit_rw <- rwf(training, drift = FALSE, h=h)
      fcast <- forecast(fit_rw,h)$mean

    } else if (predictions[i] == "RWD") { 
      fit_rwd <- rwf(training, drift = TRUE, h=h)
      fcast <- forecast(fit_rwd,h)$mean

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
    
      mase[i,1:h] <- abs(fcast-test)/mean(abs(diff(training, lag=m)))
  }
  
  colMeans(mase,na.rm=TRUE)
}

#-----------------------------------------------------------------------

rank_benchmark <- function(tsList, h, m){ # for quarterly=4, monthly=12
  
  total_ts <- length(tsList)
  mase_arima <- matrix(NA, total_ts, h)
  mase_ets <- matrix(NA, total_ts, h)
  mase_wn <- matrix(NA, total_ts, h)
  mase_rw <- matrix(NA, total_ts, h)
  mase_rwd <- matrix(NA, total_ts, h)  
  mase_stlar <- matrix(NA, total_ts, h)
  mase_theta <- matrix(NA, total_ts, h)

  if (m==1){Nrow=7
  } else{
    Nrow=8
    mase_snaive <- matrix(NA, total_ts, h)}
  
  mase <- matrix(NA, Nrow, h)
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    test <- tsList[[i]]$xx

    fit_arima <- auto.arima(training)
    fcast_arima <- forecast(fit_arima,h)$mean
    mase_arima[i,1:h] <- abs(fcast_arima-test)/mean(abs(diff(training, lag=m)))
    
    fit_ets <- ets(training)
    fcast_ets <- forecast(fit_ets,h)$mean
    mase_ets[i,1:h] <- abs(fcast_ets-test)/mean(abs(diff(training, lag=m)))
    
    fit_wn <- Arima(training,order=c(0,0,0))
    fcast_wn <- forecast(fit_wn,h)$mean
    mase_wn[i,1:h] <- abs(fcast_wn-test)/mean(abs(diff(training, lag=m)))
    
    fit_rw <- rwf(training, drift = FALSE, h=h)
    fcast_rw <- forecast(fit_rw,h)$mean
    mase_rw[i,1:h] <- abs(fcast_rw-test)/mean(abs(diff(training, lag=m)))
    
    fit_rwd <- rwf(training, drift = TRUE, h=h)
    fcast_rwd <- forecast(fit_rwd,h)$mean[1:h]
    mase_rwd[i,1:h] <- abs(fcast_rwd-test)/mean(abs(diff(training, lag=m)))
    
    STLAR <- stlar(training,h=h)
    fcast_stlar <- forecast(STLAR,h)$mean
    mase_stlar[i,1:h] <- abs(fcast_stlar-test)/mean(abs(diff(training, lag=m)))
    

    if (m > 1){
      # using stheta method with seasonal adjustment
      # require(forecTheta)
      fitsTheta <- stheta(training,h=h, s='additive')
      fcast_stheta <- fitsTheta$mean
      mase_theta[i,1:h] <- abs(fcast_stheta-test)/mean(abs(diff(training, lag=m))) 
    }else{
      # using thetaf method
      fitTheta <-thetaf(training,h=h)
      fcast_theta <- fitTheta$mean
      mase_theta[i,1:h] <- abs(fcast_theta-test)/mean(abs(diff(training, lag=m))) 
    }
    
    if (m>1){
    fitSnaive <-snaive(training,h=h)
    fcast_snaive <- forecast(fitSnaive, h)$mean
    mase_snaive[i,1:h] <- abs(fcast_snaive-test)/mean(abs(diff(training, lag=m)))
    }
    
  }
  
  mase[1,] <- colMeans(mase_arima)
  mase[2,] <- colMeans(mase_ets)
  mase[3,] <- colMeans(mase_wn)
  mase[4,] <- colMeans(mase_rw)
  mase[5,] <- colMeans(mase_rwd)
  mase[6,] <- colMeans(mase_stlar)
  mase[7,] <- colMeans(mase_theta)
  if (m>1){ mase[8,] <- colMeans(mase_snaive) }
  
  if (m==1){
    row.names(mase) <- c("auto.arima", "ets", "WN", "RW", "RWD", "STL_AR", "Theta")
  } else {
    row.names(mase) <- c("auto.arima", "ets", "WN", "RW", "RWD", "STL_AR", "Theta", "snaive")      
  }

  return(mase)
}
