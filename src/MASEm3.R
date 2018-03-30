MASEm3 <- function(temp,h){ 
  training <- temp$x
  testing <- temp$xx[1:h]
  m <- frequency(training)
  #h <- length(testing)
  # auto.arima
  fitArima <- auto.arima(training)
  forecastArima <- forecast(fitArima, h)
  ARIMA <- accuracy(forecastArima,testing)[2,6]
  # ETS models
  fitEts <- ets(training)
  forecastEts <- forecast(fitEts, h)
  ETS <- accuracy(forecastEts,testing)[2,6]
  # WN
  fitWN <- Arima(training,order=c(0,0,0))
  forecastWN <- forecast(fitWN, h)
  WN <- accuracy(forecastWN,testing)[2,6]
  # RW
  fitRW <- Arima(training,order=c(0,1,0))
  forecastRW <- forecast(fitRW, h)
  RW <- accuracy(forecastRW,testing)[2,6]
  # RD
  fitRWD <- Arima(training,order=c(0,1,0), include.drift=T)
  forecastRWD <- forecast(fitRWD,h)
  RWD <- accuracy(forecastRWD,testing)[2,6]
  
  #STL-AR
  STLAR <- stlar(training,h=h)
  STLAR <- accuracy(STLAR,testing)["Test set","MASE"]
  
  # Theta
  if (m > 1){
    # using stheta method with seasonal adjustment
    # require(forecTheta)
    fitTheta <- stheta(training,h=h, s='additive')
    meanDiff <- mean(abs(diff(training, lag = frequency(training))))
    Theta <- errorMetric(obs=testing, forec=fitTheta$mean, type = "AE", statistic = "M") / meanDiff
  }else{
    # using thetaf method
    fitTheta <-thetaf(training,h=length(testing))
    Theta <- accuracy(fitTheta, testing)["Test set","MASE"]
  }
  
  MaseM3 <- data.frame(ARIMA, ETS, WN, RW, RWD, STLAR, Theta)
  names(MaseM3) <- c("auto.arima", "ets", "WN", "RW", "RWD", "STLAR", "Theta")
  
  
  
  
  return(MaseM3)
  
}