library(forecast)

#-----------------------

head.ts <- function(x, h){ 
  if(!is.ts(x))
    x <- ts(x, frequency=findfrequency(x))
  endx <- end(x)
  window(x, end=c(endx[1],endx[2]-h))
}


#-----------------------

monthly_mase_h1 <- function(temp){ 
  training <- temp$x
  testing <- temp$xx
  testing1 <- ts(testing[1], start=start(testing), frequency=12)
  m <- frequency(training)
  
  # auto.arima
  fitArima <- auto.arima(training)
  forecastArima <- forecast(fitArima, 18)
  ARIMA1 <- accuracy(forecastArima,testing1)[2,6]
  
  # ETS models
  fitEts <- ets(training)
  forecastEts <- forecast(fitEts, 18)
  ETS1 <- accuracy(forecastEts,testing1)[2,6]
  
  # WN
  fitWN <- Arima(training,order=c(0,0,0))
  forecastWN <- forecast(fitWN, 18)
  WN1 <- accuracy(forecastWN,testing1)[2,6]
  
  
  # RW
  fitRW <- Arima(training,order=c(0,1,0))
  forecastRW <- forecast(fitRW, 18)
  RW1 <- accuracy(forecastRW,testing1)[2,6]
  
  # RD
  fitRWD <- Arima(training,order=c(0,1,0), include.drift=T)
  forecastRWD <- forecast(fitRWD,18)
  RWD1 <- accuracy(forecastRWD,testing1)[2,6]
  
  
  #STL-AR
  STLAR <- stlar(training,h=18)
  STLAR1 <- accuracy(STLAR,testing1)["Test set","MASE"]
  
  # Theta
  if (m > 1){
    # using stheta method with seasonal adjustment
    # require(forecTheta)
    fitTheta <- stheta(training,h=18, s='additive')
    meanDiff <- mean(abs(diff(training, lag = frequency(training))))
    Theta <- errorMetric(obs=testing, forec=fitTheta$mean, type = "AE", statistic = "M") / meanDiff
    Theta1 <- errorMetric(obs=testing1, forec=head.ts(fitTheta$mean, 18-1), type = "AE", statistic = "M") / meanDiff
    
  }else{
    # using thetaf method
    fitTheta <-thetaf(training,h=18)
    Theta1 <- accuracy(fitTheta, testing1)["Test set","MASE"]
  }
  
  #SNAIVE
  fit_snaive <- snaive(training, h=18)
  snaive1 <- accuracy(fit_snaive, testing1)["Test set","MASE"]
  
  
  MaseMonthly <- data.frame(ARIMA1, ETS1, WN1, RW1, RWD1, STLAR1, Theta1, snaive1)
  
  names(MaseMonthly) <- c("ARIMA1", "ETS1", "WN1", "RW1","RWD1", "STLAR1", "Theta1", "snaive1")
  
  return(MaseMonthly)
}