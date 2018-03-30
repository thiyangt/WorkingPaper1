library(forecast)
#-----------------------

head.ts <- function(x, h){ 
  if(!is.ts(x))
    x <- ts(x, frequency=findfrequency(x))
  endx <- end(x)
  window(x, end=c(endx[1],endx[2]-h))
}

#-----------------------

mase_quarterlyOther <- function(temp){ 
  training <- temp$x
  testing <- temp$xx
  testing1 <- head.ts( testing, 8-1)
  testing4 <- head.ts( testing, 8-4)
  testing6 <- head.ts( testing, 8-6)
  testing8 <- head.ts( testing, 8-8)
  m <- frequency(training)
  
  # auto.arima
  fitArima <- auto.arima(training)
  forecastArima <- forecast(fitArima, 8)
  ARIMA1 <- accuracy(forecastArima,testing1)[2,6]
  ARIMA4 <- accuracy(forecastArima,testing4)[2,6]
  ARIMA6 <- accuracy(forecastArima,testing6)[2,6]
  ARIMA8 <- accuracy(forecastArima,testing8)[2,6]
  
  # ETS models
  fitEts <- ets(training)
  forecastEts <- forecast(fitEts, 8)
  ETS1 <- accuracy(forecastEts,testing1)[2,6]
  ETS4 <- accuracy(forecastEts,testing4)[2,6]
  ETS6 <- accuracy(forecastEts,testing6)[2,6]
  ETS8 <- accuracy(forecastEts,testing8)[2,6]
  
  # WN
  fitWN <- Arima(training,order=c(0,0,0))
  forecastWN <- forecast(fitWN, 8)
  WN1 <- accuracy(forecastWN,testing1)[2,6]
  WN4 <- accuracy(forecastWN,testing4)[2,6]
  WN6 <- accuracy(forecastWN,testing6)[2,6]
  WN8 <- accuracy(forecastWN,testing8)[2,6]
  
  # RW
  fitRW <- Arima(training,order=c(0,1,0))
  forecastRW <- forecast(fitRW, 8)
  RW1 <- accuracy(forecastRW,testing1)[2,6]
  RW4 <- accuracy(forecastRW,testing4)[2,6]
  RW6 <- accuracy(forecastRW,testing6)[2,6]
  RW8 <- accuracy(forecastRW,testing8)[2,6]
  
  # RD
  fitRWD <- Arima(training,order=c(0,1,0), include.drift=T)
  forecastRWD <- forecast(fitRWD,8)
  RWD1 <- accuracy(forecastRWD,testing1)[2,6]
  RWD4 <- accuracy(forecastRWD,testing4)[2,6]
  RWD6 <- accuracy(forecastRWD,testing6)[2,6]
  RWD8 <- accuracy(forecastRWD,testing8)[2,6]
  
  #STL-AR
  STLAR <- stlar(training,h=8)
  STLAR1 <- accuracy(STLAR,testing1)["Test set","MASE"]
  STLAR4 <- accuracy(STLAR,testing4)["Test set","MASE"]
  STLAR6 <- accuracy(STLAR,testing6)["Test set","MASE"]
  STLAR8 <- accuracy(STLAR,testing8)["Test set","MASE"]
  
  # Theta
  if (m > 1){
    # using stheta method with seasonal adjustment
    # require(forecTheta)
    fitTheta <- stheta(training,h=8, s='additive')
    meanDiff <- mean(abs(diff(training, lag = frequency(training))))
    Theta <- errorMetric(obs=testing, forec=fitTheta$mean, type = "AE", statistic = "M") / meanDiff
    Theta1 <- errorMetric(obs=testing1, forec=head.ts(fitTheta$mean, 8-1), type = "AE", statistic = "M") / meanDiff
    Theta4 <- errorMetric(obs=testing4, forec=head.ts(fitTheta$mean, 8-4), type = "AE", statistic = "M") / meanDiff
    Theta6 <- errorMetric(obs=testing6, forec=head.ts(fitTheta$mean, 8-6), type = "AE", statistic = "M") / meanDiff
    Theta8 <- errorMetric(obs=testing8, forec=head.ts(fitTheta$mean, 8-8), type = "AE", statistic = "M") / meanDiff
  }else{
    # using thetaf method
    fitTheta <-thetaf(training,h=8)
    Theta1 <- accuracy(fitTheta, testing1)["Test set","MASE"]
    Theta4 <- accuracy(fitTheta, testing4)["Test set","MASE"]
    Theta6 <- accuracy(fitTheta, testing6)["Test set","MASE"]
    Theta8 <- accuracy(fitTheta, testing8)["Test set","MASE"]
  }
  
  #SNAIVE
  fit_snaive <- snaive(training, h=8)
  snaive1 <- accuracy(fit_snaive, testing1)["Test set","MASE"]
  snaive4 <- accuracy(fit_snaive, testing4)["Test set","MASE"]
  snaive6 <- accuracy(fit_snaive, testing6)["Test set","MASE"]
  snaive8 <- accuracy(fit_snaive, testing8)["Test set","MASE"]
  
  MaseQuarterly <- data.frame(ARIMA1, ARIMA4, ARIMA6, ARIMA8,
                              ETS1, ETS4, ETS6, ETS8, 
                              WN1, WN4, WN6, WN8, 
                              RW1, RW4, RW6, RW8,
                              RWD1, RWD4, RWD6, RWD8,
                              STLAR1, STLAR4, STLAR6, STLAR8,
                              Theta1, Theta4, Theta6, Theta8,
                              snaive1, snaive4, snaive6, snaive8)
  
  names(MaseQuarterly) <- c("ARIMA1", "ARIMA4", "ARIMA6", "ARIMA8",
                            "ETS1", "ETS4", "ETS6", "ETS8",
                            "WN1", "WN4", "WN6", "WN8",
                            "RW1", "RW4", "RW6", "RW8",
                            "RWD1", "RWD4", "RWD6", "RWD8",
                            "STLAR1", "STLAR4", "STLAR6", "STLAR8",
                            "Theta1", "Theta4", "Theta6", "Theta8",
                            "snaive1", "snaive4", "snaive6", "snaive8")
  
  
  
  return(MaseQuarterly)
}


