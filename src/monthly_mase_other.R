MASEmonthlyOther <- function(temp){ 
  training <- temp$x
  testing <- temp$xx
  testing6 <- head.ts( testing, 18-6)
  testing12 <- head.ts( testing, 18-12)
  testing18 <- head.ts( testing, 18-18)
  m <- frequency(training)
  
  # auto.arima
  fitArima <- auto.arima(training)
  forecastArima <- forecast(fitArima, 18)
  ARIMA6 <- accuracy(forecastArima,testing6)[2,6]
  ARIMA12 <- accuracy(forecastArima,testing12)[2,6]
  ARIMA18 <- accuracy(forecastArima,testing18)[2,6]
  
  
  # ETS models
  fitEts <- ets(training)
  forecastEts <- forecast(fitEts, 18)
  ETS6 <- accuracy(forecastEts,testing6)[2,6]
  ETS12 <- accuracy(forecastEts,testing12)[2,6]
  ETS18 <- accuracy(forecastEts,testing18)[2,6]
  
  # WN
  fitWN <- Arima(training,order=c(0,0,0))
  forecastWN <- forecast(fitWN, 18)
  WN6 <- accuracy(forecastWN,testing6)[2,6]
  WN12 <- accuracy(forecastWN,testing12)[2,6]
  WN18 <- accuracy(forecastWN,testing18)[2,6]
  
  # RW
  fitRW <- Arima(training,order=c(0,1,0))
  forecastRW <- forecast(fitRW, 18)
  RW6 <- accuracy(forecastRW,testing6)[2,6]
  RW12 <- accuracy(forecastRW,testing12)[2,6]
  RW18 <- accuracy(forecastRW,testing18)[2,6]
  
  # RD
  fitRWD <- Arima(training,order=c(0,1,0), include.drift=T)
  forecastRWD <- forecast(fitRWD,18)
  RWD6 <- accuracy(forecastRWD,testing6)[2,6]
  RWD12 <- accuracy(forecastRWD,testing12)[2,6]
  RWD18 <- accuracy(forecastRWD,testing18)[2,6]
  
  
  #STL-AR
  STLAR <- stlar(training,h=18)
  STLAR6 <- accuracy(STLAR,testing6)["Test set","MASE"]
  STLAR12 <- accuracy(STLAR,testing12)["Test set","MASE"]
  STLAR18 <- accuracy(STLAR,testing18)["Test set","MASE"]
  
  
  # Theta
  if (m > 1){
    # using stheta method with seasonal adjustment
    # require(forecTheta)
    fitTheta <- stheta(training,h=18, s='additive')
    meanDiff <- mean(abs(diff(training, lag = frequency(training))))
    Theta <- errorMetric(obs=testing, forec=fitTheta$mean, type = "AE", statistic = "M") / meanDiff
    Theta6 <- errorMetric(obs=testing6, forec=head.ts(fitTheta$mean,18-6), type = "AE", statistic = "M") / meanDiff
    Theta12 <- errorMetric(obs=testing12, forec=head.ts(fitTheta$mean, 18-12), type = "AE", statistic = "M") / meanDiff
    Theta18 <- errorMetric(obs=testing18, forec=head.ts(fitTheta$mean, 18-18), type = "AE", statistic = "M") / meanDiff
  }else{
    # using thetaf method
    fitTheta <-thetaf(training,h=18)
    Theta6 <- accuracy(fitTheta, testing6)["Test set","MASE"]
    Theta12 <- accuracy(fitTheta, testing12)["Test set","MASE"]
    Theta18 <- accuracy(fitTheta, testing18)["Test set","MASE"]
  }
  
  #SNAIVE
  fit_snaive <- snaive(training, h=18)
  snaive6 <- accuracy(fit_snaive, testing6)["Test set","MASE"]
  snaive12 <- accuracy(fit_snaive, testing12)["Test set","MASE"]
  snaive18 <- accuracy(fit_snaive, testing18)["Test set","MASE"]
  
  MaseMonthly <- data.frame(ARIMA6, ARIMA12, ARIMA18,
                            ETS6, ETS12, ETS18, 
                            WN6, WN12, WN18, 
                            RW6, RW12, RW18,
                            RWD6,RWD12, RWD18, 
                            STLAR6,STLAR12, STLAR18,
                            Theta6, Theta12, Theta18,
                            snaive6, snaive12, snaive18)
  
  names(MaseMonthly) <- c("ARIMA6", "ARIMA12", "ARIMA18",
                          "ETS6", "ETS12", "ETS18", 
                          "WN6",  "WN12", "WN18", 
                          "RW6", "RW12", "RW18",
                          "RWD6", "RWD12", "RWD18", 
                          "STLAR6","STLAR12", "STLAR18",
                          "Theta6", "Theta12", "Theta18",
                          "snaive6", "snaive12", "snaive18")
  
  
  
  return(MaseMonthly)
}