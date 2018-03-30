library(forecast)
library(forecTheta)

source("src/stlar.R")

calMASEh <- function(predictions, tsList,h){
  
  total_ts <- length(tsList)
  outMASE <- numeric(total_ts)
  error_models<-c()
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    testing <- tsList[[i]]$xx[1:h]
    #h <- length(testing)
    m <- frequency(training)
    if (predictions[i] == "ARIMA") {
      fit_arima <- auto.arima(training)
      forecast_arima <- forecast(fit_arima,h)
      outMASE[i] <- accuracy(forecast_arima,testing)[2,"MASE"] 
    } else if (predictions[i] == "ARMA/AR/MA") { 
      fit_arma <- auto.arima(training,d=0, stationary=TRUE)
      forecast_arma <- forecast(fit_arma,h)
      outMASE[i] <- accuracy(forecast_arma,testing)[2,"MASE"]
    } else if (predictions[i] == "ETS-dampedtrend") { 
      fit_ets <- ets(training, model= "ZZN", damped = TRUE)
      forecast_ets <- forecast(fit_ets,h)
      outMASE[i] <- accuracy(forecast_ets,testing)[2,"MASE"]
    } else if (predictions[i] == "ETS-notrend") { 
      fit_ets <- ets(training, model= "ZNN", damped = FALSE)
      forecast_ets <- forecast(fit_ets,h)
      outMASE[i] <- accuracy(forecast_ets,testing)[2,"MASE"]
    } else if (predictions[i] == "ETS-trend") { 
      fit_ets <- ets(training, model= "ZZN", damped = FALSE)
      forecast_ets <- forecast(fit_ets,h)
      outMASE[i] <- accuracy(forecast_ets,testing)[2,"MASE"]
    } else if (predictions[i] == "Random Walk") { 
      fit_rw <- rwf(training, drift = FALSE)
      forecast_rw <- forecast(fit_rw,h)
      outMASE[i] <- accuracy(forecast_rw,testing)[2,"MASE"]
    } else if (predictions[i] == "Random Walk with Drift") { 
      fit_rwd <- rwf(training, drift = TRUE)
      forecast_rwd <- forecast(fit_rwd,h)
      outMASE[i] <- accuracy(forecast_rwd,testing)[2,"MASE"]
    } else if (predictions[i] == "STL-AR") { 
      STLAR <- stlar(training,h=h)
      outMASE[i] <- accuracy(STLAR,testing)["Test set","MASE"]
    } else if (predictions[i] == "Theta") {
      if (m > 1){
        # using stheta method with seasonal adjustment
        # require(forecTheta)
        fitTheta <- stheta(training,h=h, s='additive')
        meanDiff <- mean(abs(diff(training, lag = frequency(training))))
        outMASE[i] <- errorMetric(obs=testing, forec=fitTheta$mean, type = "AE", statistic = "M") / meanDiff
      }else{
        # using thetaf method
        fitTheta <-thetaf(training,h=length(testing))
        outMASE[i] <- accuracy(fitTheta, testing)["Test set","MASE"]
      }
    } else {
      fit_wn <- Arima(training,order=c(0,0,0))
      forecast_wn <- forecast(fit_wn,h)
      outMASE[i] <- accuracy(forecast_wn,testing)[2,"MASE"]
      
    }
    
  }
  
  return(outMASE)
  
}

#library(Mcomp)
#data(M3)
# summary of M3 data 
#M3

# Extract annual data - 645
#yearly_m3 <- subset(M3, "yearly")
#MASEM3Y <- calMASE(predictionsM3, yearly_m3)
#summary(MASEM3Y)


