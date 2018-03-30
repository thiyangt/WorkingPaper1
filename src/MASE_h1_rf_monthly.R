library(forecast)
library(forecTheta)


head.ts <- function(x, h){ 
  if(!is.ts(x))
    x <- ts(x, frequency=findfrequency(x))
  endx <- end(x)
  window(x, end=c(endx[1],endx[2]-h))
}

monthly_h1_rf <- function(predictions, tsList){
  
  total_ts <- length(tsList)
  outMASE1 <- numeric(total_ts)
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    testing <- tsList[[i]]$xx
    testing1 <- ts(testing[1], start=start(testing), frequency=12)
    
    #h <- length(testing)
    m <- frequency(training)
    if (predictions[i] == "ARIMA") {
      fit_arima <- auto.arima(training, seasonal = FALSE)
      forecast_arima <- forecast(fit_arima,18)
      outMASE1[i] <- accuracy(forecast_arima,testing1)[2,"MASE"] 
      
    } else if (predictions[i] == "ARMA/AR/MA") { 
      fit_arma <- auto.arima(training,d=0, stationary=TRUE, seasonal = FALSE)
      forecast_arma <- forecast(fit_arma,18)
      outMASE1[i] <- accuracy(forecast_arma,testing1)[2,"MASE"]
      
    }else if (predictions[i] == "SARIMA") { 
      fit_sarima <- auto.arima(training, seasonal=TRUE)
      forecast_sarima <- forecast(fit_sarima,18)
      outMASE1[i] <- accuracy(forecast_sarima,testing1)[2,"MASE"]
      
    }  else if (predictions[i] == "ETS-dampedtrend") { 
      fit_ets <- ets(training, model= "ZZN", damped = TRUE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE1[i] <- accuracy(forecast_ets,testing1)[2,"MASE"]
      
    } else if (predictions[i] == "ETS-notrendnoseasonal") { 
      fit_ets <- ets(training, model= "ZNN", damped = FALSE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE1[i] <- accuracy(forecast_ets,testing1)[2,"MASE"]
      
    } else if (predictions[i] == "ETS-trend") { 
      fit_ets <- ets(training, model= "ZZN", damped = FALSE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE1[i] <- accuracy(forecast_ets,testing1)[2,"MASE"]
      
    } else if (predictions[i] == "ETS-trendseasonal") { 
      fit_ets <- ets(training, model= "ZZZ", damped = FALSE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE1[i] <- accuracy(forecast_ets,testing1)[2,"MASE"]
      
    }else if (predictions[i] == "ETS-dampedtrendseasonal") { 
      fit_ets <- ets(training, model= "ZZZ", damped = TRUE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE1[i] <- accuracy(forecast_ets,testing1)[2,"MASE"]
      
    }else if (predictions[i] == "ETS-seasonal") { 
      fit_ets <- ets(training, model= "ZNZ")
      forecast_ets <- forecast(fit_ets,18)
      outMASE1[i] <- accuracy(forecast_ets,testing1)[2,"MASE"]
      
    }else if (predictions[i] == "SNAIVE") { 
      fit_snaive <- snaive(training, h=18)
      outMASE1[i] <- accuracy(fit_snaive, testing1)["Test set","MASE"]
      
    }else if (predictions[i] == "RW") { 
      fit_rw <- rwf(training, drift = FALSE)
      forecast_rw <- forecast(fit_rw,18)
      outMASE1[i] <- accuracy(forecast_rw,testing1)[2,"MASE"]
      
    } else if (predictions[i] == "RWD") { 
      fit_rwd <- rwf(training, drift = TRUE)
      forecast_rwd <- forecast(fit_rwd,18)
      outMASE1[i] <- accuracy(forecast_rwd,testing1)[2,"MASE"]
      
    } else if (predictions[i] == "STL-AR") { 
      STLAR <- stlar(training,h=18)
      outMASE1[i] <- accuracy(STLAR,testing1)["Test set","MASE"]
      
    } else if (predictions[i] == "Theta") {
      if (m > 1){
        # using stheta method with seasonal adjustment
        # require(forecTheta)
        fitTheta <- stheta(training,h=18, s='additive')
        meanDiff <- mean(abs(diff(training, lag = frequency(training))))
        outMASE1[i] <- errorMetric(obs=testing1, forec=head.ts(fitTheta$mean, 18-1), type = "AE", statistic = "M") / meanDiff
        
      }else{
        # using thetaf method
        fitTheta <-thetaf(training,h=18)
        outMASE1[i] <- accuracy(fitTheta, testing1)["Test set","MASE"]
        
      }
    } else {
      fit_wn <- Arima(training,order=c(0,0,0))
      forecast_wn <- forecast(fit_wn,18)
      outMASE1[i] <- accuracy(forecast_wn,testing1)[2,"MASE"]
    }
    
  }
  
  outMASE <- data.frame(outMASE1)
  return(outMASE)
  
}