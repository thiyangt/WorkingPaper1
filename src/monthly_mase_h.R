library(forecast)
library(forecTheta)

#source("src/2-stlar.R")

head.ts <- function(x, h){ 
  if(!is.ts(x))
    x <- ts(x, frequency=findfrequency(x))
  endx <- end(x)
  window(x, end=c(endx[1],endx[2]-h))
}

monthlyMASE_h <- function(predictions, tsList){
  
  total_ts <- length(tsList)
  outMASE6 <- numeric(total_ts)
  outMASE12 <- numeric(total_ts)
  outMASE18 <- numeric(total_ts)
  
  for (i in 1:total_ts){
    
    training <- tsList[[i]]$x
    testing6 <- head.ts(tsList[[i]]$xx, 18-6)
    testing12 <- head.ts(tsList[[i]]$xx, 18-12)
    testing18 <- head.ts(tsList[[i]]$xx, 18-18)
    
    #h <- length(testing)
    m <- frequency(training)
    if (predictions[i] == "ARIMA") {
      fit_arima <- auto.arima(training, seasonal = FALSE)
      forecast_arima <- forecast(fit_arima,18)
      outMASE6[i] <- accuracy(forecast_arima,testing6)[2,"MASE"] 
      outMASE12[i] <- accuracy(forecast_arima,testing12)[2,"MASE"] 
      outMASE18[i] <- accuracy(forecast_arima,testing18)[2,"MASE"] 
    } else if (predictions[i] == "ARMA/AR/MA") { 
      fit_arma <- auto.arima(training,d=0, stationary=TRUE, seasonal = FALSE)
      forecast_arma <- forecast(fit_arma,18)
      outMASE6[i] <- accuracy(forecast_arma,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_arma,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_arma,testing18)[2,"MASE"]
    }else if (predictions[i] == "SARIMA") { 
      fit_sarima <- auto.arima(training, seasonal=TRUE)
      forecast_sarima <- forecast(fit_sarima,18)
      outMASE6[i] <- accuracy(forecast_sarima,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_sarima,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_sarima,testing18)[2,"MASE"]
    }  else if (predictions[i] == "ETS-dampedtrend") { 
      fit_ets <- ets(training, model= "ZZN", damped = TRUE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE6[i] <- accuracy(forecast_ets,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_ets,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_ets,testing18)[2,"MASE"]
    } else if (predictions[i] == "ETS-notrendnoseasonal") { 
      fit_ets <- ets(training, model= "ZNN", damped = FALSE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE6[i] <- accuracy(forecast_ets,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_ets,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_ets,testing18)[2,"MASE"]
    } else if (predictions[i] == "ETS-trend") { 
      fit_ets <- ets(training, model= "ZZN", damped = FALSE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE6[i] <- accuracy(forecast_ets,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_ets,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_ets,testing18)[2,"MASE"]
    } else if (predictions[i] == "ETS-trendseasonal") { 
      fit_ets <- ets(training, model= "ZZZ", damped = FALSE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE6[i] <- accuracy(forecast_ets,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_ets,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_ets,testing18)[2,"MASE"]
    }else if (predictions[i] == "ETS-dampedtrendseasonal") { 
      fit_ets <- ets(training, model= "ZZZ", damped = TRUE)
      forecast_ets <- forecast(fit_ets,18)
      outMASE6[i] <- accuracy(forecast_ets,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_ets,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_ets,testing18)[2,"MASE"]
    }else if (predictions[i] == "ETS-seasonal") { 
      fit_ets <- ets(training, model= "ZNZ")
      forecast_ets <- forecast(fit_ets,18)
      outMASE6[i] <- accuracy(forecast_ets,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_ets,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_ets,testing18)[2,"MASE"]
    }else if (predictions[i] == "SNAIVE") { 
      fit_snaive <- snaive(training, h=18)
      outMASE6[i] <- accuracy(fit_snaive, testing6)["Test set","MASE"]
      outMASE12[i] <- accuracy(fit_snaive, testing12)["Test set","MASE"]
      outMASE18[i] <- accuracy(fit_snaive, testing18)["Test set","MASE"]
    }else if (predictions[i] == "RW") { 
      fit_rw <- rwf(training, drift = FALSE)
      forecast_rw <- forecast(fit_rw,18)
      outMASE6[i] <- accuracy(forecast_rw,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_rw,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_rw,testing18)[2,"MASE"]
    } else if (predictions[i] == "RWD") { 
      fit_rwd <- rwf(training, drift = TRUE)
      forecast_rwd <- forecast(fit_rwd,18)
      outMASE6[i] <- accuracy(forecast_rwd,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_rwd,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_rwd,testing18)[2,"MASE"]
    } else if (predictions[i] == "STL-AR") { 
      STLAR <- stlar(training,h=18)
      outMASE6[i] <- accuracy(STLAR,testing6)["Test set","MASE"]
      outMASE12[i] <- accuracy(STLAR,testing12)["Test set","MASE"]
      outMASE18[i] <- accuracy(STLAR,testing18)["Test set","MASE"]
    } else if (predictions[i] == "Theta") {
      if (m > 1){
        # using stheta method with seasonal adjustment
        # require(forecTheta)
        fitTheta <- stheta(training,h=18, s='additive')
        meanDiff <- mean(abs(diff(training, lag = frequency(training))))
        outMASE6[i] <- errorMetric(obs=testing6, forec=head.ts(fitTheta$mean,18-6), type = "AE", statistic = "M") / meanDiff
        outMASE12[i] <- errorMetric(obs=testing12, forec=head.ts(fitTheta$mean, 18-12), type = "AE", statistic = "M") / meanDiff
        outMASE18[i] <- errorMetric(obs=testing18, forec=head.ts(fitTheta$mean, 18-18), type = "AE", statistic = "M") / meanDiff
      }else{
        # using thetaf method
        fitTheta <-thetaf(training,h=18)
        outMASE6[i] <- accuracy(fitTheta, testing6)["Test set","MASE"]
        outMASE12[i] <- accuracy(fitTheta, testing12)["Test set","MASE"]
        outMASE18[i] <- accuracy(fitTheta, testing18)["Test set","MASE"]
      }
    } else {
      fit_wn <- Arima(training,order=c(0,0,0))
      forecast_wn <- forecast(fit_wn,18)
      outMASE6[i] <- accuracy(forecast_wn,testing6)[2,"MASE"]
      outMASE12[i] <- accuracy(forecast_wn,testing12)[2,"MASE"]
      outMASE18[i] <- accuracy(forecast_wn,testing18)[2,"MASE"]
    }
    
  }
  
  outMASE <- data.frame(outMASE6, outMASE12, outMASE18)
  return(outMASE)
  
}



