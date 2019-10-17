library(tidyr)

##m3yearly
#for plotting
m3yearly1 <- data.frame(h1=M3Yrfc_h1_meanMASE,
                       h2=M3Yrfc_h2_meanMASE,
                       h4=M3Yrfc_h4_meanMASE,
                       h6=M3Yrfc_h6_meanMASE,
                       method="RF-class priors")
m3yearly2 <- data.frame(h1=M3Yrfu_h1_meanMASE,
                        h2=M3Yrfu_h2_meanMASE,
                        h4=M3Yrfu_h4_meanMASE,
                        h6=M3Yrfu_h6_meanMASE,
                        method="RF-unbalanced")
m3ybench1 <- MASEh1_m3y %>% gather(method, h1, auto.arima:Theta)
m3ybench2 <- MASEh2_m3y %>% gather(method, h2, auto.arima:Theta)
m3ybench4 <- MASEh4_m3y %>% gather(method, h4, auto.arima:Theta)
m3ybench6 <- MASEh6_m3y %>% gather(method, h6, auto.arima:Theta)
m3ybench <- data.frame(h1=m3ybench1$h1, h2=m3ybench2$h2,
                       h4=m3ybench4$h4, h6=m3ybench6$h6,
                       method=m3ybench1$method)
m3yearly <- rbind(m3yearly2,m3yearly1, m3ybench)
m3yearly$series <- rep("M3", 5160)

#m1yearly
m1yearly1 <- data.frame(h1=M1Yrfc_h1_meanMASE,
                        h2=M1Yrfc_h2_meanMASE,
                        h4=M1Yrfc_h4_meanMASE,
                        h6=M1Yrfc_h6_meanMASE,
                        method="RF-class priors")
m1yearly2 <- data.frame(h1=M1Yrfu_h1_meanMASE,
                        h2=M1Yrfu_h2_meanMASE,
                        h4=M1Yrfu_h4_meanMASE,
                        h6=M1Yrfu_h6_meanMASE,
                        method="RF-unbalanced")
m1ybench1 <- MASEh1_m1y %>% gather(method, h1, auto.arima:Theta)
m1ybench2 <- MASEh2_m1y %>% gather(method, h2, auto.arima:Theta)
m1ybench4 <- MASEh4_m1y %>% gather(method, h4, auto.arima:Theta)
m1ybench6 <- MASEh6_m1y %>% gather(method, h6, auto.arima:Theta)
m1ybench <- data.frame(h1=m1ybench1$h1, h2=m1ybench2$h2,
                       h4=m1ybench4$h4, h6=m1ybench6$h6,
                       method=m1ybench1$method)
m1yearly <- rbind(m1yearly2,m1yearly1, m1ybench)
m1yearly$series <- rep("M1", 1448)

yearly_boxplot <- rbind(m3yearly, m1yearly)
saveRDS(yearly_boxplot, file = "data/yearly_boxplot.rds")

## Quarterly
##m3quaterly
#for plotting
m3quarterly1 <- data.frame(h1=M3Qrfc_h1_meanMASE,
                        h4=M3Qrfc_h4_meanMASE,
                        h6=M3Qrfc_h6_meanMASE,
                        h8=M3Qrfc_h8_meanMASE,
                        method="RF-class priors")
m3quarterly2 <- data.frame(h1=M3Qrfu_h1_meanMASE,
                        h4=M3Qrfu_h4_meanMASE,
                        h6=M3Qrfu_h6_meanMASE,
                        h8=M3Qrfu_h8_meanMASE,
                        method="RF-unbalanced")

m3qbench <- data.frame(h1=c(MASEOtherm3quarterly$ARIMA1, MASEOtherm3quarterly$ETS1, MASEOtherm3quarterly$WN1, MASEOtherm3quarterly$RW1,MASEOtherm3quarterly$RWD1, MASEOtherm3quarterly$STLAR1, MASEOtherm3quarterly$Theta1, MASEOtherm3quarterly$snaive1), 
                       h4=c(MASEOtherm3quarterly$ARIMA4, MASEOtherm3quarterly$ETS4, MASEOtherm3quarterly$WN4, MASEOtherm3quarterly$RW4,MASEOtherm3quarterly$RWD4, MASEOtherm3quarterly$STLAR4, MASEOtherm3quarterly$Theta4, MASEOtherm3quarterly$snaive4), 
                       h6=c(MASEOtherm3quarterly$ARIMA6, MASEOtherm3quarterly$ETS6, MASEOtherm3quarterly$WN6, MASEOtherm3quarterly$RW6,MASEOtherm3quarterly$RWD6, MASEOtherm3quarterly$STLAR6, MASEOtherm3quarterly$Theta6, MASEOtherm3quarterly$snaive6),
                       h8=c(MASEOtherm3quarterly$ARIMA8, MASEOtherm3quarterly$ETS8, MASEOtherm3quarterly$WN8, MASEOtherm3quarterly$RW8,MASEOtherm3quarterly$RWD8, MASEOtherm3quarterly$STLAR8, MASEOtherm3quarterly$Theta8, MASEOtherm3quarterly$snaive8))
m3qbench$method <- c(rep("auto.arima",756),
                     rep("ets",756),
                     rep("WN",756),
                     rep("RW",756),
                     rep("RWD",756),
                     rep("STL-AR",756),
                     rep("Theta",756),
                     rep("Snaive",756))
m3quarterly <- rbind(m3quarterly2, m3quarterly1, m3qbench)
m3quarterly$series <- rep("M3", 7560)

##m1quaterly
#for plotting
m1quarterlq1 <- data.frame(h1=M1Qrfc_h1_meanMASE,
                          h4=M1Qrfc_h4_meanMASE,
                          h6=M1Qrfc_h6_meanMASE,
                          h8=M1Qrfc_h8_meanMASE,
                          method="RF-class priors")
m1quarterlq2 <- data.frame(h1=M1Qrfu_h1_meanMASE,
                          h4=M1Qrfu_h4_meanMASE,
                          h6=M1Qrfu_h6_meanMASE,
                          h8=M1Qrfu_h8_meanMASE,
                          method="RF-unbalanced")

m1qbench <- data.frame(h1=c(MASEOtherm1quarterly$ARIMA1, MASEOtherm1quarterly$ETS1, MASEOtherm1quarterly$WN1, MASEOtherm1quarterly$RW1,MASEOtherm1quarterly$RWD1, MASEOtherm1quarterly$STLAR1, MASEOtherm1quarterly$Theta1, MASEOtherm1quarterly$snaive1), 
                       h4=c(MASEOtherm1quarterly$ARIMA4, MASEOtherm1quarterly$ETS4, MASEOtherm1quarterly$WN4, MASEOtherm1quarterly$RW4,MASEOtherm1quarterly$RWD4, MASEOtherm1quarterly$STLAR4, MASEOtherm1quarterly$Theta4, MASEOtherm1quarterly$snaive4), 
                       h6=c(MASEOtherm1quarterly$ARIMA6, MASEOtherm1quarterly$ETS6, MASEOtherm1quarterly$WN6, MASEOtherm1quarterly$RW6,MASEOtherm1quarterly$RWD6, MASEOtherm1quarterly$STLAR6, MASEOtherm1quarterly$Theta6, MASEOtherm1quarterly$snaive6),
                       h8=c(MASEOtherm1quarterly$ARIMA8, MASEOtherm1quarterly$ETS8, MASEOtherm1quarterly$WN8, MASEOtherm1quarterly$RW8,MASEOtherm1quarterly$RWD8, MASEOtherm1quarterly$STLAR8, MASEOtherm1quarterly$Theta8, MASEOtherm1quarterly$snaive8))
m1qbench$method <- c(rep("auto.arima",203),
                     rep("ets",203),
                     rep("WN",203),
                     rep("RW",203),
                     rep("RWD",203),
                     rep("STL-AR",203),
                     rep("Theta",203),
                     rep("Snaive",203))
m1quarterly <- rbind(m1quarterlq2, m1quarterlq1, m1qbench)
m1quarterly$series <- rep("M1", 2030)

quarterly_boxplot <- rbind(m3quarterly, m1quarterly)
saveRDS(quarterly_boxplot, file = "data/quarterly_boxplot.rds")


## Monthly
### m3
monUnbm3 <- cbind(M3Munb_h1, M3Munb_h)
colnames(monUnbm3) <- c("h=1", "h=6", "h=12", "h=18")
monUnbm3$method <- rep("RF-unbalanced", 1428)

monCPm3 <- cbind(M3Mrfc_h1, M3Mrfc_h)
colnames(monCPm3) <- c("h=1", "h=6", "h=12", "h=18")
monCPm3$method <- rep("RF-class priors", 1428)
m3mbench1 <- MASE_Other_m3monthly_h1 %>% gather(method, h1, ARIMA1:snaive1)
m3mbench1$method <- c(rep("auto.arima", 1428),
                      rep("ets", 1428),
                      rep("WN", 1428),
                      rep("RW", 1428),
                      rep("RWD", 1428),
                      rep("STL-AR", 1428),
                      rep("Theta", 1428),
                      rep("Snaive", 1428))
m3mbench <- data.frame(
                       h6=c(MASEOtherm3monthly$ARIMA6, MASEOtherm3monthly$ETS6, MASEOtherm3monthly$WN6, MASEOtherm3monthly$RW6, MASEOtherm3monthly$RWD6, MASEOtherm3monthly$STLAR6, MASEOtherm3monthly$Theta6, MASEOtherm3monthly$snaive6), 
                       h12=c(MASEOtherm3monthly$ARIMA12, MASEOtherm3monthly$ETS12, MASEOtherm3monthly$WN12, MASEOtherm3monthly$RW12, MASEOtherm3monthly$RWD12, MASEOtherm3monthly$STLAR12, MASEOtherm3monthly$Theta12, MASEOtherm3monthly$snaive12),
                       h18=c(MASEOtherm3monthly$ARIMA18, MASEOtherm3monthly$ETS18, MASEOtherm3monthly$WN18, MASEOtherm3monthly$RW18, MASEOtherm3monthly$RWD18, MASEOtherm3monthly$STLAR18, MASEOtherm3monthly$Theta18, MASEOtherm3monthly$snaive18))
m3mbench$method <- c(rep("auto.arima", 1428),
                     rep("ets", 1428),
                     rep("WN", 1428),
                     rep("RW", 1428),
                     rep("RWD", 1428),
                     rep("STL-AR", 1428),
                     rep("Theta", 1428),
                     rep("Snaive", 1428))
m3mbench$h1 <- m3mbench1$h1
m3mbenchall <- data.frame(h1=m3mbench$h1,
                          h6=m3mbench$h6,
                          h12=m3mbench$h12,
                          h18=m3mbench$h18,
                          method=m3mbench$method) 
colnames(m3mbenchall) <- c("h=1", "h=6", "h=12", "h=18", "method")
m3monthly <- rbind(monUnbm3, monCPm3, m3mbenchall)
m3monthly$series <- rep("M3", 14280)

### m1
monUnbm1 <- cbind(M1Munb_h1, M1Munb_h)
colnames(monUnbm1) <- c("h=1", "h=6", "h=12", "h=18")
monUnbm1$method <- rep("RF-unbalanced", 617)

monCPm1 <- cbind(M1Mrfc_h1, M1Mrfc_h)
colnames(monCPm1) <- c("h=1", "h=6", "h=12", "h=18")
monCPm1$method <- rep("RF-class priors", 617)
m1mbench1 <- MASE_Other_m1monthly_h1 %>% gather(method, h1, ARIMA1:snaive1)
m1mbench1$method <- c(rep("auto.arima", 617),
                      rep("ets", 617),
                      rep("WN", 617),
                      rep("RW", 617),
                      rep("RWD", 617),
                      rep("STL-AR", 617),
                      rep("Theta", 617),
                      rep("Snaive", 617))
m1mbench <- data.frame(
  h6=c(MASE_Other_m1monthly_h$ARIMA6, MASE_Other_m1monthly_h$ETS6, MASE_Other_m1monthly_h$WN6, MASE_Other_m1monthly_h$RW6, MASE_Other_m1monthly_h$RWD6, MASE_Other_m1monthly_h$STLAR6, MASE_Other_m1monthly_h$Theta6, MASE_Other_m1monthly_h$snaive6), 
  h12=c(MASE_Other_m1monthly_h$ARIMA12, MASE_Other_m1monthly_h$ETS12, MASE_Other_m1monthly_h$WN12, MASE_Other_m1monthly_h$RW12, MASE_Other_m1monthly_h$RWD12, MASE_Other_m1monthly_h$STLAR12, MASE_Other_m1monthly_h$Theta12, MASE_Other_m1monthly_h$snaive12),
  h18=c(MASE_Other_m1monthly_h$ARIMA18, MASE_Other_m1monthly_h$ETS18, MASE_Other_m1monthly_h$WN18, MASE_Other_m1monthly_h$RW18, MASE_Other_m1monthly_h$RWD18, MASE_Other_m1monthly_h$STLAR18, MASE_Other_m1monthly_h$Theta18, MASE_Other_m1monthly_h$snaive18))
m1mbench$method <- c(rep("auto.arima", 617),
                     rep("ets", 617),
                     rep("WN", 617),
                     rep("RW", 617),
                     rep("RWD", 617),
                     rep("STL-AR", 617),
                     rep("Theta", 617),
                     rep("Snaive", 617))
m1mbench$h1 <- m1mbench1$h1
m1mbenchall <- data.frame(h1=m1mbench$h1,
                          h6=m1mbench$h6,
                          h12=m1mbench$h12,
                          h18=m1mbench$h18,
                          method=m1mbench$method) 
colnames(m1mbenchall) <- c("h=1", "h=6", "h=12", "h=18", "method")
m1monthly <- rbind(monUnbm1, monCPm1, m1mbenchall)
m1monthly$series <- rep("M1", 6170)         
monthly_boxplot <- rbind(m3monthly, m1monthly)
saveRDS(monthly_boxplot, file = "data/monthly_boxplot.rds")


#### GGPLOT - boxplot 
yearly_box <- readRDS("data/yearly_boxplot.rds")
quarterly_box <- readRDS("data/quarterly_boxplot.rds")
monthly_box <- readRDS("data/monthly_boxplot.rds")

## yearly series
library(tidyverse)
yearly <- yearly_box %>% gather(h, MASE, "h1":"h6")
yearly$h <- recode(yearly$h , h1 = "h=1", h2 = "h=1-2", h4= "h=1-4", h6="h=1-6")
yearly_bp <- ggplot(yearly, aes(x=method, y=MASE, group=method)) + geom_boxplot(aes(fill=method), outlier.size = 0.1) + 
  facet_grid(series ~ h) +theme(axis.text.x = element_text(angle=60, hjust=1))+xlab("")
## Example code to test the values in the table
#yea_sub <- yearly %>% filter(h=="h6", method=="WN", series=="M1")
# ssummary(yea_sub)

quarterly <- quarterly_box %>% gather(h, MASE, "h1":"h8")
quarterly$h <- recode(quarterly$h , h1 = "h=1", h4 = "h=1-4", h6= "h=1-6", h8="h=1-8")
quarterly_bp <- ggplot(quarterly, aes(x=method, y=MASE, group=method)) + geom_boxplot(aes(fill=method), outlier.size = 0.1) + 
  facet_grid(series ~ h) +theme(axis.text.x = element_text(angle=60, hjust=1))+xlab("")



monthly <- monthly_box %>% gather(h, MASE, "h=1":"h=18")
monthly$h <- recode(monthly$h , "h=1" = "h=1", "h=6" = "h=1-6", "h=12"= "h=1-12", "h=18"="h=1-18")
monthly$method <- factor(monthly$method, levels=c("RF-unbalanced", "RF-class priors",
                                                  "auto.arima", "ets", "WN", "RW", "RWD", "STL_AR", "Theta", "Snaive"))
monthly_bp <- ggplot(monthly, aes(x=method, y=MASE, group=method)) + geom_boxplot(aes(fill=method), outlier.size = 0.1) + 
  facet_grid(series ~ h) +theme(axis.text.x = element_text(angle=60, hjust=1))+xlab("")
