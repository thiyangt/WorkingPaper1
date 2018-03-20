## ---- load
library(tidyverse)
library(ggplot2)
library(reshape2)

## ---- data
YE1 <- load("data/yearly_exp1.rda")
YE2 <- load("data/yearly_exp2.rda")
YE2_sub <- load("data/yearly_exp2_sub.rda")
QE1 <- load("data/quarterly_exp1.rda")
QE2 <- load("data/quarterly_exp2.rda")
QE2_sub <- load("data/quarterly_exp2_sub.rda")
ME1 <- load("data/monthly_exp1.rda")
ME2 <- load("data/monthly_exp2.rda")

yearly_exp1 <- rename(yearly_exp1, spikiness = spike)
yearly_exp2 <- rename(yearly_exp2, spikiness = spike)
quarterly_exp1 <- rename(quarterly_exp1, spikiness = spike)
quarterly_exp2 <- rename(quarterly_exp2, spikiness = spike)
monthly_exp1 <- rename(monthly_exp1, spikiness = spike)
monthly_exp2 <- rename(monthly_exp2, spikiness = spike)

yearly_exp1 <- rename(yearly_exp1,  "T" = N)
yearly_exp2 <- rename(yearly_exp2, "T" = N)
quarterly_exp1 <- rename(quarterly_exp1,  "T" = N)
quarterly_exp2 <- rename(quarterly_exp2,  "T" = N)
monthly_exp1 <- rename(monthly_exp1,  "T" = N)
monthly_exp2 <- rename(monthly_exp2,  "T" = N)


quarterly_exp1 <- rename(quarterly_exp1, seasonality = seasonal_strength)
quarterly_exp2 <- rename(quarterly_exp2, seasonality = seasonal_strength)
monthly_exp1 <- rename(monthly_exp1, seasonality = seasonal_strength)
monthly_exp2 <- rename(monthly_exp2, seasonality = seasonal_strength)

## --- figure 5

cormatA <- round(cor(yearly_exp1[,1:25]),2)
melted_cormatA <- melt(cormatA)
get_lower_triA <- function(cormatA){
  cormatA[upper.tri(cormatA)] <- NA
  return(cormatA)
}
get_upper_triA <- function(cormatA){
  cormatA[lower.tri(cormatA)]<- NA
  return(cormatA)
}
upper_triA <- get_upper_triA(cormatA)
melted_cormatA <- melt(upper_triA, na.rm = TRUE)
ggheatmapA <- ggplot(melted_cormatA, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 9),
        axis.title.x =  element_blank(),
        axis.title.y = element_text(size = 20, face="bold"),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 20, face="bold"))+
  coord_fixed()+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  labs(title = "Experiment 1", y = "Yearly")


cormatB <- round(cor(yearly_exp2[,1:25]),2)
melted_cormatB <- melt(cormatB)
get_lower_triB <- function(cormatB){
  cormatB[upper.tri(cormatB)] <- NA
  return(cormatB)
}
get_upper_triB <- function(cormatB){
  cormatB[lower.tri(cormatB)]<- NA
  return(cormatB)
}
upper_triB <- get_upper_triB(cormatB)
melted_cormatB <- melt(upper_triB, na.rm = TRUE)
ggheatmapB <- ggplot(melted_cormatB, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.x =  element_blank(),
        axis.title.y = element_text(size = 20, face="bold"),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal",
        plot.title = element_text(hjust = 0.5, size = 20, face="bold"))+
  coord_fixed()+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  labs(title = "Experiment 2", y = "")


cormatC <- round(cor(quarterly_exp1[, 1:30]),2)
get_lower_triC <- function(cormatC){
  cormatC[upper.tri(cormatC)] <- NA
  return(cormatC)
}
get_upper_triC <- function(cormatC){
  cormatC[lower.tri(cormatC)]<- NA
  return(cormatC)
}
upper_triC <- get_upper_triC(cormatC)
melted_cormatC <- melt(upper_triC, na.rm = TRUE)
ggheatmapC <- ggplot(melted_cormatC, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.x =  element_blank(),
        axis.title.y = element_text(size = 20, face="bold"),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  coord_fixed()+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  labs(title = "", y = "Quarterly")



cormatD <- round(cor(quarterly_exp2[, 1:30]),2)
get_lower_triD <-function(cormatD){
  cormatD[upper.tri(cormatD)] <- NA
  return(cormatD)
}
get_upper_triD <- function(cormatD){
  cormatD[lower.tri(cormatD)]<- NA
  return(cormatD)
}
upper_triD <- get_upper_triD(cormatD)
melted_cormatD <- melt(upper_triD, na.rm = TRUE)
ggheatmapD <- ggplot(melted_cormatD, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.x =  element_blank(),
        axis.title.y =  element_text(size = 20, face="bold"),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  coord_fixed()+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  labs(title = "", y = "")


cormatE <- round(cor(monthly_exp1[, 1:30]),2)
get_lower_triE <- function(cormatE){
  cormatE[upper.tri(cormatE)] <- NA
  return(cormatE)
}
get_upper_triE <- function(cormatE){
  cormatE[lower.tri(cormatE)]<- NA
  return(cormatE)
}
upper_triE <- get_upper_triE(cormatE)
melted_cormatE <- melt(upper_triE, na.rm = TRUE)
ggheatmapE <- ggplot(melted_cormatE, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.x =  element_blank(),
        axis.title.y = element_text(size = 20, face="bold"),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  coord_fixed()+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  labs(title = "", y = "Monthly")


cormatF <- round(cor(monthly_exp2[, 1:30]),2)
melted_cormatF <- melt(cormatF)
get_lower_triF <- function(cormatF){
  cormatF[upper.tri(cormatF)] <- NA
  return(cormatF)
}
get_upper_triF <- function(cormatF){
  cormatF[lower.tri(cormatF)]<- NA
  return(cormatF)
}
upper_triF <- get_upper_triF(cormatF)
melted_cormatF <- melt(upper_triF, na.rm = TRUE)
ggheatmapF <- ggplot(melted_cormatF, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 9, hjust = 1),
        axis.text.y = element_text(size = 13),
        axis.title.x =  element_blank(),
        axis.title.y =   element_text(size = 20, face="bold"),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
  coord_fixed()+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))+
  labs(title = "", y = "")

ggsave(ggheatmapA, filename = "figures/ggheatmapA.png")
ggsave(ggheatmapB, filename = "figures/ggheatmapB.png")
ggsave(ggheatmapC, filename = "figures/ggheatmapC.png")
ggsave(ggheatmapD, filename = "figures/ggheatmapD.png")
ggsave(ggheatmapE, filename = "figures/ggheatmapE.png")
ggsave(ggheatmapF, filename = "figures/ggheatmapF.png")

## ---- Experiment 1
pcaM1YDF <- filter(yearly_exp1, datasource=="M1") # Yealy - E1
pcaM1YDFvariables <- pcaM1YDF[,1:25]
pcaM1Y <- prcomp(pcaM1YDFvariables, center=TRUE, scale=TRUE)
#summary(pcaM1Y)
PC1m1y = pcaM1Y$x[,1]
PC2m1y = pcaM1Y$x[,2]
PC3m1y = pcaM1Y$x[,3]
m1yPCAresults = data.frame(PC1m1y, PC2m1y, PC3m1y,pcaM1YDFvariables)
M1Ysimulated <- filter(yearly_exp1, datasource!="M1")
projectM1Ysimulated <- M1Ysimulated[,1:25]
simulatedPCAM1Y <- scale(projectM1Ysimulated,pcaM1Y$center, pcaM1Y$scale) %*% pcaM1Y$rotation
M1Ypca <- data.frame(PC1=PC1m1y, PC2=PC2m1y, PC3=PC3m1y)
rownames(M1Ypca) <- NULL
SimulatedPCAM1Y <- data.frame(PC1=simulatedPCAM1Y[,1], PC2=simulatedPCAM1Y[,2], PC3=simulatedPCAM1Y[,3])
M3Y <- filter(yearly_exp2, datasource=="M3")
projectM3Y <- M3Y[,1:25]
simM3YPCA <- scale(projectM3Y, pcaM1Y$center,pcaM1Y$scale) %*% pcaM1Y$rotation
M3YPCA <- data.frame(PC1=simM3YPCA[,1], PC2=simM3YPCA[,2], PC3=simM3YPCA[,3])
pcaALLM1Y <- bind_rows(SimulatedPCAM1Y, M3YPCA, M1Ypca, .id="source")
pca1M1Y <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("Yearly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca1M1Y.png",  width = 5, height = 4)
pca2M1Y <- ggplot(pcaALLM1Y, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5,size = 20, face = "bold"))
ggsave("figures/pca2M1Y.png",  width = 5, height = 4)
pca3M1Y <- ggplot(pcaALLM1Y, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca3M1Y.png",  width = 5, height = 4)

pcaM1QDF <- filter(quarterly_exp1, datasource=="M1") # Quarterly - E1
pcaM1QDFvariables <- pcaM1QDF[,1:30]
pcaM1Q <- prcomp(pcaM1QDFvariables, center=TRUE, scale=TRUE)
summary(pcaM1Q)
PC1m1q = pcaM1Q$x[,1]
PC2m1q = pcaM1Q$x[,2]
PC3m1q = pcaM1Q$x[,3]
m1qPCAresults = data.frame(PC1m1q, PC2m1q, PC3m1q,pcaM1QDFvariables)
M1Qsimulated <- filter(quarterly_exp1, datasource!="M1")
projectM1Qsimulated <- M1Qsimulated[, 1:30]
simulatedPCAM1Q <- scale(projectM1Qsimulated,pcaM1Q$center, pcaM1Q$scale) %*% pcaM1Q$rotation
M1Qpca <- data.frame(PC1=PC1m1q, PC2=PC2m1q, PC3=PC3m1q)
rownames(M1Qpca) <- NULL
SimulatedPCAM1Q <- data.frame(PC1=simulatedPCAM1Q[,1], PC2=simulatedPCAM1Q[,2], PC3=simulatedPCAM1Q[,3])
M3Q <- filter(quarterly_exp2, datasource=="M3")
projectM3Q <- M3Q[,1:30]
simM3QPCA <- scale(projectM3Q, pcaM1Q$center,pcaM1Q$scale) %*% pcaM1Q$rotation
M3QPCA <- data.frame(PC1=simM3QPCA[,1], PC2=simM3QPCA[,2], PC3=simM3QPCA[,3])
pcaALLM1Q <- bind_rows(SimulatedPCAM1Q, M3QPCA, M1Qpca, .id="source")
pca1M1Q <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("Quarterly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca1M1Q.png", width = 5, height = 4)
pca2M1Q <- ggplot(pcaALLM1Q, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca2M1Q.png",  width = 5, height = 4)
pca3M1Q <- ggplot(pcaALLM1Q, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca3M1Q.png",  width = 5, height = 4)


pcaM1MDF <- filter(monthly_exp1, datasource=="M1") # Monthly-E1
pcaM1MDFvariables <- pcaM1MDF[,1:30]
pcaM1M <- prcomp(pcaM1MDFvariables, center=TRUE, scale=TRUE)
summary(pcaM1M)
PC1m1m = pcaM1M$x[,1]
PC2m1m = pcaM1M$x[,2]
PC3m1m = pcaM1M$x[,3]
m1mPCAresults = data.frame(PC1m1m, PC2m1m, PC3m1m,pcaM1MDFvariables)
M1Msimulated <- filter(monthly_exp1, datasource!="M1")
projectM1Msimulated <- M1Msimulated[, 1:30]
simulatedPCAM1M <- scale(projectM1Msimulated,pcaM1M$center, pcaM1M$scale) %*% pcaM1M$rotation
M1Mpca <- data.frame(PC1=PC1m1m, PC2=PC2m1m, PC3=PC3m1m)
rownames(M1Mpca) <- NULL
SimulatedPCAM1M <- data.frame(PC1=simulatedPCAM1M[,1], PC2=simulatedPCAM1M[,2], PC3=simulatedPCAM1M[,3])
M3M <- filter(monthly_exp2, datasource=="M3")
projectM3M <- M3M[, 1:30]
simM3MPCA <- scale(projectM3M, pcaM1M$center,pcaM1M$scale) %*% pcaM1M$rotation
M3MPCA <- data.frame(PC1=simM3MPCA[,1], PC2=simM3MPCA[,2], PC3=simM3MPCA[,3])
pcaALLM1M <- bind_rows(SimulatedPCAM1M, M3MPCA, M1Mpca, .id="source")
pca1M1M <- ggplot(pcaALLM1M, aes(x=PC1, y=PC2, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("Monthly")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca1M1M.png", width = 5, height = 4)
pca2M1M <- ggplot(pcaALLM1M, aes(x=PC1, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca2M1M.png", width = 5, height = 4)
pca3M1M <- ggplot(pcaALLM1M, aes(x=PC2, y=PC3, color=source)) + geom_point()+ theme(legend.position="none")+
  scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+theme(aspect.ratio = 1)+ggtitle("")+
  theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))
ggsave("figures/pca3M1M.png", width = 5, height = 4)


## ---- Experiment 2

pcaM3YDF <- filter(yearly_exp2, datasource=="M3") # Yearly - E2
pcaM3YDFvariables <- pcaM3YDF [,1:25]
pcaM3Y <- prcomp(pcaM3YDFvariables, center=TRUE, scale=TRUE)
summary(pcaM3Y)
PC1m3y = pcaM3Y$x[,1]
PC2m3y = pcaM3Y$x[,2]
PC3m3y = pcaM3Y$x[,3]
m3yPCAresults = data.frame(PC1m3y, PC2m3y, PC3m3y,pcaM3YDFvariables)
M3Ysimulated <- filter(yearly_exp2, datasource!="M3")
projectM3Ysimulated <- M3Ysimulated[,1:25]
simulatedPCAM3Y <- scale(projectM3Ysimulated,pcaM3Y$center, pcaM3Y$scale) %*% pcaM3Y$rotation
M3Ypca <- data.frame(PC1=PC1m3y, PC2=PC2m3y, PC3=PC3m3y)
rownames(M3Ypca) <- NULL
SimulatedPCAM3Y <- data.frame(PC1=simulatedPCAM3Y[,1], PC2=simulatedPCAM3Y[,2], PC3=simulatedPCAM3Y[,3])
M1Y <- filter(yearly_exp1, datasource=="M1")
projectM1Y <- M1Y[,1:25]
simM1YPCA <- scale(projectM1Y, pcaM3Y$center,pcaM3Y$scale) %*% pcaM3Y$rotation
M1YPCA <- data.frame(PC1=simM1YPCA[,1], PC2=simM1YPCA[,2], PC3=simM1YPCA[,3])
rfsubsample <- load("data/yearly_exp2_sub.rda")
projectM3RFdataSub <- yearly_exp2_sub[,1:25]
M3RFdataSubPCA <- scale(projectM3RFdataSub, pcaM3Y$center,pcaM3Y$scale) %*% pcaM3Y$rotation
subsamplePCA <- data.frame(PC1=M3RFdataSubPCA[,1], PC2=M3RFdataSubPCA[,2], PC3=M3RFdataSubPCA[,3])
pcaALLM3Y <- bind_rows(SimulatedPCAM3Y, subsamplePCA, M3Ypca,M1YPCA, .id="source")
pca1M3Y <- ggplot(pcaALLM3Y, aes(x=PC1, y=PC2, color=source)) + geom_point()+
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("Yearly")
ggsave("figures/pca1M3Y.png", width = 5, height = 4)
pca2M3Y <- ggplot(pcaALLM3Y, aes(x=PC1, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow" ,"black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca2M3Y.png", width = 5, height = 4)
pca3M3Y <- ggplot(pcaALLM3Y, aes(x=PC2, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1" ))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca3M3Y.png", width = 5, height = 4)

pcaM3QDF <- filter(quarterly_exp2, datasource=="M3") #Quaterly - E2
pcaM3QDFvariables <- pcaM3QDF[, 1:30]
pcaM3Q <- prcomp(pcaM3QDFvariables, center=TRUE, scale=TRUE)
summary(pcaM3Q)
PC1m3q = pcaM3Q$x[,1]
PC2m3q = pcaM3Q$x[,2]
PC3m3q = pcaM3Q$x[,3]
m3qPCAresults = data.frame(PC1m3q, PC2m3q, PC3m3q,pcaM3QDFvariables)
M3Qsimulated <- filter(quarterly_exp2, datasource!="M3")
projectM3Qsimulated <- M3Qsimulated[,1:30]
simulatedPCAM3Q <- scale(projectM3Qsimulated,pcaM3Q$center, pcaM3Q$scale) %*% pcaM3Q$rotation
M3Qpca <- data.frame(PC1=PC1m3q, PC2=PC2m3q, PC3=PC3m3q)
rownames(M3Qpca) <- NULL
SimulatedPCAM3Q <- data.frame(PC1=simulatedPCAM3Q[,1], PC2=simulatedPCAM3Q[,2], PC3=simulatedPCAM3Q[,3])
M1Q <- filter(quarterly_exp1, datasource=="M1")
projectM1Q <- M1Q[,1:30]
simM1QPCA <- scale(projectM1Q, pcaM3Q$center,pcaM3Q$scale) %*% pcaM3Q$rotation
M1QPCA <- data.frame(PC1=simM1QPCA[,1], PC2=simM1QPCA[,2], PC3=simM1QPCA[,3])
projectM3QRFdataSub <- quarterly_exp2_sub[,1:30]
M3QRFdataSubPCA <- scale(projectM3QRFdataSub, pcaM3Q$center,pcaM3Q$scale) %*% pcaM3Q$rotation
subsamplePCAQ <- data.frame(PC1=M3QRFdataSubPCA[,1], PC2=M3QRFdataSubPCA[,2], PC3=M3QRFdataSubPCA[,3])
pcaALLM3Q <- bind_rows(SimulatedPCAM3Q, subsamplePCAQ, M3Qpca, M1QPCA, .id="source")
pca1M3Q <- ggplot(pcaALLM3Q, aes(x=PC1, y=PC2, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("Quartely")
ggsave("figures/pca1M3Q.png", width = 5, height = 4)
pca2M3Q <- ggplot(pcaALLM3Q, aes(x=PC1, y=PC3, color=source)) + geom_point()+ 
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca2M3Q.png", width = 5, height = 4)
pca3M3Q <- ggplot(pcaALLM3Q, aes(x=PC2, y=PC3, color=source)) + geom_point()+
  scale_color_manual(values=c("forestgreen", "yellow", "black", "firebrick1"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca3M3Q.png", width = 5, height = 4)



pcaM3MDF <- filter(monthly_exp2, datasource=="M3") #Monthly - E2
pcaM3MDFvariables <- pcaM3MDF[,1:30]
pcaM3M <- prcomp(pcaM3MDFvariables, center=TRUE, scale=TRUE)
summary(pcaM3M)
PC1m3m = pcaM3M$x[,1]
PC2m3m = pcaM3M$x[,2]
PC3m3m = pcaM3M$x[,3]
m3mPCAresults = data.frame(PC1m3m, PC2m3m, PC3m3m,pcaM3MDFvariables)
M3Msimulated <- filter(monthly_exp2, datasource!="M3")
projectM3Msimulated <- M3Msimulated[,1:30]
simulatedPCAM3M <- scale(projectM3Msimulated,pcaM3M$center, pcaM3M$scale) %*% pcaM3M$rotation
M3Mpca <- data.frame(PC1=PC1m3m, PC2=PC2m3m, PC3=PC3m3m)
rownames(M3Mpca) <- NULL
SimulatedPCAM3M <- data.frame(PC1=simulatedPCAM3M[,1], PC2=simulatedPCAM3M[,2], PC3=simulatedPCAM3M[,3])
M1M <- filter(monthly_exp1, datasource=="M1")
projectM1M <- M1M[, 1:30]
simM1MPCA <- scale(projectM1M, pcaM3M$center,pcaM3M$scale) %*% pcaM3M$rotation
M1MPCA <- data.frame(PC1=simM1MPCA[,1], PC2=simM1MPCA[,2], PC3=simM1MPCA[,3])
pcaALLM3M <- bind_rows(SimulatedPCAM3M, M1MPCA, M3Mpca, .id="source")
pca1M3M <- ggplot(pcaALLM3M, aes(x=PC1, y=PC2, color=source)) + geom_point()+
	scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("Monthly")
ggsave("figures/pca1M3M.png", width = 5, height = 4)
pca2M3M <- ggplot(pcaALLM3M, aes(x=PC1, y=PC3, color=source)) + geom_point()+ 
	scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca2M3M.png", width = 5, height = 4)
pca3M3M <- ggplot(pcaALLM3M, aes(x=PC2, y=PC3, color=source)) + geom_point()+
	scale_color_manual(values=c("forestgreen", "firebrick1", "black"))+
  theme(aspect.ratio = 1, legend.position="none", plot.margin=grid::unit(c(0,0,0,0), "mm"), plot.title = element_text(hjust = 0.5, size = 20, face = "bold"))+ggtitle("")
ggsave("figures/pca3M3M.png", width = 5, height = 4)


