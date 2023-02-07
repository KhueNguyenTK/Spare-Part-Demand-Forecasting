# R script for cleaning, describing, and classifying eight data sets
# 
# List of data sets:
#   1, Four industrial data sets:
#         MAN: 3451 items, 150 weeks
#         BRAF: 5000 items, 7 years
#         AUTO: 3000 items, 2 years
#         OIL: 14523 items, 56 months
#   2, Four simulated data sets, each containing 6500 items 
#      whose demands are simulated for a period of 60 moths
#      (denoted by SIM1, SIM2, SIM3, SIM4)
#
# Note that:
#     AUTO and the four simulated data sets do not have information on 
#     individual prices (these are calculated in the Data Description section)
# 
# The raw data (RawData.RData) is extracted from the Excel file "Data.xlsx",
# which contains only prices & demands (and total installed for OIL).
# The full data can be found in folder "All Data sets".
# 
# Author: Khue Nguyen (Nguyen Hoang Thi Khue), <nguyen@ese.eur.nl>


library(readxl)
# LOADING DATA
load("RawData.RData")

########## DATA CLEANING ##########

# Extracting price & leadtime vectors (leadtimes are set to 1)
# Exceptions: AUTO + SIM's data-sets as they do not have price yet
# Exceptions: OIL - also extract total installed

pricesMAN <- MAN[,1]
leadtimesMAN <- 1
MAN <- MAN[,-1]  

pricesBRAF <- BRAF[,1]
leadtimesBRAF <- 1
BRAF <- BRAF[,-1]

leadtimesAUTO <- 1

pricesOIL <- OIL[,1]
totalOIL <- OIL[,2]
leadtimesOIL <- 1
OIL <- OIL[,-c(1,2)]

leadtimesSIM1 <- 1

leadtimesSIM2 <- 1

leadtimesSIM3 <- 1

leadtimesSIM4 <- 1

# 'Flip' AUTO + OIL (as these data sets start from the closest periods)
AUTO <- rev(AUTO)
OIL <- rev(OIL)

# Remove NOV-97 in OIL (column)
OIL <- OIL[,-11]

# Rename columns
colnames(MAN) <- c(1:150)
colnames(BRAF) <- c(1:84)
colnames(AUTO) <- c(1:24)
colnames(OIL) <- c(1:55)

# Set negative demands to 0
MAN <- pmax(MAN,0)
BRAF <- pmax(BRAF,0)
AUTO <- pmax(AUTO,0)
OIL <- pmax(OIL,0)

# OIL: If demand > total installed -> set demand to 0
for(i in 1:nrow(OIL)) {
  if(!is.na(totalOIL[i])) {
    OIL[i,] <- pmin(OIL[i,], totalOIL[i])
  }
}

# Set NA values to 0
MAN[is.na(MAN)] <- 0
BRAF[is.na(BRAF)] <- 0
AUTO[is.na(AUTO)] <- 0
OIL[is.na(OIL)] <- 0

# Transpose the data sets (col = item, row = time period)
MAN <- as.data.frame(t(MAN))
BRAF <- as.data.frame(t(BRAF))
AUTO <- as.data.frame(t(AUTO))
OIL <- as.data.frame(t(OIL))
SIM1 <- as.data.frame(t(SIM1))
SIM2 <- as.data.frame(t(SIM2))
SIM3 <- as.data.frame(t(SIM3))
SIM4 <- as.data.frame(t(SIM4))

# Split data sets
sampleMAN <- round(nrow(MAN)*.70)
trainMAN <- MAN[1:(sampleMAN),]
testMAN <- MAN[-(1:(sampleMAN)),]

sampleBRAF <- round(nrow(BRAF)*.70)
trainBRAF <- BRAF[1:(sampleBRAF),]
testBRAF <- BRAF[-(1:(sampleBRAF)),]

sampleAUTO <- round(nrow(AUTO)*.70)
trainAUTO <- AUTO[1:(sampleAUTO),]
testAUTO <- AUTO[-(1:(sampleAUTO)),]

sampleOIL <- round(nrow(OIL)*.70)
trainOIL <- OIL[1:(sampleOIL),]
testOIL <- OIL[-(1:(sampleOIL)),]

sampleSIM1 <- round(nrow(SIM1)*.70)
trainSIM1 <- SIM1[1:(sampleSIM1),]
testSIM1 <- SIM1[-(1:(sampleSIM1)),]

sampleSIM2 <- round(nrow(SIM2)*.70)
trainSIM2 <- SIM2[1:(sampleSIM2),]
testSIM2 <- SIM2[-(1:(sampleSIM2)),]

sampleSIM3 <- round(nrow(SIM3)*.70)
trainSIM3 <- SIM3[1:(sampleSIM3),]
testSIM3 <- SIM3[-(1:(sampleSIM3)),]

sampleSIM4 <- round(nrow(SIM4)*.70)
trainSIM4 <- SIM4[1:(sampleSIM4),]
testSIM4 <- SIM4[-(1:(sampleSIM4)),]

# Drop items which do not have > 1 demand occurrence (industrial only)
# Results: Drop 2059 from MAN and 6879 from OIL
cond1 <- colSums(trainMAN != 0, na.rm = TRUE) < 2
mask1 <- !(cond1)
trainMAN <- subset(trainMAN, select = mask1)
testMAN <- subset(testMAN, select = mask1)

cond2 <- colSums(trainBRAF != 0, na.rm = TRUE) < 2
mask2 <- !(cond2)
trainBRAF <- subset(trainBRAF, select = mask2)
testBRAF <- subset(testBRAF, select = mask2)

cond3 <- colSums(trainAUTO != 0, na.rm = TRUE) < 2
mask3 <- !(cond3)
trainAUTO <- subset(trainAUTO, select = mask3)
testAUTO <- subset(testAUTO, select = mask3)

cond4 <- colSums(trainOIL != 0, na.rm = TRUE) < 2
mask4 <- !(cond4)
trainOIL <- subset(trainOIL, select = mask4)
testOIL <- subset(testOIL, select = mask4)


# Store the old MAN + OIL data sets, as we drop items from these
MAN_full <- MAN
OIL_full <- OIL

MAN <- subset(MAN, select = mask1)
OIL <- subset(OIL, select = mask4)

# Drop the prices of the items that are removed
pricesMAN <- pricesMAN[mask1]
pricesOIL <- pricesOIL[mask4]





########## DATA DESCRIPTION ##########
#USE THE DATA AFTER DROPPING (NEW OIL AND MAN)

# Average monthly sales for each item
avgSalesPerItem_MAN <- colMeans(MAN) * 4 #As this data set has weekly sales
avgSalesPerItem_BRAF <- colMeans(BRAF)
avgSalesPerItem_AUTO <- colMeans(AUTO)
avgSalesPerItem_OIL <- colMeans(OIL)
avgSalesPerItem_SIM1 <- colMeans(SIM1)
avgSalesPerItem_SIM2 <- colMeans(SIM2)
avgSalesPerItem_SIM3 <- colMeans(SIM3)
avgSalesPerItem_SIM4 <- colMeans(SIM4)

# Industrial data sets (Product price for AUTO is not yet available)
industrial <- as.data.frame(c("MAN", "BRAF", "AUTO", "OIL"))
colnames(industrial) <- c("Data set")
industrial$MinSale <- c(min(avgSalesPerItem_MAN), min(avgSalesPerItem_BRAF), min(avgSalesPerItem_AUTO), min(avgSalesPerItem_OIL))
industrial$MeanSale <- c(mean(avgSalesPerItem_MAN), mean(avgSalesPerItem_BRAF), mean(avgSalesPerItem_AUTO), mean(avgSalesPerItem_OIL))
industrial$MaxSale <- c(max(avgSalesPerItem_MAN), max(avgSalesPerItem_BRAF), max(avgSalesPerItem_AUTO), max(avgSalesPerItem_OIL))
industrial$StDevSale <- c(sd(avgSalesPerItem_MAN), sd(avgSalesPerItem_BRAF), sd(avgSalesPerItem_AUTO), sd(avgSalesPerItem_OIL))

industrial$MinPrice <- c(min(pricesMAN), min(pricesBRAF), NA, min(pricesOIL))
industrial$MeanPrice <- c(mean(pricesMAN), mean(pricesBRAF), NA, mean(pricesOIL))
industrial$MaxPrice <- c(max(pricesMAN), max(pricesBRAF), NA, max(pricesOIL))
industrial$StDevPrice <- c(sd(pricesMAN), sd(pricesBRAF), NA, sd(pricesOIL))

# Calculate RPS
RPS <- c(industrial[1,7]/industrial[1,3], industrial[2,7]/industrial[2,3], NA, industrial[4,7]/industrial[4,3])
RPS[3] <- mean(RPS, na.rm = TRUE)
industrial$RPS <- RPS

# Calculate prices for AUTO
avgSales_AUTO <- mean(avgSalesPerItem_AUTO)
RMS_AUTO <- avgSalesPerItem_AUTO / avgSales_AUTO
avgItemPrice_AUTO <- RPS[3] * avgSales_AUTO 
pricesAUTO <- avgItemPrice_AUTO / RMS_AUTO

# Update the description table
industrial$MinPrice[3] <- min(pricesAUTO)
industrial$MeanPrice[3] <- RPS[3] * industrial$MeanSale[3]
industrial$MaxPrice[3] <- max(pricesAUTO)
industrial$StDevPrice[3] <- sd(pricesAUTO)



# Simulated data sets (Product price obtained by the same way for AUTO)
avgSales_SIM1 <- mean(avgSalesPerItem_SIM1)
RMS_SIM1 <- avgSalesPerItem_SIM1 / avgSales_SIM1
pricesSIM1 <- RPS[3] * avgSales_SIM1 / RMS_SIM1

avgSales_SIM2 <- mean(avgSalesPerItem_SIM2)
RMS_SIM2 <- avgSalesPerItem_SIM2 / avgSales_SIM2
pricesSIM2 <- RPS[3] * avgSales_SIM2 / RMS_SIM2

avgSales_SIM3 <- mean(avgSalesPerItem_SIM3)
RMS_SIM3 <- avgSalesPerItem_SIM3 / avgSales_SIM3
pricesSIM3 <- RPS[3] * avgSales_SIM3 / RMS_SIM3

avgSales_SIM4 <- mean(avgSalesPerItem_SIM4)
RMS_SIM4 <- avgSalesPerItem_SIM4 / avgSales_SIM4
pricesSIM4 <- RPS[3] * avgSales_SIM4 / RMS_SIM4

# Create table for simulated data sets
simulated <- as.data.frame(c("SIM1", "SIM2", "SIM3", "SIM4"))
colnames(simulated) <- c("Data set")
simulated$CV2 <- c(0.75, 0.80, 0.30, 0.25)
simulated$p <- c(1.00, 1.50, 1.05, 1.45)
simulated$MeanSale <- c(avgSales_SIM1, avgSales_SIM2, avgSales_SIM3, avgSales_SIM4)
simulated$StDevSale <- c(sd(avgSalesPerItem_SIM1), sd(avgSalesPerItem_SIM2), sd(avgSalesPerItem_SIM3), sd(avgSalesPerItem_SIM4))
simulated$MeanPrice <- c(RPS[3] * avgSales_SIM1, RPS[3] * avgSales_SIM2, RPS[3] * avgSales_SIM3, RPS[3] * avgSales_SIM4)
simulated$StDevPrice <- c(sd(pricesSIM1), sd(pricesSIM2), sd(pricesSIM3), sd(pricesSIM4))

# View the tables
View(industrial)
View(simulated)


# save(AUTO, BRAF, MAN, OIL, SIM1, SIM2, SIM3, SIM4,
#      testAUTO, testBRAF, testMAN, testOIL, testSIM1, testSIM2, testSIM3, testSIM4,
#      trainAUTO, trainBRAF, trainMAN, trainOIL, trainSIM1, trainSIM2, trainSIM3, trainSIM4,
#      cond1, cond2, cond3, cond4,
#      leadtimesAUTO, leadtimesBRAF, leadtimesMAN, leadtimesOIL, leadtimesSIM1, leadtimesSIM2, leadtimesSIM3, leadtimesSIM4,
#      mask1, mask2, mask3, mask4,
#      pricesAUTO, pricesBRAF, pricesMAN, pricesOIL, pricesSIM1, pricesSIM2, pricesSIM3, pricesSIM4,
#      sampleAUTO, sampleBRAF, sampleMAN, sampleOIL, sampleSIM1, sampleSIM2, sampleSIM3, sampleSIM4,
#      totalOIL,
#      file = "CleanedData.Rda")




########## DATA CLASSIFICATION ##########

library(dplyr, lib.loc = "C:/ProgramData/App-V/B047D8F3-34A7-477D-8417-728C45F5168F/464FFE20-99EE-475A-81C5-F623B973619E/Root/R/R-4.0.5/library")

# Change 0 values to NA to take NZD statistics
is.na(MAN) <- MAN == 0
is.na(BRAF) <- BRAF == 0
is.na(AUTO) <- AUTO == 0
is.na(OIL) <- OIL == 0
is.na(SIM1) <- SIM1 == 0
is.na(SIM2) <- SIM2 == 0
is.na(SIM3) <- SIM3 == 0
is.na(SIM4) <- SIM4 == 0

# Average NZD
avgNZD_MAN <- colMeans(MAN, na.rm = TRUE)
avgNZD_BRAF <- colMeans(BRAF, na.rm = TRUE)
avgNZD_AUTO <- colMeans(AUTO, na.rm = TRUE)
avgNZD_OIL <- colMeans(OIL, na.rm = TRUE)
avgNZD_SIM1 <- colMeans(SIM1, na.rm = TRUE)
avgNZD_SIM2 <- colMeans(SIM2, na.rm = TRUE)
avgNZD_SIM3 <- colMeans(SIM3, na.rm = TRUE)
avgNZD_SIM4 <- colMeans(SIM4, na.rm = TRUE)


# SD NZD
colSD_NZD <- function(data) {
  output <- c(length = nrow(data))
  for(i in 1:ncol(data)) {
    output[i] <- sd(data[,i], na.rm = TRUE)
  }
  n <- colSums(!is.na(data))
  output <- output * sqrt((n-1)/n)
  return(output)
}
sdNZD_MAN <- colSD_NZD(MAN)
sdNZD_BRAF <- colSD_NZD(BRAF)
sdNZD_AUTO <- colSD_NZD(AUTO)
sdNZD_OIL <- colSD_NZD(OIL)
sdNZD_SIM1 <- colSD_NZD(SIM1)
sdNZD_SIM2 <- colSD_NZD(SIM2)
sdNZD_SIM3 <- colSD_NZD(SIM3)
sdNZD_SIM4 <- colSD_NZD(SIM4)

# CV2
CV2_MAN <- (sdNZD_MAN / avgNZD_MAN)^2
CV2_BRAF <- (sdNZD_BRAF / avgNZD_BRAF)^2
CV2_AUTO <- (sdNZD_AUTO / avgNZD_AUTO)^2
CV2_OIL <- (sdNZD_OIL / avgNZD_OIL)^2
CV2_SIM1 <- (sdNZD_SIM1 / avgNZD_SIM1)^2
CV2_SIM2 <- (sdNZD_SIM2 / avgNZD_SIM2)^2
CV2_SIM3 <- (sdNZD_SIM3 / avgNZD_SIM3)^2
CV2_SIM4 <- (sdNZD_SIM4 / avgNZD_SIM4)^2

# p
p_MAN <- nrow(MAN) / colSums(!is.na(MAN))
p_BRAF <- nrow(BRAF) / colSums(!is.na(BRAF))
p_AUTO <- nrow(AUTO) / colSums(!is.na(AUTO))
p_OIL <- nrow(OIL) / colSums(!is.na(OIL))
p_SIM1 <- nrow(SIM1) / colSums(!is.na(SIM1))
p_SIM2 <- nrow(SIM2) / colSums(!is.na(SIM2))
p_SIM3 <- nrow(SIM3) / colSums(!is.na(SIM3))
p_SIM4 <- nrow(SIM4) / colSums(!is.na(SIM4))

# Classify
classifyDemand <- function(CV2, p) {
  output <- as.data.frame(cbind(CV2, p))
  output$Type <- case_when(p < 1.32 & CV2 >= 0.49 ~ 'Erratic',
                           p >= 1.32 & CV2 >= 0.49 ~ 'Lumpy',
                           p < 1.32 & CV2 < 0.49 ~ 'Smooth',
                           p >= 1.32 & CV2 < 0.49 ~ 'Intermittent')
  return(output)
}
classify_MAN <- classifyDemand(CV2_MAN, p_MAN)
classify_BRAF <- classifyDemand(CV2_BRAF, p_BRAF)
classify_AUTO <- classifyDemand(CV2_AUTO, p_AUTO)
classify_OIL <- classifyDemand(CV2_OIL, p_OIL)
classify_SIM1 <- classifyDemand(CV2_SIM1, p_SIM1)
classify_SIM2 <- classifyDemand(CV2_SIM2, p_SIM2)
classify_SIM3 <- classifyDemand(CV2_SIM3, p_SIM3)
classify_SIM4 <- classifyDemand(CV2_SIM4, p_SIM4)

#dataClassification <- as.data.frame(c("MAN", "BRAF", "AUTO", "OIL", "SIM1", "SIM2", "SIM3", "SIM4"))
#colnames(simulated) <- c("Data set")
#dataClassification$CV2 <- rbind(c(mean(CV2_MAN), MEAN(CV2_BRAF), mean(CV2_AUTO), mean(CV2_OIL)), simulated$CV2)
#dataClassification$p <- rbind(c(mean(p_MAN), MEAN(p_BRAF), mean(p_AUTO), mean(p_OIL)), simulated$p)

table(classify_MAN$Type)
table(classify_BRAF$Type)
table(classify_AUTO$Type)
table(classify_OIL$Type)
table(classify_SIM1$Type)
table(classify_SIM2$Type)
table(classify_SIM3$Type)
table(classify_SIM4$Type)







########## CLASSIFICATION USING ONLY THE TRAINING SETS (tr_) ##########

# Change 0 values to NA to take NZD statistics
is.na(trainMAN) <- trainMAN == 0
is.na(trainBRAF) <- trainBRAF == 0
is.na(trainAUTO) <- trainAUTO == 0
is.na(trainOIL) <- trainOIL == 0
is.na(trainSIM1) <- trainSIM1 == 0
is.na(trainSIM2) <- trainSIM2 == 0
is.na(trainSIM3) <- trainSIM3 == 0
is.na(trainSIM4) <- trainSIM4 == 0

# Average NZD
tr_avgNZD_MAN <- colMeans(trainMAN, na.rm = TRUE)
tr_avgNZD_BRAF <- colMeans(trainBRAF, na.rm = TRUE)
tr_avgNZD_AUTO <- colMeans(trainAUTO, na.rm = TRUE)
tr_avgNZD_OIL <- colMeans(trainOIL, na.rm = TRUE)
tr_avgNZD_SIM1 <- colMeans(trainSIM1, na.rm = TRUE)
tr_avgNZD_SIM2 <- colMeans(trainSIM2, na.rm = TRUE)
tr_avgNZD_SIM3 <- colMeans(trainSIM3, na.rm = TRUE)
tr_avgNZD_SIM4 <- colMeans(trainSIM4, na.rm = TRUE)


# SD NZD
tr_sdNZD_MAN <- colSD_NZD(trainMAN)
tr_sdNZD_BRAF <- colSD_NZD(trainBRAF)
tr_sdNZD_AUTO <- colSD_NZD(trainAUTO)
tr_sdNZD_OIL <- colSD_NZD(trainOIL)
tr_sdNZD_SIM1 <- colSD_NZD(trainSIM1)
tr_sdNZD_SIM2 <- colSD_NZD(trainSIM2)
tr_sdNZD_SIM3 <- colSD_NZD(trainSIM3)
tr_sdNZD_SIM4 <- colSD_NZD(trainSIM4)


# CV2 (using MAN and OIL)
tr_CV2_MAN <- (tr_sdNZD_MAN / tr_avgNZD_MAN)^2
tr_CV2_BRAF <- (tr_sdNZD_BRAF / tr_avgNZD_BRAF)^2
tr_CV2_AUTO <- (tr_sdNZD_AUTO / tr_avgNZD_AUTO)^2
tr_CV2_OIL <- (tr_sdNZD_OIL / tr_avgNZD_OIL)^2
tr_CV2_SIM1 <- (tr_sdNZD_SIM1 / tr_avgNZD_SIM1)^2
tr_CV2_SIM2 <- (tr_sdNZD_SIM2 / tr_avgNZD_SIM2)^2
tr_CV2_SIM3 <- (tr_sdNZD_SIM3 / tr_avgNZD_SIM3)^2
tr_CV2_SIM4 <- (tr_sdNZD_SIM4 / tr_avgNZD_SIM4)^2

# p
tr_p_MAN <- nrow(trainMAN) / colSums(!is.na(trainMAN))
tr_p_BRAF <- nrow(trainBRAF) / colSums(!is.na(trainBRAF))
tr_p_AUTO <- nrow(trainAUTO) / colSums(!is.na(trainAUTO))
tr_p_OIL <- nrow(trainOIL) / colSums(!is.na(trainOIL))
tr_p_SIM1 <- nrow(trainSIM1) / colSums(!is.na(trainSIM1))
tr_p_SIM2 <- nrow(trainSIM2) / colSums(!is.na(trainSIM2))
tr_p_SIM3 <- nrow(trainSIM3) / colSums(!is.na(trainSIM3))
tr_p_SIM4 <- nrow(trainSIM4) / colSums(!is.na(trainSIM4))

# Classify
tr_classify_MAN <- classifyDemand(tr_CV2_MAN, tr_p_MAN)
tr_classify_BRAF <- classifyDemand(tr_CV2_BRAF, tr_p_BRAF)
tr_classify_AUTO <- classifyDemand(tr_CV2_AUTO, tr_p_AUTO)
tr_classify_OIL <- classifyDemand(tr_CV2_OIL, tr_p_OIL)
tr_classify_SIM1 <- classifyDemand(tr_CV2_SIM1, tr_p_SIM1)
tr_classify_SIM2 <- classifyDemand(tr_CV2_SIM2, tr_p_SIM2)
tr_classify_SIM3 <- classifyDemand(tr_CV2_SIM3, tr_p_SIM3)
tr_classify_SIM4 <- classifyDemand(tr_CV2_SIM4, tr_p_SIM4)

#dataClassification <- as.data.frame(c("MAN", "BRAF", "AUTO", "OIL", "SIM1", "SIM2", "SIM3", "SIM4"))
#colnames(simulated) <- c("Data set")
#dataClassification$CV2 <- rbind(c(mean(CV2_MAN), MEAN(CV2_BRAF), mean(CV2_AUTO), mean(CV2_OIL)), simulated$CV2)
#dataClassification$p <- rbind(c(mean(p_MAN), MEAN(p_BRAF), mean(p_AUTO), mean(p_OIL)), simulated$p)

table(tr_classify_MAN$Type)
table(tr_classify_BRAF$Type)
table(tr_classify_AUTO$Type)
table(tr_classify_OIL$Type)
table(tr_classify_SIM1$Type)
table(tr_classify_SIM2$Type)
table(tr_classify_SIM3$Type)
table(tr_classify_SIM4$Type)

