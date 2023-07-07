# R script for the inventory performance of seven different methods in forecasting spare part demand
# 
# We consider three different distributions: Normal, Gamma, and Negative Binomial
# 
# List of Methods:
#   1, Croston
#   2, Simple Exponential Smoothing (SES)
#   3, Syntetos and Boylan (SBA)
#   4, Teunter-Syntetos-Babai (TSB)
#   5, Willemain
#   6, Multi-Layer Perception (MLP)
#   7, LightGBM
#
# Author:
# Khue Nguyen (Nguyen Hoang Thi Khue), <nguyen@ese.eur.nl>

targetFillRates <- c(75:99)
targetFillRates <- targetFillRates / 100
nTFR <- length(targetFillRates)

####################################################
### ESC NORMAL ###

ESC_NORM <- function(mu, sigma, R) {
  # return(sigma * dnorm((R - mu) / sigma) + (mu - R) * (1 - pnorm(R, mu, sigma)))
  return(sigma^2 * dnorm(R, mu, sigma) + (mu - R) * (1 - pnorm(R, mu, sigma)))
}

#General Normal without maxR
IPM_NORM <- function(data, train_data, test_data, predictions, prices, data_name, Method) {
  # Containing fill rate for each item for each TFR (col ~ item, row ~ TFR)
  # Each entry = total supply / total demand of an item for a TFR
  FillRates <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(FillRates) <- colnames(data)
  rownames(FillRates) <- targetFillRates
  
  # Containing total supply for each item for each TFR (col ~ item, row ~ TFR)
  TotalSupply <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(TotalSupply) <- colnames(data)
  rownames(TotalSupply) <- targetFillRates

  # Containing total demand for each item
  TotalDemand <- colSums(test_data)
    
  # Containing average holding cost for each item for each TFR (col ~ item, row ~ TFR)
  HoldingCosts <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(HoldingCosts) <- colnames(data)
  rownames(HoldingCosts) <- targetFillRates
  
  t = nrow(train_data)
  h = nrow(test_data)
  
  for (i in 1:ncol(test_data)) {
    for (j in 1:nTFR) {
      r <- targetFillRates[j]
      inventory <- data.frame(TFR = rep(r, length = h), Sigma = NA, Mu = NA,
                              R = NA, Q = NA, ILpreD = NA, D = NA, S = NA, ILpostD = NA, Cost = NA)
      inventory$D <- rep(test_data[,i])
      for (k in 1:h) {
        inventory$Sigma[k] <- sd(data[1:(t+k-1),i])
      }
      inventory$Mu <- predictions[,i]
      
      for (k in 1:h) {
        if (inventory$Mu[k] < 0) {
          inventory$R[k] <- 0
          next
        }
        # maxR <- max(data[1:(t+k-1),i])
        R <- 0
        lossTarget <- (1 - r) * inventory$Mu[k]
        tempQ <- inventory$Mu[k]
        shortagePerCycle <- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R) #- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R + tempQ)
        while (shortagePerCycle > lossTarget) {
          R <- R + 1
          # if (R == maxR) {
          #   break
          # }
          shortagePerCycle <- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R) #- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R + tempQ)
        }
        inventory$R[k] <- R
      }
      
      inventory$Q[1] <- 0                              # Initial order quantity
      inventory$ILpreD[1] <- inventory$R[1]
      inventory$S[1] <- min(inventory$ILpreD[1], inventory$D[1])
      inventory$ILpostD[1] <- inventory$ILpreD[1] - inventory$D[1]
      
      for (k in 2:h) {
        inventory$Q[k] <- max(0, inventory$R[k] - inventory$ILpostD[k-1])
        inventory$ILpreD[k] <- max(inventory$ILpostD[k-1], inventory$R[k])
        inventory$S[k] <- min(inventory$ILpreD[k], inventory$D[k])
        inventory$ILpostD[k] <- inventory$ILpreD[k] - inventory$D[k]
      }
      
      Cost <- 0.25 * inventory$ILpreD * prices[i]
      Cost[Cost < 0] <- 0
      inventory$Cost <- Cost
      
      if (TotalDemand[i] == 0) {
        FillRates[j,i] <- 0
      } else {
        FillRates[j,i] <- sum(inventory$S) / sum(inventory$D)
      }
      TotalSupply[j,i] <- sum(inventory$S)
      HoldingCosts[j,i] <- mean(inventory$Cost)
    } # end loop r
    print(paste("Done", i))
  } # end loop i
  
  totalAFR <- rowSums(TotalSupply) / sum(TotalDemand)
  avgAFR <- rowMeans(FillRates)
  
  # Holding Cost (for each target fill rate) is the sum of holding costs of all items
  holdingCost <- rowSums(HoldingCosts)
  
  # Service Level data frame
  ServiceLevel <- data.frame(avgAFR, totalAFR, holdingCost, targetFillRates, Method)
  colnames(ServiceLevel) <- c("AchievedFillRates_Avg", "AchievedFillRates_Total", "HoldingCosts", "TargetFillRates", "Method")
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/IPM_L0/ESC2/NORMAL_IPM_", Method, "_", data_name, ".Rda", sep = "")
  save(ServiceLevel, FillRates, TotalDemand, TotalSupply, file = file_name)
  
  return(ServiceLevel)
}

# NORM2: Take into account maxR
IPM_NORM <- function(data, train_data, test_data, predictions, prices, data_name, Method) {
  # Containing fill rate for each item for each TFR (col ~ item, row ~ TFR)
  # Each entry = total supply / total demand of an item for a TFR
  FillRates <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(FillRates) <- colnames(data)
  rownames(FillRates) <- targetFillRates
  
  # Containing total supply for each item for each TFR (col ~ item, row ~ TFR)
  TotalSupply <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(TotalSupply) <- colnames(data)
  rownames(TotalSupply) <- targetFillRates
  
  # Containing total demand for each item
  TotalDemand <- colSums(test_data)
  
  # Containing average holding cost for each item for each TFR (col ~ item, row ~ TFR)
  HoldingCosts <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(HoldingCosts) <- colnames(data)
  rownames(HoldingCosts) <- targetFillRates
  
  t = nrow(train_data)
  h = nrow(test_data)
  
  for (i in 1:ncol(test_data)) {
    for (j in 1:nTFR) {
      r <- targetFillRates[j]
      inventory <- data.frame(TFR = rep(r, length = h), Sigma = NA, Mu = NA,
                              R = NA, Q = NA, ILpreD = NA, D = NA, S = NA, ILpostD = NA, Cost = NA)
      inventory$D <- rep(test_data[,i])
      for (k in 1:h) {
        inventory$Sigma[k] <- sd(data[1:(t+k-1),i])
      }
      inventory$Mu <- predictions[,i]
      
      for (k in 1:h) {
        if (inventory$Mu[k] < 0) {
          inventory$R[k] <- 0
          next
        }
        maxR <- max(data[1:(t+k-1),i])
        R <- 0
        lossTarget <- (1 - r) * inventory$Mu[k]
        tempQ <- inventory$Mu[k]
        shortagePerCycle <- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R) #- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R + tempQ)
        while (shortagePerCycle > lossTarget) {
          R <- R + 1
          if (R == maxR) {
            break
          }
          shortagePerCycle <- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R) #- ESC_NORM(inventory$Mu[k], inventory$Sigma[k], R + tempQ)
        }
        inventory$R[k] <- R
      }
      
      inventory$Q[1] <- 0                              # Initial order quantity
      inventory$ILpreD[1] <- inventory$R[1]
      inventory$S[1] <- min(inventory$ILpreD[1], inventory$D[1])
      inventory$ILpostD[1] <- inventory$ILpreD[1] - inventory$D[1]
      
      for (k in 2:h) {
        inventory$Q[k] <- max(0, inventory$R[k] - inventory$ILpostD[k-1])
        inventory$ILpreD[k] <- max(inventory$ILpostD[k-1], inventory$R[k])
        inventory$S[k] <- min(inventory$ILpreD[k], inventory$D[k])
        inventory$ILpostD[k] <- inventory$ILpreD[k] - inventory$D[k]
      }
      
      Cost <- 0.25 * inventory$ILpreD * prices[i]
      Cost[Cost < 0] <- 0
      inventory$Cost <- Cost
      
      if (TotalDemand[i] == 0) {
        FillRates[j,i] <- 0
      } else {
        FillRates[j,i] <- sum(inventory$S) / sum(inventory$D)
      }
      TotalSupply[j,i] <- sum(inventory$S)
      HoldingCosts[j,i] <- mean(inventory$Cost)
    } # end loop r
    print(paste("Done", i))
  } # end loop i
  
  totalAFR <- rowSums(TotalSupply) / sum(TotalDemand)
  avgAFR <- rowMeans(FillRates)
  
  # Holding Cost (for each target fill rate) is the sum of holding costs of all items
  holdingCost <- rowSums(HoldingCosts)
  
  # Service Level data frame
  ServiceLevel <- data.frame(avgAFR, totalAFR, holdingCost, targetFillRates, Method)
  colnames(ServiceLevel) <- c("AchievedFillRates_Avg", "AchievedFillRates_Total", "HoldingCosts", "TargetFillRates", "Method")
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/IPM_L0/ESC2/NORMAL2_IPM_", Method, "_", data_name, ".Rda", sep = "")
  save(ServiceLevel, FillRates, TotalDemand, TotalSupply, file = file_name)
  
  return(ServiceLevel)
}

set.seed(8377)
Croston_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, Croston_SIM1, pricesSIM1, "SIM1" , "Croston")
SES_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, SES_SIM1, pricesSIM1, "SIM1" , "SES")
SBA_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, SBA_SIM1, pricesSIM1, "SIM1" , "SBA")
TSB_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, TSB_SIM1, pricesSIM1, "SIM1" , "TSB")
MLP_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, MLP_SIM1, pricesSIM1, "SIM1" , "MLP")
LightGBM_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, LightGBM_SIM1, pricesSIM1, "SIM1" , "LightGBM")
Willemain_IPM_NORM_SIM1 <- IPM_NORM(SIM1, trainSIM1, testSIM1, Willemain_SIM1, pricesSIM1, "SIM1" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, Croston_SIM2, pricesSIM2, "SIM2" , "Croston")
SES_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, SES_SIM2, pricesSIM2, "SIM2" , "SES")
SBA_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, SBA_SIM2, pricesSIM2, "SIM2" , "SBA")
TSB_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, TSB_SIM2, pricesSIM2, "SIM2" , "TSB")
MLP_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, MLP_SIM2, pricesSIM2, "SIM2" , "MLP")
LightGBM_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, LightGBM_SIM2, pricesSIM2, "SIM2" , "LightGBM")
Willemain_IPM_NORM_SIM2 <- IPM_NORM(SIM2, trainSIM2, testSIM2, Willemain_SIM2, pricesSIM2, "SIM2" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, Croston_SIM3, pricesSIM3, "SIM3" , "Croston")
SES_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, SES_SIM3, pricesSIM3, "SIM3" , "SES")
SBA_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, SBA_SIM3, pricesSIM3, "SIM3" , "SBA")
TSB_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, TSB_SIM3, pricesSIM3, "SIM3" , "TSB")
MLP_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, MLP_SIM3, pricesSIM3, "SIM3" , "MLP")
LightGBM_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, LightGBM_SIM3, pricesSIM3, "SIM3" , "LightGBM")
Willemain_IPM_NORM_SIM3 <- IPM_NORM(SIM3, trainSIM3, testSIM3, Willemain_SIM3, pricesSIM3, "SIM3" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, Croston_SIM4, pricesSIM4, "SIM4" , "Croston")
SES_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, SES_SIM4, pricesSIM4, "SIM4" , "SES")
SBA_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, SBA_SIM4, pricesSIM4, "SIM4" , "SBA")
TSB_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, TSB_SIM4, pricesSIM4, "SIM4" , "TSB")
MLP_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, MLP_SIM4, pricesSIM4, "SIM4" , "MLP")
LightGBM_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, LightGBM_SIM4, pricesSIM4, "SIM4" , "LightGBM")
Willemain_IPM_NORM_SIM4 <- IPM_NORM(SIM4, trainSIM4, testSIM4, Willemain_SIM4, pricesSIM4, "SIM4" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, Croston_MAN, pricesMAN, "MAN" , "Croston")
SES_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, SES_MAN, pricesMAN, "MAN" , "SES")
SBA_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, SBA_MAN, pricesMAN, "MAN" , "SBA")
TSB_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, TSB_MAN, pricesMAN, "MAN" , "TSB")
MLP_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, MLP_MAN, pricesMAN, "MAN" , "MLP")
LightGBM_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, LightGBM_MAN, pricesMAN, "MAN" , "LightGBM")
Willemain_IPM_NORM_MAN <- IPM_NORM(MAN, trainMAN, testMAN, Willemain_MAN, pricesMAN, "MAN" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, Croston_BRAF, pricesBRAF, "BRAF" , "Croston")
SES_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, SES_BRAF, pricesBRAF, "BRAF" , "SES")
SBA_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, SBA_BRAF, pricesBRAF, "BRAF" , "SBA")
TSB_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, TSB_BRAF, pricesBRAF, "BRAF" , "TSB")
MLP_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, MLP_BRAF, pricesBRAF, "BRAF" , "MLP")
LightGBM_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, LightGBM_BRAF, pricesBRAF, "BRAF" , "LightGBM")
Willemain_IPM_NORM_BRAF <- IPM_NORM(BRAF, trainBRAF, testBRAF, Willemain_BRAF, pricesBRAF, "BRAF" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, Croston_AUTO, pricesAUTO, "AUTO" , "Croston")
SES_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, SES_AUTO, pricesAUTO, "AUTO" , "SES")
SBA_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, SBA_AUTO, pricesAUTO, "AUTO" , "SBA")
TSB_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, TSB_AUTO, pricesAUTO, "AUTO" , "TSB")
MLP_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, MLP_AUTO, pricesAUTO, "AUTO" , "MLP")
LightGBM_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, LightGBM_AUTO, pricesAUTO, "AUTO" , "LightGBM")
Willemain_IPM_NORM_AUTO <- IPM_NORM(AUTO, trainAUTO, testAUTO, Willemain_AUTO, pricesAUTO, "AUTO" , "Willemain")

set.seed(8377)
Croston_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, Croston_OIL, pricesOIL, "OIL" , "Croston")
SES_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, SES_OIL, pricesOIL, "OIL" , "SES")
SBA_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, SBA_OIL, pricesOIL, "OIL" , "SBA")
TSB_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, TSB_OIL, pricesOIL, "OIL" , "TSB")
MLP_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, MLP_OIL, pricesOIL, "OIL" , "MLP")
LightGBM_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, LightGBM_OIL, pricesOIL, "OIL" , "LightGBM")
Willemain_IPM_NORM_OIL <- IPM_NORM(OIL, trainOIL, testOIL, Willemain_OIL, pricesOIL, "OIL" , "Willemain")

####################################################
### ESC GAMMA ###

ESC_GAMMA <- function(k, alpha, R) {
  ESC = k/alpha - R - k/alpha * pgamma(alpha*R, k+1, 1) + R * pgamma(alpha*R, k, 1)
  return(ESC)
}

IPM_GAMMA <- function(data, train_data, test_data, predictions, prices, data_name, Method) {
  # Containing fill rate for each item for each TFR (col ~ item, row ~ TFR)
  # Each entry = total supply / total demand of an item for a TFR
  FillRates <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(FillRates) <- colnames(data)
  rownames(FillRates) <- targetFillRates
  
  # Containing total supply for each item for each TFR (col ~ item, row ~ TFR)
  TotalSupply <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(TotalSupply) <- colnames(data)
  rownames(TotalSupply) <- targetFillRates
  
  # Containing total demand for each item
  TotalDemand <- colSums(test_data)
  
  # Containing average holding cost for each item for each TFR (col ~ item, row ~ TFR)
  HoldingCosts <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(HoldingCosts) <- colnames(data)
  rownames(HoldingCosts) <- targetFillRates
  
  t = nrow(train_data)
  h = nrow(test_data)
  
  for (i in 1:ncol(test_data)) {
    for (j in 1:nTFR) {
      r <- targetFillRates[j]
      inventory <- data.frame(TFR = rep(r, length = h), Sigma = NA, Mu = NA, K = NA, Lambda = NA,
                              R = NA, Q = NA, ILpreD = NA, D = NA, S = NA, ILpostD = NA, Cost = NA)
      inventory$D <- rep(test_data[,i])
      for (k in 1:h) {
        inventory$Sigma[k] <- sd(data[1:(t+k-1),i])
      }
      inventory$Mu <- predictions[,i]
      inventory$K      <- inventory$Mu^2 / inventory$Sigma^2   # shape
      inventory$Lambda <- inventory$Sigma^2 / inventory$Mu     # scale
      alpha  <- 1 / inventory$Lambda                           # rate
      
      for (g in 1:h) {
        if (inventory$Mu[g] <= 0) {
          inventory$R[g] <- 0
          next
        }
        maxR <- round(max(data[1:(t+g-1),i]))
        R <- 0
        lossTarget <- (1 - r) * inventory$Mu[g]
        tempQ <- inventory$Mu[g]
        shortagePerCycle <- ESC_GAMMA(inventory$K[g], alpha[g], R) #- ESC_GAMMA(inventory$K[g], alpha[g], R + tempQ)
        while (shortagePerCycle > lossTarget) {
          R <- R + 1
          if (R == maxR) {
            break
          }
          shortagePerCycle <- ESC_GAMMA(inventory$K[g], alpha[g], R) #- ESC_GAMMA(inventory$K[g], alpha[g], R + tempQ)
        }
        inventory$R[g] <- R
      }
      
      inventory$Q[1] <- 0                              # Initial order quantity
      inventory$ILpreD[1] <- inventory$R[1]
      inventory$S[1] <- min(inventory$ILpreD[1], inventory$D[1])
      inventory$ILpostD[1] <- inventory$ILpreD[1] - inventory$D[1]
      
      for (g in 2:h) {
        inventory$Q[g] <- max(0, inventory$R[g] - inventory$ILpostD[g-1])
        inventory$ILpreD[g] <- max(inventory$ILpostD[g-1], inventory$R[g])
        inventory$S[g] <- min(inventory$ILpreD[g], inventory$D[g])
        inventory$ILpostD[g] <- inventory$ILpreD[g] - inventory$D[g]
      }
      
      Cost <- 0.25 * inventory$ILpreD * prices[i]
      Cost[Cost < 0] <- 0
      inventory$Cost <- Cost
      
      if (TotalDemand[i] == 0) {
        FillRates[j,i] <- 0
      } else {
        FillRates[j,i] <- sum(inventory$S) / sum(inventory$D)
      }
      TotalSupply[j,i] <- sum(inventory$S)
      HoldingCosts[j,i] <- mean(inventory$Cost)
    } # end loop r
    print(paste("Done", i))
  } # end loop i
  
  totalAFR <- rowSums(TotalSupply) / sum(TotalDemand)
  avgAFR <- rowMeans(FillRates)
  
  # Holding Cost (for each target fill rate) is the sum of holding costs of all items
  holdingCost <- rowSums(HoldingCosts)
  
  # Service Level data frame
  ServiceLevel <- data.frame(avgAFR, totalAFR, holdingCost, targetFillRates, Method)
  colnames(ServiceLevel) <- c("AchievedFillRates_Avg", "AchievedFillRates_Total", "HoldingCosts", "TargetFillRates", "Method")
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/IPM_L0/ESC2/GAMMA_IPM_", Method, "_", data_name, ".Rda", sep = "")
  save(ServiceLevel, FillRates, TotalDemand, TotalSupply, file = file_name)
  
  return(ServiceLevel)
}

set.seed(8377)
Croston_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, Croston_SIM1, pricesSIM1, "SIM1" , "Croston")
SES_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, SES_SIM1, pricesSIM1, "SIM1" , "SES")
SBA_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, SBA_SIM1, pricesSIM1, "SIM1" , "SBA")
TSB_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, TSB_SIM1, pricesSIM1, "SIM1" , "TSB")
MLP_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, MLP_SIM1, pricesSIM1, "SIM1" , "MLP")
LightGBM_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, LightGBM_SIM1, pricesSIM1, "SIM1" , "LightGBM")
Willemain_IPM_GAMMA_SIM1 <- IPM_GAMMA(SIM1, trainSIM1, testSIM1, Willemain_SIM1, pricesSIM1, "SIM1" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, Croston_SIM2, pricesSIM2, "SIM2" , "Croston")
SES_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, SES_SIM2, pricesSIM2, "SIM2" , "SES")
SBA_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, SBA_SIM2, pricesSIM2, "SIM2" , "SBA")
TSB_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, TSB_SIM2, pricesSIM2, "SIM2" , "TSB")
MLP_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, MLP_SIM2, pricesSIM2, "SIM2" , "MLP")
LightGBM_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, LightGBM_SIM2, pricesSIM2, "SIM2" , "LightGBM")
Willemain_IPM_GAMMA_SIM2 <- IPM_GAMMA(SIM2, trainSIM2, testSIM2, Willemain_SIM2, pricesSIM2, "SIM2" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, Croston_SIM3, pricesSIM3, "SIM3" , "Croston")
SES_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, SES_SIM3, pricesSIM3, "SIM3" , "SES")
SBA_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, SBA_SIM3, pricesSIM3, "SIM3" , "SBA")
TSB_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, TSB_SIM3, pricesSIM3, "SIM3" , "TSB")
MLP_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, MLP_SIM3, pricesSIM3, "SIM3" , "MLP")
LightGBM_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, LightGBM_SIM3, pricesSIM3, "SIM3" , "LightGBM")
Willemain_IPM_GAMMA_SIM3 <- IPM_GAMMA(SIM3, trainSIM3, testSIM3, Willemain_SIM3, pricesSIM3, "SIM3" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, Croston_SIM4, pricesSIM4, "SIM4" , "Croston")
SES_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, SES_SIM4, pricesSIM4, "SIM4" , "SES")
SBA_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, SBA_SIM4, pricesSIM4, "SIM4" , "SBA")
TSB_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, TSB_SIM4, pricesSIM4, "SIM4" , "TSB")
MLP_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, MLP_SIM4, pricesSIM4, "SIM4" , "MLP")
LightGBM_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, LightGBM_SIM4, pricesSIM4, "SIM4" , "LightGBM")
Willemain_IPM_GAMMA_SIM4 <- IPM_GAMMA(SIM4, trainSIM4, testSIM4, Willemain_SIM4, pricesSIM4, "SIM4" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, Croston_MAN, pricesMAN, "MAN" , "Croston")
SES_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, SES_MAN, pricesMAN, "MAN" , "SES")
SBA_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, SBA_MAN, pricesMAN, "MAN" , "SBA")
TSB_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, TSB_MAN, pricesMAN, "MAN" , "TSB")
MLP_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, MLP_MAN, pricesMAN, "MAN" , "MLP")
LightGBM_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, LightGBM_MAN, pricesMAN, "MAN" , "LightGBM")
Willemain_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, Willemain_MAN, pricesMAN, "MAN" , "Willemain")
# Willemain_IPM_GAMMA_MAN <- IPM_GAMMA(MAN, trainMAN, testMAN, Willemain_MAN_p1, pricesMAN, "MAN" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, Croston_BRAF, pricesBRAF, "BRAF" , "Croston")
SES_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, SES_BRAF, pricesBRAF, "BRAF" , "SES")
SBA_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, SBA_BRAF, pricesBRAF, "BRAF" , "SBA")
TSB_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, TSB_BRAF, pricesBRAF, "BRAF" , "TSB")
MLP_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, MLP_BRAF, pricesBRAF, "BRAF" , "MLP")
LightGBM_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, LightGBM_BRAF, pricesBRAF, "BRAF" , "LightGBM")
Willemain_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, Willemain_BRAF, pricesBRAF, "BRAF" , "Willemain")
# Willemain_IPM_GAMMA_BRAF <- IPM_GAMMA(BRAF, trainBRAF, testBRAF, Willemain_BRAF, pricesBRAF, "BRAF" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, Croston_AUTO, pricesAUTO, "AUTO" , "Croston")
SES_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, SES_AUTO, pricesAUTO, "AUTO" , "SES")
SBA_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, SBA_AUTO, pricesAUTO, "AUTO" , "SBA")
TSB_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, TSB_AUTO, pricesAUTO, "AUTO" , "TSB")
MLP_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, MLP_AUTO, pricesAUTO, "AUTO" , "MLP")
LightGBM_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, LightGBM_AUTO, pricesAUTO, "AUTO" , "LightGBM")
Willemain_IPM_GAMMA_AUTO <- IPM_GAMMA(AUTO, trainAUTO, testAUTO, Willemain_AUTO, pricesAUTO, "AUTO" , "Willemain")

set.seed(8377)
Croston_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, Croston_OIL, pricesOIL, "OIL" , "Croston")
SES_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, SES_OIL, pricesOIL, "OIL" , "SES")
SBA_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, SBA_OIL, pricesOIL, "OIL" , "SBA")
TSB_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, TSB_OIL, pricesOIL, "OIL" , "TSB")
MLP_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, MLP_OIL, pricesOIL, "OIL" , "MLP")
LightGBM_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, LightGBM_OIL, pricesOIL, "OIL" , "LightGBM")
Willemain_IPM_GAMMA_OIL <- IPM_GAMMA(OIL, trainOIL, testOIL, Willemain_OIL, pricesOIL, "OIL" , "Willemain")

####################################################
### ESC NEGBIN ###

ESC_NEGBIN <- function(p, r, beta, R) {
  ESC = -(R - r*beta) * (1 - pnbinom(R, r, p)) + (R + r) * beta * dnbinom(R, r, p)
  return(ESC)
}

IPM_NEGBIN <- function(data, train_data, test_data, predictions, prices, data_name, Method) {
  # Containing fill rate for each item for each TFR (col ~ item, row ~ TFR)
  # Each entry = total supply / total demand of an item for a TFR
  FillRates <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(FillRates) <- colnames(data)
  rownames(FillRates) <- targetFillRates
  
  # Containing total supply for each item for each TFR (col ~ item, row ~ TFR)
  TotalSupply <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(TotalSupply) <- colnames(data)
  rownames(TotalSupply) <- targetFillRates
  
  # Containing total demand for each item
  TotalDemand <- colSums(test_data)
  
  # Containing average holding cost for each item for each TFR (col ~ item, row ~ TFR)
  HoldingCosts <- as.data.frame(matrix(ncol = ncol(test_data), nrow = nTFR))
  colnames(HoldingCosts) <- colnames(data)
  rownames(HoldingCosts) <- targetFillRates
  
  t = nrow(train_data)
  h = nrow(test_data)
  
  for (i in 1:ncol(test_data)) {
    for (j in 1:nTFR) {
      r <- targetFillRates[j]
      inventory <- data.frame(TFR = rep(r, length = h), Sigma = NA, Mu = NA, pNB = NA, rNB = NA, betaNB = NA,
                              R = NA, Q = NA, ILpreD = NA, D = NA, S = NA, ILpostD = NA, Cost = NA)
      inventory$D <- rep(test_data[,i])
      for (k in 1:h) {
        inventory$Sigma[k] <- sd(data[1:(t+k-1),i])
      }
      inventory$Mu    <- predictions[,i]
      inventory$pNB    <- 1 - inventory$Mu / inventory$Sigma^2
      inventory$rNB    <- (1 - inventory$pNB) * inventory$Mu / inventory$pNB
      inventory$betaNB <- inventory$pNB / (1 - inventory$pNB)
      
      for (g in 1:h) {
        if (inventory$Mu[g] >= inventory$Sigma[g]^2 || inventory$Mu[g] <= 0 || inventory$betaNB[g] == Inf) {
          inventory$R[g] <- 0
          next
        }
        maxR <- max(data[1:(t+g-1),i])
        R <- 0
        lossTarget <- (1 - r) * inventory$Mu[g]
        tempQ <- round(inventory$Mu[g])
        shortagePerCycle <- ESC_NEGBIN(inventory$pNB[g], inventory$rNB[g], inventory$betaNB[g], R) #- ESC_NEGBIN(inventory$pNB[g], inventory$rNB[g], inventory$betaNB[g], R + tempQ)
        while (shortagePerCycle > lossTarget) {
          shortagePerCycle <- ESC_NEGBIN(inventory$pNB[g], inventory$rNB[g], inventory$betaNB[g], R) #- ESC_NEGBIN(inventory$pNB[g], inventory$rNB[g], inventory$betaNB[g], R + tempQ)
          R <- R + 1
          if (R == maxR) {
            break
          }
        }
        inventory$R[g] <- R
      }
      
      inventory$Q[1] <- 0                              # Initial order quantity
      inventory$ILpreD[1] <- inventory$R[1]
      inventory$S[1] <- min(inventory$ILpreD[1], inventory$D[1])
      inventory$ILpostD[1] <- inventory$ILpreD[1] - inventory$D[1]
      
      for (g in 2:h) {
        inventory$Q[g] <- max(0, inventory$R[g] - inventory$ILpostD[g-1])
        inventory$ILpreD[g] <- max(inventory$ILpostD[g-1], inventory$R[g])
        inventory$S[g] <- min(inventory$ILpreD[g], inventory$D[g])
        inventory$ILpostD[g] <- inventory$ILpreD[g] - inventory$D[g]
      }
      
      Cost <- 0.25 * inventory$ILpreD * prices[i]
      Cost[Cost < 0] <- 0
      inventory$Cost <- Cost
      
      if (TotalDemand[i] == 0) {
        FillRates[j,i] <- 0
      } else {
        FillRates[j,i] <- sum(inventory$S) / sum(inventory$D)
      }
      TotalSupply[j,i] <- sum(inventory$S)
      HoldingCosts[j,i] <- mean(inventory$Cost)
    } # end loop r
    print(paste("Done", i))
  } # end loop i
  
  totalAFR <- rowSums(TotalSupply) / sum(TotalDemand)
  avgAFR <- rowMeans(FillRates)
  
  # Holding Cost (for each target fill rate) is the sum of holding costs of all items
  holdingCost <- rowSums(HoldingCosts)
  
  # Service Level data frame
  ServiceLevel <- data.frame(avgAFR, totalAFR, holdingCost, targetFillRates, Method)
  colnames(ServiceLevel) <- c("AchievedFillRates_Avg", "AchievedFillRates_Total", "HoldingCosts", "TargetFillRates", "Method")
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/IPM_L0/ESC2/NEGBIN_IPM_", Method, "_", data_name, ".Rda", sep = "")
  save(ServiceLevel, FillRates, TotalDemand, TotalSupply, file = file_name)
  
  return(ServiceLevel)
}

set.seed(8377)
Croston_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, Croston_SIM1, pricesSIM1, "SIM1" , "Croston")
SES_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, SES_SIM1, pricesSIM1, "SIM1" , "SES")
SBA_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, SBA_SIM1, pricesSIM1, "SIM1" , "SBA")
TSB_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, TSB_SIM1, pricesSIM1, "SIM1" , "TSB")
MLP_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, MLP_SIM1, pricesSIM1, "SIM1" , "MLP")
LightGBM_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, LightGBM_SIM1, pricesSIM1, "SIM1" , "LightGBM")
Willemain_IPM_NEGBIN_SIM1 <- IPM_NEGBIN(SIM1, trainSIM1, testSIM1, Willemain_SIM1, pricesSIM1, "SIM1" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, Croston_SIM2, pricesSIM2, "SIM2" , "Croston")
SES_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, SES_SIM2, pricesSIM2, "SIM2" , "SES")
SBA_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, SBA_SIM2, pricesSIM2, "SIM2" , "SBA")
TSB_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, TSB_SIM2, pricesSIM2, "SIM2" , "TSB")
MLP_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, MLP_SIM2, pricesSIM2, "SIM2" , "MLP")
LightGBM_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, LightGBM_SIM2, pricesSIM2, "SIM2" , "LightGBM")
Willemain_IPM_NEGBIN_SIM2 <- IPM_NEGBIN(SIM2, trainSIM2, testSIM2, Willemain_SIM2, pricesSIM2, "SIM2" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, Croston_SIM3, pricesSIM3, "SIM3" , "Croston")
SES_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, SES_SIM3, pricesSIM3, "SIM3" , "SES")
SBA_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, SBA_SIM3, pricesSIM3, "SIM3" , "SBA")
TSB_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, TSB_SIM3, pricesSIM3, "SIM3" , "TSB")
MLP_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, MLP_SIM3, pricesSIM3, "SIM3" , "MLP")
LightGBM_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, LightGBM_SIM3, pricesSIM3, "SIM3" , "LightGBM")
Willemain_IPM_NEGBIN_SIM3 <- IPM_NEGBIN(SIM3, trainSIM3, testSIM3, Willemain_SIM3, pricesSIM3, "SIM3" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, Croston_SIM4, pricesSIM4, "SIM4" , "Croston")
SES_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, SES_SIM4, pricesSIM4, "SIM4" , "SES")
SBA_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, SBA_SIM4, pricesSIM4, "SIM4" , "SBA")
TSB_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, TSB_SIM4, pricesSIM4, "SIM4" , "TSB")
MLP_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, MLP_SIM4, pricesSIM4, "SIM4" , "MLP")
LightGBM_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, LightGBM_SIM4, pricesSIM4, "SIM4" , "LightGBM")
Willemain_IPM_NEGBIN_SIM4 <- IPM_NEGBIN(SIM4, trainSIM4, testSIM4, Willemain_SIM4, pricesSIM4, "SIM4" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, Croston_MAN, pricesMAN, "MAN" , "Croston")
SES_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, SES_MAN, pricesMAN, "MAN" , "SES")
SBA_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, SBA_MAN, pricesMAN, "MAN" , "SBA")
TSB_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, TSB_MAN, pricesMAN, "MAN" , "TSB")
MLP_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, MLP_MAN, pricesMAN, "MAN" , "MLP")
LightGBM_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, LightGBM_MAN, pricesMAN, "MAN" , "LightGBM")
Willemain_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, Willemain_MAN, pricesMAN, "MAN" , "Willemain")
# Willemain_IPM_NEGBIN_MAN <- IPM_NEGBIN(MAN, trainMAN, testMAN, Willemain_MAN_p1, pricesMAN, "MAN" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, Croston_BRAF, pricesBRAF, "BRAF" , "Croston")
SES_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, SES_BRAF, pricesBRAF, "BRAF" , "SES")
SBA_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, SBA_BRAF, pricesBRAF, "BRAF" , "SBA")
TSB_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, TSB_BRAF, pricesBRAF, "BRAF" , "TSB")
MLP_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, MLP_BRAF, pricesBRAF, "BRAF" , "MLP")
LightGBM_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, LightGBM_BRAF, pricesBRAF, "BRAF" , "LightGBM")
Willemain_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, Willemain_BRAF, pricesBRAF, "BRAF" , "Willemain")
# Willemain_IPM_NEGBIN_BRAF <- IPM_NEGBIN(BRAF, trainBRAF, testBRAF, Willemain_BRAF, pricesBRAF, "BRAF" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, Croston_AUTO, pricesAUTO, "AUTO" , "Croston")
SES_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, SES_AUTO, pricesAUTO, "AUTO" , "SES")
SBA_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, SBA_AUTO, pricesAUTO, "AUTO" , "SBA")
TSB_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, TSB_AUTO, pricesAUTO, "AUTO" , "TSB")
MLP_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, MLP_AUTO, pricesAUTO, "AUTO" , "MLP")
LightGBM_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, LightGBM_AUTO, pricesAUTO, "AUTO" , "LightGBM")
Willemain_IPM_NEGBIN_AUTO <- IPM_NEGBIN(AUTO, trainAUTO, testAUTO, Willemain_AUTO, pricesAUTO, "AUTO" , "Willemain")

set.seed(8377)
Croston_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, Croston_OIL, pricesOIL, "OIL" , "Croston")
SES_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, SES_OIL, pricesOIL, "OIL" , "SES")
SBA_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, SBA_OIL, pricesOIL, "OIL" , "SBA")
TSB_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, TSB_OIL, pricesOIL, "OIL" , "TSB")
MLP_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, MLP_OIL, pricesOIL, "OIL" , "MLP")
LightGBM_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, LightGBM_OIL, pricesOIL, "OIL" , "LightGBM")
Willemain_IPM_NEGBIN_OIL <- IPM_NEGBIN(OIL, trainOIL, testOIL, Willemain_OIL, pricesOIL, "OIL" , "Willemain")
