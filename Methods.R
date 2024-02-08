# R script for forecasting spare part demands using seven different methods
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
# The R code from Daniel de Haan is utilized (and improved for efficiency) in this script
#
# Reference:
# de Haan, D. (2021). GitHub repository for benchmarking spare parts demand forecasting for intermittent demand (Version 1.0.0) [Computer software]. https://github.com/danieldehaan96/spdf
#
# Author:
# Khue Nguyen (Nguyen Hoang Thi Khue), <nguyen@ese.eur.nl>


# Install packages and library
install.packages("tsintermittent")
install.packages("xlsx")
install.packages("lightgbm")
install.packages("boot")
install.packages("expm")
install.packages("igraph")
install.packages("matlab")
install.packages("markovchain")
install.packages("Rcpp")
install.packages("TraMineR")
install.packages("dplyr")
install.packages("RSNNS")
install.packages("tidyr")
install.packages("collapse")
install.packages("runner")
install.packages("ggplot2")
install.packages("data.table")
install.packages("greybox")
install.packages("beepr")
install.packages("knitr")
install.packages("greybox")
install.packages("sjmisc")

library(xlsx)
library(tsintermittent)
library(lightgbm)
library(readxl)
library(boot)
library(expm)
library(igraph)
library(matlab)
library(markovchain)
library(Rcpp)
library(TraMineR)
library(dplyr)
library(RSNNS)
library(tidyr)
library(collapse)
library(runner)
library(ggplot2)
library(data.table)
library(greybox)
library(beepr)
library(knitr)
library(greybox)
library(sjmisc)



############################# PERFORMANCE MEASURES #############################

# Forecasting Accuracy Measures (FAM)
FAM <- function(train_data, test_data, predictions) {
  MSE_score <- NULL
  MASE_score <- NULL
  RMSSE_score <- NULL
  for (i in 1:ncol(test_data)){
    MSE_score <- cbind(MSE_score,  MSE(t(test_data[i]), t(predictions[,i])))
    MASE_score <- cbind(MASE_score,  MASE(t(test_data[i]), t(predictions[,i]), mean(abs(t(train_data[i])))))
    RMSSE_score <- cbind(RMSSE_score,  RMSSE(t(test_data[i]), t(predictions[,i]), mean(abs(t(train_data[i])))))
  }
  MSE_score <- mean(MSE_score)
  MASE_score <- mean(MASE_score)
  RMSSE_score <- mean(RMSSE_score)
  return(c(MSE_score, MASE_score, RMSSE_score))
}


# Inventory performance measures (IPM) -> See the Rscrip InventoryPerformance.R


############################# CROSTON AND VARIANTS #############################

# Croston
Croston_Perf <- function(train_data, test_data, data_name) {
  h <- nrow(test_data)                               # Number of periods to be predicted (for each item)
  numItems <- ncol(train_data)                       # Number of items
  predictions <- matrix(nrow = h, ncol = numItems)   # Matrix storing the predictions
  
  # For each item
  for (j in 1:numItems) {
    x <- rbind(train_data[j], test_data[j])          # All demands (train + test) of item j
    t <- nrow(train_data)                            # Number of periods used for predicting (updated every iteration below)
    
    for (i in 1:h) {
      predictions[i,j] <- crost(x[1:t,], h = 1, w = NULL, nop = 2, type = "croston", cost = "mar", init = "naive", init.op = FALSE, na.rm = TRUE)$frc.out
      t = t + 1
    }
    
    print(paste("Done item", j))
  }
  
  # Store predictions as data frame
  predictions <- as.data.frame(predictions)
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/Results_Predictions/Croston_", data_name, ".Rda", sep = "")
  save(predictions, file = file_name)
  return(predictions)
}

# SES
SES_Perf <- function(train_data, test_data, data_name) {
  h <- nrow(test_data)                               # Number of periods to be predicted (for each item)
  numItems <- ncol(train_data)                       # Number of items
  predictions <- matrix(nrow = h, ncol = numItems)   # Matrix storing the predictions
  
  # For each item
  for (j in 1:numItems) {
    x <- rbind(train_data[j], test_data[j])          # All demands (train + test) of item j
    t <- nrow(train_data)                            # Number of periods used for predicting (updated every iteration below)
    
    for (i in 1:h) {
      predictions[i,j] <- sexsm(x[1:t,], h=1, w=NULL, cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out
      t = t + 1
    }
    
    print(paste("Done item", j))
  }
  
  # Store predictions as data frame
  predictions <- as.data.frame(predictions)
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/Results_Predictions/SES_", data_name, ".Rda", sep = "")
  save(predictions, file = file_name)
  return(predictions)
}

# SBA
SBA_Perf <- function(train_data, test_data, data_name) {
  h <- nrow(test_data)                               # Number of periods to be predicted (for each item)
  numItems <- ncol(train_data)                       # Number of items
  predictions <- matrix(nrow = h, ncol = numItems)   # Matrix storing the predictions
  
  # For each item
  for (j in 1:numItems) {
    x <- rbind(train_data[j], test_data[j])          # All demands (train + test) of item j
    t <- nrow(train_data)                            # Number of periods used for predicting (updated every iteration below)
    
    for (i in 1:h) {
      predictions[i,j] <- crost(x[1:t,], h=1, w=NULL, nop=2, type="sba", cost="mar", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out
      t = t + 1
    }
    
    print(paste("Done item", j))
  }
  
  # Store predictions as data frame
  predictions <- as.data.frame(predictions)
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/Results_Predictions/SBA_", data_name, ".Rda", sep = "")
  save(predictions, file = file_name)
  return(predictions)
}

# TSB
TSB_Perf <- function(train_data, test_data, data_name) {
  h <- nrow(test_data)                               # Number of periods to be predicted (for each item)
  numItems <- ncol(train_data)                       # Number of items
  predictions <- matrix(nrow = h, ncol = numItems)   # Matrix storing the predictions
  
  # For each item
  for (j in 1:numItems) {
    x <- rbind(train_data[j], test_data[j])          # All demands (train + test) of item j
    t <- nrow(train_data)                            # Number of periods used for predicting (updated every iteration below)
    
    for (i in 1:h) {
      predictions[i,j] <- tsb(x[1:t,], h=1, w=NULL, cost="msr", init="naive", init.opt=FALSE, na.rm=TRUE)$frc.out
      t = t + 1
    }
    
    print(paste("Done item", j))
  }
  
  # Store predictions as data frame
  predictions <- as.data.frame(predictions)
  
  # Saving
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/Results_Predictions/TSB_", data_name, ".Rda", sep = "")
  save(predictions, file = file_name)
  return(predictions)
}



############################# WILLEMAIN #############################




############################# ML METHODS #############################

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))} #Supporting functions for MLP_Perf

MLP_Perf <- function(data, data_name) {
  # 1, Preparing training data (same)
  # 1.1, Normalize data
  mldata <- data
  for (i in 1:ncol(data)){
    mldata[,i]  <- normalize(data[,i]) #originally: mldataOIL[[i]] <- normalize(OIL[[i]]), which is the same (might be different: [i])
  }
  
  # 1.2, Splitting the data again into test and train data.
  
  sample = round(nrow(data)*.70) 
  train_data <- mldata[1:(sample), ]
  test_data <- mldata[-(1:(sample)), ]
  
  # mldata variable, length 6 with 5 input and 1 output.
  n <- nrow(train_data)
  mldata <- matrix(ncol = 6, nrow = ncol(train_data) * (n - 5))
  
  # For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
  for (i in 1:ncol(train_data)){
    for (t in 1:6) {
      startRow <- ((i - 1) * (n - 5) + 1)
      endRow <- i * (n - 5)
      mldata[startRow:endRow, t] <- t(train_data[t:(t + n - 6),i])
    }
  }
  
  mldata <- as.data.frame(mldata)
  
  
  # 2, Train the model (different for each method)
  model <- mlp(mldata[,1:5], mldata[,6], size=6, learnFuncParams=c(0.1), maxit=200)
  
  print("Done training model")
  
  
  # 3, Make predictions (same)
  
  # Creating the prediction matrix.
  h <- nrow(test_data)                               # Number of periods to be predicted (for each item)
  numItems <- ncol(train_data)                       # Number of items
  predictions <- matrix(nrow = h, ncol = numItems)   # Matrix storing the predictions
  
  # Predicting the forecast horizon with as input the last five values of a series and saving it as the predictions.
  for (j in 1:numItems) {
    input <- tail(train_data[,j], n=5)
    for (i in 1:nrow(test_data)) {
      predictions[i,j] <- predict(model, t(input))
      input[1:4] <- input[2:5]
      input[5] <- test_data[i,j]
    }
  }
  
  # Denormalizing the data for evaluation of forecasts.
  for (i in 1:ncol(predictions)) {
    predictions[,i] <- predictions[,i] * (max(data[,i]) - min(data[,i])) + min(data[,i])
  }
  
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/Results_Predictions/MLP_", data_name, ".Rda", sep = "")
  save(predictions, file = file_name)
  return(predictions)
}


LightGBM_Perf <- function(data, data_name) {
  # 1, Preparing training data (same)
  # 1.1, Normalize data
  mldata <- data
  for (i in 1:ncol(data)){
    mldata[,i]  <- normalize(data[,i]) #originally: mldataOIL[[i]] <- normalize(OIL[[i]]), which is the same (might be different: [i])
  }
  
  # 1.2, Splitting the data again into test and train data.
  
  sample = round(nrow(data)*.70) 
  train_data <- mldata[1:(sample), ]
  test_data <- mldata[-(1:(sample)), ]
  
  # mldata variable, length 6 with 5 input and 1 output.
  n <- nrow(train_data)
  mldata <- matrix(ncol = 6, nrow = ncol(train_data) * (n - 5))
  
  # For loop that creates a 6 column data set with the first 5 as inputs to the neural net and the 6th column as the target variable.
  for (i in 1:ncol(train_data)){
    for (t in 1:6) {
      startRow <- ((i - 1) * (n - 5) + 1)
      endRow <- i * (n - 5)
      mldata[startRow:endRow, t] <- t(train_data[t:(t + n - 6),i])
    }
  }
  
  mldata <- as.data.frame(mldata)
  
  
  # 2, Train the model (different for each method)
  # Setting the hyper-parameters for the LightGBM model, based on the kaggle method.
  p <- list(objective = "regression", metric ="rmse", boosting = "gbdt",
            force_row_wise = TRUE, learning_rate = 0.075, num_leaves = 128,
            min_data = 100, sub_feature = 0.8, sub_row = 0.75,
            bagging_freq = 1, lambda_l2 = 0.1, nthread = 4)
  
  # Training the LightGBM algorithm.
  model <- lightgbm(data = as.matrix(mldata[,(1:5)]), params = p, 
                    label = mldata$V6, nrounds = 12000,
                    early_stopping_rounds = 400, eval_freq = 400)
  
  print("Done training model")
  
  
  # 3, Make predictions (same)
  
  # Creating the prediction matrix.
  h <- nrow(test_data)                               # Number of periods to be predicted (for each item)
  numItems <- ncol(train_data)                       # Number of items
  predictions <- matrix(nrow = h, ncol = numItems)   # Matrix storing the predictions
  
  # Predicting the forecast horizon with as input the last five values of a series and saving it as the predictions.
  for (j in 1:numItems) {
    input <- tail(train_data[,j], n=5)
    for (i in 1:nrow(test_data)) {
      predictions[i,j] <- predict(model, t(input))
      input[1:4] <- input[2:5]
      input[5] <- test_data[i,j]
    }
  }
  
  # Denormalizing the data for evaluation of forecasts.
  for (i in 1:ncol(predictions)) {
    predictions[,i] <- predictions[,i] * (max(data[,i]) - min(data[,i])) + min(data[,i])
  }
  
  file_name <- paste("C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/RA - Spare parts demand forecasting/K/Updated/Results_Predictions/LightGBM_", data_name, ".Rda", sep = "")
  save(predictions, file = file_name)
  return(predictions)
}


# # Only used in case of lower version of R where the "base" package does not
# # support function isa().
# isa <- function(data, string) {
#   if (is.data.frame(data) && string == "data.frame") {
#     return(TRUE)
#   } else {
#     return(FALSE)
#   }
# }



############################# RUNNING METHODS ON DATA SETS #############################


# Set seed for reproducibility.
set.seed(8377)


########## CROSTON ##########
ptm <- proc.time()

##### SIM1 #####
Croston_SIM1 <- Croston_Perf(trainSIM1, testSIM1, "SIM1")
Croston_FAM_SIM1 <- FAM(trainSIM1, testSIM1, Croston_SIM1)
Croston_IPM_SIM1 <- IPM(trainSIM1, testSIM1, Croston_SIM1, leadtimesSIM1, pricesSIM1, "Croston")

##### SIM2 #####
Croston_SIM2 <- Croston_Perf(trainSIM2, testSIM2, "SIM2")
Croston_FAM_SIM2 <- FAM(trainSIM2, testSIM2, Croston_SIM2)
Croston_IPM_SIM2 <- IPM(trainSIM2, testSIM2, Croston_SIM2, leadtimesSIM2, pricesSIM2, "Croston")

##### SIM3 #####
Croston_SIM3 <- Croston_Perf(trainSIM3, testSIM3, "SIM3")
Croston_FAM_SIM3 <- FAM(trainSIM3, testSIM3, Croston_SIM3)
Croston_IPM_SIM3 <- IPM(trainSIM3, testSIM3, Croston_SIM3, leadtimesSIM3, pricesSIM3, "Croston")

##### SIM4 #####
Croston_SIM4 <- Croston_Perf(trainSIM4, testSIM4, "SIM4")
Croston_FAM_SIM4 <- FAM(trainSIM4, testSIM4, Croston_SIM4)
Croston_IPM_SIM4 <- IPM(trainSIM4, testSIM4, Croston_SIM4, leadtimesSIM4, pricesSIM4, "Croston")

##### MAN #####
Croston_MAN <- Croston_Perf(trainMAN, testMAN, "MAN")
Croston_FAM_MAN <- FAM(trainMAN, testMAN, Croston_MAN)
Croston_IPM_MAN <- IPM(trainMAN, testMAN, Croston_MAN, leadtimesMAN, pricesMAN, "Croston")

##### BRAF #####
Croston_BRAF <- Croston_Perf(trainBRAF, testBRAF, "BRAF")
Croston_FAM_BRAF <- FAM(trainBRAF, testBRAF, Croston_BRAF)
Croston_IPM_BRAF <- IPM(trainBRAF, testBRAF, Croston_BRAF, leadtimesBRAF, pricesBRAF, "Croston")

##### AUTO #####
Croston_AUTO <- Croston_Perf(trainAUTO, testAUTO, "AUTO")
Croston_FAM_AUTO <- FAM(trainAUTO, testAUTO, Croston_AUTO)
Croston_IPM_AUTO <- IPM(trainAUTO, testAUTO, Croston_AUTO, leadtimesAUTO, pricesAUTO, "Croston")

##### Print running time #####
print("time Croston total")
proc.time() - ptm



########## SES ##########
ptm <- proc.time()

##### SIM1 #####
SES_SIM1 <- SES_Perf(trainSIM1, testSIM1, "SIM1")
SES_FAM_SIM1 <- FAM(trainSIM1, testSIM1, SES_SIM1)
SES_IPM_SIM1 <- IPM(trainSIM1, testSIM1, SES_SIM1, leadtimesSIM1, pricesSIM1, "SES")

##### SIM2 #####
SES_SIM2 <- SES_Perf(trainSIM2, testSIM2, "SIM2")
SES_FAM_SIM2 <- FAM(trainSIM2, testSIM2, SES_SIM2)
SES_IPM_SIM2 <- IPM(trainSIM2, testSIM2, SES_SIM2, leadtimesSIM2, pricesSIM2, "SES")

##### SIM3 #####
SES_SIM3 <- SES_Perf(trainSIM3, testSIM3, "SIM3")
SES_FAM_SIM3 <- FAM(trainSIM3, testSIM3, SES_SIM3)
SES_IPM_SIM3 <- IPM(trainSIM3, testSIM3, SES_SIM3, leadtimesSIM3, pricesSIM3, "SES")

##### SIM4 #####
SES_SIM4 <- SES_Perf(trainSIM4, testSIM4, "SIM4")
SES_FAM_SIM4 <- FAM(trainSIM4, testSIM4, SES_SIM4)
SES_IPM_SIM4 <- IPM(trainSIM4, testSIM4, SES_SIM4, leadtimesSIM4, pricesSIM4, "SES")

##### MAN #####
SES_MAN <- SES_Perf(trainMAN, testMAN, "MAN")
SES_FAM_MAN <- FAM(trainMAN, testMAN, SES_MAN)
SES_IPM_MAN <- IPM(trainMAN, testMAN, SES_MAN, leadtimesMAN, pricesMAN, "SES")

##### BRAF #####
SES_BRAF <- SES_Perf(trainBRAF, testBRAF, "BRAF")
SES_FAM_BRAF <- FAM(trainBRAF, testBRAF, SES_BRAF)
SES_IPM_BRAF <- IPM(trainBRAF, testBRAF, SES_BRAF, leadtimesBRAF, pricesBRAF, "SES")

##### AUTO #####
SES_AUTO <- SES_Perf(trainAUTO, testAUTO, "AUTO")
SES_FAM_AUTO <- FAM(trainAUTO, testAUTO, SES_AUTO)
SES_IPM_AUTO <- IPM(trainAUTO, testAUTO, SES_AUTO, leadtimesAUTO, pricesAUTO, "SES")

##### Print running time #####
print("time SES total")
proc.time() - ptm



########## SBA ##########
ptm <- proc.time()

##### SIM1 #####
SBA_SIM1 <- SBA_Perf(trainSIM1, testSIM1, "SIM1")
SBA_FAM_SIM1 <- FAM(trainSIM1, testSIM1, SBA_SIM1)
SBA_IPM_SIM1 <- IPM(trainSIM1, testSIM1, SBA_SIM1, leadtimesSIM1, pricesSIM1, "SBA")

##### SIM2 #####
SBA_SIM2 <- SBA_Perf(trainSIM2, testSIM2, "SIM2")
SBA_FAM_SIM2 <- FAM(trainSIM2, testSIM2, SBA_SIM2)
SBA_IPM_SIM2 <- IPM(trainSIM2, testSIM2, SBA_SIM2, leadtimesSIM2, pricesSIM2, "SBA")

##### SIM3 #####
SBA_SIM3 <- SBA_Perf(trainSIM3, testSIM3, "SIM3")
SBA_FAM_SIM3 <- FAM(trainSIM3, testSIM3, SBA_SIM3)
SBA_IPM_SIM3 <- IPM(trainSIM3, testSIM3, SBA_SIM3, leadtimesSIM3, pricesSIM3, "SBA")

##### SIM4 #####
SBA_SIM4 <- SBA_Perf(trainSIM4, testSIM4, "SIM4")
SBA_FAM_SIM4 <- FAM(trainSIM4, testSIM4, SBA_SIM4)
SBA_IPM_SIM4 <- IPM(trainSIM4, testSIM4, SBA_SIM4, leadtimesSIM4, pricesSIM4, "SBA")

##### MAN #####
SBA_MAN <- SBA_Perf(trainMAN, testMAN, "MAN")
SBA_FAM_MAN <- FAM(trainMAN, testMAN, SBA_MAN)
SBA_IPM_MAN <- IPM(trainMAN, testMAN, SBA_MAN, leadtimesMAN, pricesMAN, "SBA")

##### BRAF #####
SBA_BRAF <- SBA_Perf(trainBRAF, testBRAF, "BRAF")
SBA_FAM_BRAF <- FAM(trainBRAF, testBRAF, SBA_BRAF)
SBA_IPM_BRAF <- IPM(trainBRAF, testBRAF, SBA_BRAF, leadtimesBRAF, pricesBRAF, "SBA")

##### AUTO #####
SBA_AUTO <- SBA_Perf(trainAUTO, testAUTO, "AUTO")
SBA_FAM_AUTO <- FAM(trainAUTO, testAUTO, SBA_AUTO)
SBA_IPM_AUTO <- IPM(trainAUTO, testAUTO, SBA_AUTO, leadtimesAUTO, pricesAUTO, "SBA")

##### Print running time #####
print("time SBA total")
proc.time() - ptm



########## TSB ##########
ptm <- proc.time()

##### SIM1 #####
TSB_SIM1 <- TSB_Perf(trainSIM1, testSIM1, "SIM1")
TSB_FAM_SIM1 <- FAM(trainSIM1, testSIM1, TSB_SIM1)
TSB_IPM_SIM1 <- IPM(trainSIM1, testSIM1, TSB_SIM1, leadtimesSIM1, pricesSIM1, "TSB")

##### SIM2 #####
TSB_SIM2 <- TSB_Perf(trainSIM2, testSIM2, "SIM2")
TSB_FAM_SIM2 <- FAM(trainSIM2, testSIM2, TSB_SIM2)
TSB_IPM_SIM2 <- IPM(trainSIM2, testSIM2, TSB_SIM2, leadtimesSIM2, pricesSIM2, "TSB")

##### SIM3 #####
TSB_SIM3 <- TSB_Perf(trainSIM3, testSIM3, "SIM3")
TSB_FAM_SIM3 <- FAM(trainSIM3, testSIM3, TSB_SIM3)
TSB_IPM_SIM3 <- IPM(trainSIM3, testSIM3, TSB_SIM3, leadtimesSIM3, pricesSIM3, "TSB")

##### SIM4 #####
TSB_SIM4 <- TSB_Perf(trainSIM4, testSIM4, "SIM4")
TSB_FAM_SIM4 <- FAM(trainSIM4, testSIM4, TSB_SIM4)
TSB_IPM_SIM4 <- IPM(trainSIM4, testSIM4, TSB_SIM4, leadtimesSIM4, pricesSIM4, "TSB")

##### MAN #####
TSB_MAN <- TSB_Perf(trainMAN, testMAN, "MAN")
TSB_FAM_MAN <- FAM(trainMAN, testMAN, TSB_MAN)
TSB_IPM_MAN <- IPM(trainMAN, testMAN, TSB_MAN, leadtimesMAN, pricesMAN, "TSB")

##### BRAF #####
TSB_BRAF <- TSB_Perf(trainBRAF, testBRAF, "BRAF")
TSB_FAM_BRAF <- FAM(trainBRAF, testBRAF, TSB_BRAF)
TSB_IPM_BRAF <- IPM(trainBRAF, testBRAF, TSB_BRAF, leadtimesBRAF, pricesBRAF, "TSB")

##### AUTO #####
TSB_AUTO <- TSB_Perf(trainAUTO, testAUTO, "AUTO")
TSB_FAM_AUTO <- FAM(trainAUTO, testAUTO, TSB_AUTO)
TSB_IPM_AUTO <- IPM(trainAUTO, testAUTO, TSB_AUTO, leadtimesAUTO, pricesAUTO, "TSB")

##### Print running time #####
print("time TSB total")
proc.time() - ptm



########## WILLEMAIN ##########




########## MLP ##########
ptm <- proc.time()

##### SIM1 #####
MLP_SIM1 <- MLP_Perf(SIM1, "SIM1")
MLP_FAM_SIM1 <- FAM(trainSIM1, testSIM1, MLP_SIM1)
MLP_IPM_SIM1 <- IPM(trainSIM1, testSIM1, MLP_SIM1, leadtimesSIM1, pricesSIM1, "MLP")

##### SIM2 #####
MLP_SIM2 <- MLP_Perf(SIM2, "SIM2")
MLP_FAM_SIM2 <- FAM(trainSIM2, testSIM2, MLP_SIM2)
MLP_IPM_SIM2 <- IPM(trainSIM2, testSIM2, MLP_SIM2, leadtimesSIM2, pricesSIM2, "MLP")

##### SIM3 #####
MLP_SIM3 <- MLP_Perf(SIM3, "SIM3")
MLP_FAM_SIM3 <- FAM(trainSIM3, testSIM3, MLP_SIM3)
MLP_IPM_SIM3 <- IPM(trainSIM3, testSIM3, MLP_SIM3, leadtimesSIM3, pricesSIM3, "MLP")

##### SIM4 #####
MLP_SIM4 <- MLP_Perf(SIM4, "SIM4")
MLP_FAM_SIM4 <- FAM(trainSIM4, testSIM4, MLP_SIM4)
MLP_IPM_SIM4 <- IPM(trainSIM4, testSIM4, MLP_SIM4, leadtimesSIM4, pricesSIM4, "MLP")

##### MAN #####
MLP_MAN <- MLP_Perf(MAN, "MAN")
MLP_FAM_MAN <- FAM(trainMAN, testMAN, MLP_MAN)
MLP_IPM_MAN <- IPM(trainMAN, testMAN, MLP_MAN, leadtimesMAN, pricesMAN, "MLP")

##### BRAF #####
MLP_BRAF <- MLP_Perf(BRAF, "BRAF")
MLP_FAM_BRAF <- FAM(trainBRAF, testBRAF, MLP_BRAF)
MLP_IPM_BRAF <- IPM(trainBRAF, testBRAF, MLP_BRAF, leadtimesBRAF, pricesBRAF, "MLP")

##### AUTO #####
MLP_AUTO <- MLP_Perf(AUTO, "AUTO")
MLP_FAM_AUTO <- FAM(trainAUTO, testAUTO, MLP_AUTO)
MLP_IPM_AUTO <- IPM(trainAUTO, testAUTO, MLP_AUTO, leadtimesAUTO, pricesAUTO, "MLP")

##### Print running time #####
print("time MLP total")
proc.time() - ptm




########## LightGBM ##########
##### SIM1 #####
LightGBM_SIM1 <- LightGBM_Perf(SIM1, "SIM1")
LightGBM_FAM_SIM1 <- FAM(trainSIM1, testSIM1, LightGBM_SIM1)
LightGBM_IPM_SIM1 <- IPM(trainSIM1, testSIM1, LightGBM_SIM1, leadtimesSIM1, pricesSIM1, "LightGBM")

##### SIM2 #####
LightGBM_SIM2 <- LightGBM_Perf(SIM2, "SIM2")
LightGBM_FAM_SIM2 <- FAM(trainSIM2, testSIM2, LightGBM_SIM2)
LightGBM_IPM_SIM2 <- IPM(trainSIM2, testSIM2, LightGBM_SIM2, leadtimesSIM2, pricesSIM2, "LightGBM")

##### SIM3 #####
LightGBM_SIM3 <- LightGBM_Perf(SIM3, "SIM3")
LightGBM_FAM_SIM3 <- FAM(trainSIM3, testSIM3, LightGBM_SIM3)
LightGBM_IPM_SIM3 <- IPM(trainSIM3, testSIM3, LightGBM_SIM3, leadtimesSIM3, pricesSIM3, "LightGBM")

##### SIM4 #####
LightGBM_SIM4 <- LightGBM_Perf(SIM4, "SIM4")
LightGBM_FAM_SIM4 <- FAM(trainSIM4, testSIM4, LightGBM_SIM4)
LightGBM_IPM_SIM4 <- IPM(trainSIM4, testSIM4, LightGBM_SIM4, leadtimesSIM4, pricesSIM4, "LightGBM")

##### MAN #####
LightGBM_MAN <- LightGBM_Perf(MAN, "MAN")
LightGBM_FAM_MAN <- FAM(trainMAN, testMAN, LightGBM_MAN)
LightGBM_IPM_MAN <- IPM(trainMAN, testMAN, LightGBM_MAN, leadtimesMAN, pricesMAN, "LightGBM")

##### BRAF #####
LightGBM_BRAF <- LightGBM_Perf(BRAF, "BRAF")
LightGBM_FAM_BRAF <- FAM(trainBRAF, testBRAF, LightGBM_BRAF)
LightGBM_IPM_BRAF <- IPM(trainBRAF, testBRAF, LightGBM_BRAF, leadtimesBRAF, pricesBRAF, "LightGBM")

##### AUTO #####
LightGBM_AUTO <- LightGBM_Perf(AUTO, "AUTO")
LightGBM_FAM_AUTO <- FAM(trainAUTO, testAUTO, LightGBM_AUTO)
LightGBM_IPM_AUTO <- IPM(trainAUTO, testAUTO, LightGBM_AUTO, leadtimesAUTO, pricesAUTO, "LightGBM")

##### Print running time #####
print("time LightGBM total")
proc.time() - ptm
