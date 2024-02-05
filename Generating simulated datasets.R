# Classify
classifyDemand <- function(CV2, p) {
  output <- as.data.frame(cbind(CV2, p))
  output$Type <- case_when(p < 1.32 & CV2 >= 0.49 ~ 'Erratic',
                           p >= 1.32 & CV2 >= 0.49 ~ 'Lumpy',
                           p < 1.32 & CV2 < 0.49 ~ 'Smooth',
                           p >= 1.32 & CV2 < 0.49 ~ 'Intermittent')
  return(output)
}


# # Generate new simulated datasets
set.seed(1234)
SIM1 <- simID(n = 15000, obs = 60, idi = 1.05, cv2 = 0.75, level = 10)

SIM2_1 <- simID(n = 15000, obs = 60, idi = 2, cv2 = 0.75, level = 10)
SIM2_2 <- simID(n = 15000, obs = 60, idi = 19.795, cv2 = 0.75, level = 10)
SIM2 <- simID(n = 20000, obs = 60, idi = 19.795, cv2 = 0.75, level = 10)

SIM3 <- simID(n = 15000, obs = 60, idi = 1.05, cv2 = 0.25, level = 10)
SIM4 <- simID(n = 15000, obs = 60, idi = 19.795, cv2 = 0.25, level = 10)



### SIM1

sampleSIM1 <- round(nrow(SIM1)*.70)
trainSIM1 <- SIM1[1:(sampleSIM1),]
testSIM1 <- SIM1[-(1:(sampleSIM1)),]

cond <- colSums(trainSIM1 != 0, na.rm = TRUE) < 2
mask <- !(cond)
SIM1_full <-SIM1
SIM1 <- subset(SIM1, select = mask)
SIM1 <- SIM1[,1:6500]

trainSIM1 <- SIM1[1:(sampleSIM1),]
testSIM1 <- SIM1[-(1:(sampleSIM1)),]

###### Classification
nItems <- ncol(trainSIM1)
nPeriods <- nrow(trainSIM1)

# Change 0 values to NA to take NZD statistics
is.na(trainSIM1) <- trainSIM1 == 0
classificationSIM1 <- data.frame(MeanNZD = rep(NA, length = nItems), StdevNZD = NA, p = NA, CV2 = NA, Classification = NA)
classificationSIM1$MeanNZD <- apply(trainSIM1, 2, mean, na.rm = TRUE)
classificationSIM1$StdevNZD <- apply(trainSIM1, 2, sd, na.rm = TRUE)
classificationSIM1$p <- nPeriods / colSums(!is.na(trainSIM1))
classificationSIM1$CV2 <- (classificationSIM1$StdevNZD / classificationSIM1$MeanNZD)^2
classificationSIM1$Classification <- classifyDemand(classificationSIM1$CV2, classificationSIM1$p)$Type
mean(classificationSIM1$p)
mean(classificationSIM1$CV2)
table(classificationSIM1$Classification)

# Change NA to 0
trainSIM1[is.na(trainSIM1)] <- 0

###### Determine prices
RPS <- 174.952 
avgMonthlySales <- mean(colMeans(SIM1))
avgProductPrice <- RPS * mean(colMeans(SIM1))
RMS <- colMeans(SIM1) / avgMonthlySales
pricesSIM1 <- avgProductPrice / RMS












### SIM2

sampleSIM2 <- round(nrow(SIM2)*.70)
trainSIM2 <- SIM2[1:(sampleSIM2),]
testSIM2 <- SIM2[-(1:(sampleSIM2)),]

cond <- colSums(trainSIM2 != 0, na.rm = TRUE) < 2
mask <- !(cond)
SIM2_full <- SIM2
SIM2 <- subset(SIM2, select = mask)

trainSIM2 <- SIM2[1:(sampleSIM2),]
testSIM2 <- SIM2[-(1:(sampleSIM2)),]

###### Classification
nItems <- ncol(trainSIM2)
nPeriods <- nrow(trainSIM2)

# Change 0 values to NA to take NZD statistics
is.na(trainSIM2) <- trainSIM2 == 0
classificationSIM2 <- data.frame(MeanNZD = rep(NA, length = nItems), StdevNZD = NA, p = NA, CV2 = NA, Classification = NA)
classificationSIM2$MeanNZD <- apply(trainSIM2, 2, mean, na.rm = TRUE)
classificationSIM2$StdevNZD <- apply(trainSIM2, 2, sd, na.rm = TRUE)
classificationSIM2$p <- nPeriods / colSums(!is.na(trainSIM2))
classificationSIM2$CV2 <- (classificationSIM2$StdevNZD / classificationSIM2$MeanNZD)^2
classificationSIM2$Classification <- classifyDemand(classificationSIM2$CV2, classificationSIM2$p)$Type
mean(classificationSIM2$p)
mean(classificationSIM2$CV2)
table(classificationSIM2$Classification)

# Form new SIM2 data set
SelectColumns <- sample(which(classificationSIM2$Classification == "Lumpy"), 5200, replace=FALSE)
SelectColumns <- c(SelectColumns, sample(c(1:ncol(SIM2))[!(c(1:ncol(SIM2)) %in% SelectColumns)], 6500-5200, replace=FALSE))
SIM2 <- SIM2[,SelectColumns]
SIM2 <- SIM2[,sample(ncol(SIM2))]

# Classify again
trainSIM2 <- SIM2[1:(sampleSIM2),]
testSIM2 <- SIM2[-(1:(sampleSIM2)),]

###### Classification
nItems <- ncol(trainSIM2)
nPeriods <- nrow(trainSIM2)

# Change 0 values to NA to take NZD statistics
is.na(trainSIM2) <- trainSIM2 == 0
classificationSIM2 <- data.frame(MeanNZD = rep(NA, length = nItems), StdevNZD = NA, p = NA, CV2 = NA, Classification = NA)
classificationSIM2$MeanNZD <- apply(trainSIM2, 2, mean, na.rm = TRUE)
classificationSIM2$StdevNZD <- apply(trainSIM2, 2, sd, na.rm = TRUE)
classificationSIM2$p <- nPeriods / colSums(!is.na(trainSIM2))
classificationSIM2$CV2 <- (classificationSIM2$StdevNZD / classificationSIM2$MeanNZD)^2
classificationSIM2$Classification <- classifyDemand(classificationSIM2$CV2, classificationSIM2$p)$Type
mean(classificationSIM2$p)
mean(classificationSIM2$CV2)
table(classificationSIM2$Classification)

###### Determine prices
RPS <- 174.952 
avgMonthlySales <- mean(colMeans(SIM2))
avgProductPrice <- RPS * mean(colMeans(SIM2))
RMS <- colMeans(SIM2) / avgMonthlySales
pricesSIM2 <- avgProductPrice / RMS














### SIM3

sampleSIM3 <- round(nrow(SIM3)*.70)
trainSIM3 <- SIM3[1:(sampleSIM3),]
testSIM3 <- SIM3[-(1:(sampleSIM3)),]

cond <- colSums(trainSIM3 != 0, na.rm = TRUE) < 2
mask <- !(cond)
SIM3_full <-SIM3
SIM3 <- subset(SIM3, select = mask)
SIM3 <- SIM3[,1:6500]

trainSIM3 <- SIM3[1:(sampleSIM3),]
testSIM3 <- SIM3[-(1:(sampleSIM3)),]

###### Classification
nItems <- ncol(trainSIM3)
nPeriods <- nrow(trainSIM3)

# Change 0 values to NA to take NZD statistics
is.na(trainSIM3) <- trainSIM3 == 0
classificationSIM3 <- data.frame(MeanNZD = rep(NA, length = nItems), StdevNZD = NA, p = NA, CV2 = NA, Classification = NA)
classificationSIM3$MeanNZD <- apply(trainSIM3, 2, mean, na.rm = TRUE)
classificationSIM3$StdevNZD <- apply(trainSIM3, 2, sd, na.rm = TRUE)
classificationSIM3$p <- nPeriods / colSums(!is.na(trainSIM3))
classificationSIM3$CV2 <- (classificationSIM3$StdevNZD / classificationSIM3$MeanNZD)^2
classificationSIM3$Classification <- classifyDemand(classificationSIM3$CV2, classificationSIM3$p)$Type
mean(classificationSIM3$p)
mean(classificationSIM3$CV2)
table(classificationSIM3$Classification)

# Change NA to 0
trainSIM3[is.na(trainSIM3)] <- 0

###### Determine prices
RPS <- 174.952 
avgMonthlySales <- mean(colMeans(SIM3))
avgProductPrice <- RPS * mean(colMeans(SIM3))
RMS <- colMeans(SIM3) / avgMonthlySales
pricesSIM3 <- avgProductPrice / RMS













### SIM4

sampleSIM4 <- round(nrow(SIM4)*.70)
trainSIM4 <- SIM4[1:(sampleSIM4),]
testSIM4 <- SIM4[-(1:(sampleSIM4)),]

cond <- colSums(trainSIM4 != 0, na.rm = TRUE) < 2
mask <- !(cond)
SIM4_full <-SIM4
SIM4 <- subset(SIM4, select = mask)
SIM4 <- SIM4[,1:6500]

trainSIM4 <- SIM4[1:(sampleSIM4),]
testSIM4 <- SIM4[-(1:(sampleSIM4)),]

###### Classification
nItems <- ncol(trainSIM4)
nPeriods <- nrow(trainSIM4)

# Change 0 values to NA to take NZD statistics
is.na(trainSIM4) <- trainSIM4 == 0
classificationSIM4 <- data.frame(MeanNZD = rep(NA, length = nItems), StdevNZD = NA, p = NA, CV2 = NA, Classification = NA)
classificationSIM4$MeanNZD <- apply(trainSIM4, 2, mean, na.rm = TRUE)
classificationSIM4$StdevNZD <- apply(trainSIM4, 2, sd, na.rm = TRUE)
classificationSIM4$p <- nPeriods / colSums(!is.na(trainSIM4))
classificationSIM4$CV2 <- (classificationSIM4$StdevNZD / classificationSIM4$MeanNZD)^2
classificationSIM4$Classification <- classifyDemand(classificationSIM4$CV2, classificationSIM4$p)$Type
mean(classificationSIM4$p)
mean(classificationSIM4$CV2)
table(classificationSIM4$Classification)

# Change NA to 0
trainSIM4[is.na(trainSIM4)] <- 0

###### Determine prices
RPS <- 174.952 
avgMonthlySales <- mean(colMeans(SIM4))
avgProductPrice <- RPS * mean(colMeans(SIM4))
RMS <- colMeans(SIM4) / avgMonthlySales
pricesSIM4 <- avgProductPrice / RMS







save(SIM1, SIM2_1, SIM2_2, SIM2, SIM3, SIM4, trainSIM1, trainSIM2_1, trainSIM2_2, trainSIM2, trainSIM3, trainSIM4,
     testSIM1, testSIM2_1, testSIM2_2, testSIM2, testSIM3, testSIM4, pricesSIM1, pricesSIM2_1, pricesSIM2_2, pricesSIM2, pricesSIM3, pricesSIM4,
     sampleSIM1, sampleSIM2_1, sampleSIM2_2, sampleSIM2, sampleSIM3, sampleSIM4,
     file = "C:/Users/574244hn/OneDrive - Erasmus University Rotterdam/Spare parts demand forecasting/New simulated datasets/SIMs datasets.Rda")
