# R code for constructing the Composite method

load("~/Library/CloudStorage/OneDrive-ErasmusUniversityRotterdam/Spare parts demand forecasting/FINAL/Datasets/Cleaned industrial datasets.Rda")
load("~/Library/CloudStorage/OneDrive-ErasmusUniversityRotterdam/Spare parts demand forecasting/FINAL/Predictions (exclude Composite).RDa")
load("~/Library/CloudStorage/OneDrive-ErasmusUniversityRotterdam/Spare parts demand forecasting/FINAL/Datasets/Classifications.Rda")

targetFillRates <- c(75:99) / 100
nTFR <- length(targetFillRates)

# Selected methods
# Based on cost assessment
selectedMethodsAvg <- data.frame(Classification = c("Erratic", "Lumpy", "Smooth", "Intermittent"),
                              Method = c("MLP", "Croston", "Willemain", "SES"))
selectedMethodsTot <- data.frame(Classification = c("Erratic", "Lumpy", "Smooth", "Intermittent"),
                                 Method = c("MLP", "Croston", "Willemain", "Croston"))

CompositeAvg_OIL <- NULL
CompositeTot_OIL <- NULL
for (col in 1:ncol(OIL)) {
  if (classificationOIL$Classification[col] == "Erratic") {
    CompositeAvg_OIL <- cbind(CompositeAvg_OIL, MLP_OIL[,col])
    CompositeTot_OIL <- cbind(CompositeTot_OIL, MLP_OIL[,col])
  } else if (classificationOIL$Classification[col] == "Lumpy") {
    CompositeAvg_OIL <- cbind(CompositeAvg_OIL, Croston_OIL[,col])
    CompositeTot_OIL <- cbind(CompositeTot_OIL, Croston_OIL[,col])
  } else if (classificationOIL$Classification[col] == "Smooth") {
    CompositeAvg_OIL <- cbind(CompositeAvg_OIL, Willemain_OIL[,col])
    CompositeTot_OIL <- cbind(CompositeTot_OIL, Willemain_OIL[,col])
  } else {
    CompositeAvg_OIL <- cbind(CompositeAvg_OIL, SES_OIL[,col])
    CompositeTot_OIL <- cbind(CompositeTot_OIL, Croston_OIL[,col])
  }
}
CompositeAvg_OIL <- as.data.frame(CompositeAvg_OIL)
CompositeTot_OIL <- as.data.frame(CompositeTot_OIL)

save(CompositeAvg_MAN, CompositeTot_MAN, CompositeAvg_BRAF, CompositeTot_BRAF, CompositeAvg_AUTO, CompositeTot_AUTO, CompositeAvg_OIL, CompositeTot_OIL, 
     file = "~/Library/CloudStorage/OneDrive-ErasmusUniversityRotterdam/Spare parts demand forecasting/FINAL/Composite method/Predictions.Rda")
