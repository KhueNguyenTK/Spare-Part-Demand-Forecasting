# Spare-Part-Demand-Forecasting
Forecasting the demands of spare parts, using 7 different methods on 8 data sets (industrial and simulated).

The raw data with full information can be found in the folder "All Data sets", which consists of four industrial and four simulated data sets. The irrelevant information (to the code) is removed in "RawData.RData", and this is used as the R environment for the R script "Cleaning_Description_Classification".

Cleaned data (stored in "CleanedData.Rda") is used in the R script "Methods", where 7 different forecasting methods (namely, Croston, SES, SBA, TSB, Willemain, MLP, LightGBM) are performed on the 8 data sets.

Reference:
de Haan, D. (2021). GitHub repository for benchmarking spare parts demand forecasting for intermittent demand (Version 1.0.0) [Computer software]. https://github.com/danieldehaan96/spdf
