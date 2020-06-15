

##########################
##########################
##                      ##
##  ELASTIC NET MODELS  ##
##                      ##
##########################
##########################


#########################
# STEP 1: PRELIMINARIES #
#########################

#Install packages
install.packages("enetLTS")
install.packages("glmnet")
install.packages("ggplot2")
install.packages("robustHD")
install.packages("grid")
install.packages("reshape")
install.packages("parallel")
install.packages("cvTools")
install.packages("stats")

# Load libraries
library(enetLTS)
library(glmnet)
library(ggplot2)
library(robustHD)
library(grid)
library(reshape)
library(parallel)
library(cvTools)
library(stats)

# Set seed for reproducibility
set.seed(42)

# Set temporary working directory
setwd("C:/Users/s161386/OneDrive - TU Eindhoven/Bachelor End Project/Model/Data/")

# Load dataset
Dataset <- read.csv("Final_Dataset.csv")


####################################
# STEP 2: DEFINE TRAIN & TEST DATA #
####################################

# Define dependent and indepedent variables and subset by HDI score
TEA_HDI_All  <- data.matrix(Dataset[,4])
EEA_HDI_All  <- data.matrix(Dataset[,5])
X_HDI_All    <- data.matrix(Dataset[,c(7:ncol(Dataset))])

TEA_HDI_High <- data.matrix(Dataset[Dataset$HDI > 0.800,][,4])
EEA_HDI_High <- data.matrix(Dataset[Dataset$HDI > 0.800,][,5])
X_HDI_High   <- data.matrix(Dataset[Dataset$HDI > 0.800,][,c(7:ncol(Dataset))])

TEA_HDI_Low  <- data.matrix(Dataset[Dataset$HDI < 0.801,][,4])
EEA_HDI_Low  <- data.matrix(Dataset[Dataset$HDI < 0.801,][,5])
X_HDI_Low    <- data.matrix(Dataset[Dataset$HDI < 0.801,][,c(7:ncol(Dataset))])

# Split data into training (70%) and testing (30%) the datasets, per HDI subset
Train_HDI_All  <- sample(1:nrow(X_HDI_All) , .70*nrow(X_HDI_All) , replace = FALSE)
Train_HDI_High <- sample(1:nrow(X_HDI_High), .70*nrow(X_HDI_High), replace = FALSE)
Train_HDI_Low  <- sample(1:nrow(X_HDI_Low) , .70*nrow(X_HDI_Low) , replace = FALSE)

# Training and test sets for TEA, EEA and independent variables, per HDI subset
TEA_HDI_All_Train  <- TEA_HDI_All[ Train_HDI_All]
TEA_HDI_All_Test   <- TEA_HDI_All[-Train_HDI_All]
EEA_HDI_All_Train  <- EEA_HDI_All[ Train_HDI_All]
EEA_HDI_All_Test   <- EEA_HDI_All[-Train_HDI_All]
X_HDI_All_Train    <- X_HDI_All[ Train_HDI_All,]
X_HDI_All_Test     <- X_HDI_All[-Train_HDI_All,]

TEA_HDI_High_Train <- TEA_HDI_High[ Train_HDI_High]
TEA_HDI_High_Test  <- TEA_HDI_High[-Train_HDI_High]
EEA_HDI_High_Train <- EEA_HDI_High[ Train_HDI_High]
EEA_HDI_High_Test  <- EEA_HDI_High[-Train_HDI_High]
X_HDI_High_Train   <- X_HDI_High[ Train_HDI_High,]
X_HDI_High_Test    <- X_HDI_High[-Train_HDI_High,]

TEA_HDI_Low_Train  <- TEA_HDI_Low[ Train_HDI_Low]
TEA_HDI_Low_Test   <- TEA_HDI_Low[-Train_HDI_Low]
EEA_HDI_Low_Train  <- EEA_HDI_Low[ Train_HDI_Low]
EEA_HDI_Low_Test   <- EEA_HDI_Low[-Train_HDI_Low]
X_HDI_Low_Train    <- X_HDI_Low[ Train_HDI_Low,]
X_HDI_Low_Test     <- X_HDI_Low[-Train_HDI_Low,]


########################
# STEP 3: TRAIN MODELS #
########################

# Fitting the models, with 10-fold cross-validation

# Model 1 & 2: All HDI - TEA & EEA
Fit_TEA_HDI_All  <- enetLTS(X_HDI_All_Train,  TEA_HDI_All_Train,  family = "gaussian", nfold = 10)
Fit_EEA_HDI_All  <- enetLTS(X_HDI_All_Train,  EEA_HDI_All_Train,  family = "gaussian", nfold = 10) 

# Model 3 & 4: High HDI - TEA & EEA
Fit_TEA_HDI_High <- enetLTS(X_HDI_High_Train, TEA_HDI_High_Train, family = "gaussian", nfold = 10)
Fit_EEA_HDI_High <- enetLTS(X_HDI_High_Train, EEA_HDI_High_Train, family = "gaussian", nfold = 10)

# Model 5 & 6: Low HDI - TEA & EEA
Fit_TEA_HDI_Low  <- enetLTS(X_HDI_Low_Train,  TEA_HDI_Low_Train,  family = "gaussian", nfold = 10)
Fit_EEA_HDI_Low  <- enetLTS(X_HDI_Low_Train,  EEA_HDI_Low_Train,  family = "gaussian", nfold = 10)


###############################
# STEP 4: RESULTS OF TRAINING #
###############################

# Model 1: All HDI - TEA
Fit_TEA_HDI_All$num.nonzerocoef
Fit_TEA_HDI_All$a0
Fit_TEA_HDI_All$coefficients[Fit_TEA_HDI_All$coefficients > 0 | Fit_TEA_HDI_All$coefficients < 0]
Fit_TEA_HDI_All$fitted.values
Fit_TEA_HDI_All$residuals
Fit_TEA_HDI_All$rmse
length(Fit_TEA_HDI_All$wt[Fit_TEA_HDI_All$wt == 0])
plotCoef.enetLTS(Fit_TEA_HDI_All)
plotResid.enetLTS(Fit_TEA_HDI_All)
plotDiagnostic.enetLTS(Fit_TEA_HDI_All)

# Model 2: All HDI - EEA
Fit_EEA_HDI_All$num.nonzerocoef
Fit_EEA_HDI_All$a0
Fit_EEA_HDI_All$coefficients[Fit_EEA_HDI_All$coefficients > 0 | Fit_EEA_HDI_All$coefficients < 0]
Fit_EEA_HDI_All$fitted.values
Fit_EEA_HDI_All$residuals
Fit_EEA_HDI_All$rmse
length(Fit_EEA_HDI_All$wt[Fit_EEA_HDI_All$wt == 0])
plotCoef.enetLTS(Fit_EEA_HDI_All)
plotResid.enetLTS(Fit_EEA_HDI_All)
plotDiagnostic.enetLTS(Fit_EEA_HDI_All)

# Model 3: High HDI - TEA
Fit_TEA_HDI_High$num.nonzerocoef
Fit_TEA_HDI_High$a0
Fit_TEA_HDI_High$coefficients[Fit_TEA_HDI_High$coefficients > 0 | Fit_TEA_HDI_High$coefficients < 0]
Fit_TEA_HDI_High$fitted.values
Fit_TEA_HDI_High$residuals
Fit_TEA_HDI_High$rmse
length(Fit_TEA_HDI_High$wt[Fit_TEA_HDI_High$wt == 0])
plotCoef.enetLTS(Fit_TEA_HDI_High)
plotResid.enetLTS(Fit_TEA_HDI_High)
plotDiagnostic.enetLTS(Fit_TEA_HDI_High)

# Model 4: High HDI - EEA
Fit_EEA_HDI_High$num.nonzerocoef
Fit_EEA_HDI_High$a0
Fit_EEA_HDI_High$coefficients[Fit_EEA_HDI_High$coefficients > 0 | Fit_EEA_HDI_High$coefficients < 0]
Fit_EEA_HDI_High$fitted.values
Fit_EEA_HDI_High$residuals
Fit_EEA_HDI_High$rmse
length(Fit_EEA_HDI_High$wt[Fit_EEA_HDI_High$wt == 0])
plotCoef.enetLTS(Fit_EEA_HDI_High)
plotResid.enetLTS(Fit_EEA_HDI_High)
plotDiagnostic.enetLTS(Fit_EEA_HDI_High)

# Model 5: Low HDI - TEA
Fit_TEA_HDI_Low$num.nonzerocoef
Fit_TEA_HDI_Low$a0
Fit_TEA_HDI_Low$coefficients[Fit_TEA_HDI_Low$coefficients > 0 | Fit_TEA_HDI_Low$coefficients < 0]
Fit_TEA_HDI_Low$fitted.values
Fit_TEA_HDI_Low$residuals
Fit_TEA_HDI_Low$rmse
length(Fit_TEA_HDI_Low$wt[Fit_TEA_HDI_Low$wt == 0])
plotCoef.enetLTS(Fit_TEA_HDI_Low)
plotResid.enetLTS(Fit_TEA_HDI_Low)
plotDiagnostic.enetLTS(Fit_TEA_HDI_Low)

# Model 6: Low HDI - EEA
Fit_EEA_HDI_Low$num.nonzerocoef
Fit_EEA_HDI_Low$a0
Fit_EEA_HDI_Low$coefficients[Fit_EEA_HDI_Low$coefficients > 0 | Fit_EEA_HDI_Low$coefficients < 0]
Fit_EEA_HDI_Low$rmse
Fit_EEA_HDI_Low$fitted.values
Fit_EEA_HDI_Low$residuals
length(Fit_EEA_HDI_Low$wt[Fit_EEA_HDI_Low$wt== 0])
plotCoef.enetLTS(Fit_EEA_HDI_Low)
plotResid.enetLTS(Fit_EEA_HDI_Low)
plotDiagnostic.enetLTS(Fit_EEA_HDI_Low)


#######################
# STEP 5: TEST MODELS #
#######################

# Run fitted models on testing data

# Model 1 & 2: All HDI - TEA & EEA
Predicted_TEA_HDI_All  <- predict(Fit_TEA_HDI_All,  newX = X_HDI_All_Test,  type = "response")
Predicted_EEA_HDI_All  <- predict(Fit_EEA_HDI_All,  newX = X_HDI_All_Test,  type = "response")

# Model 3 & 4: High HDI - TEA & EEA
Predicted_TEA_HDI_High <- predict(Fit_TEA_HDI_High, newX = X_HDI_High_Test, type = "response")
Predicted_EEA_HDI_High <- predict(Fit_EEA_HDI_High, newX = X_HDI_High_Test, type = "response")

# Model 5 & 6: Low HDI - TEA & EEA
Predicted_TEA_HDI_Low  <- predict(Fit_TEA_HDI_Low,  newX = X_HDI_Low_Test,  type = "response")
Predicted_EEA_HDI_Low  <- predict(Fit_EEA_HDI_Low,  newX = X_HDI_Low_Test,  type = "response")


##############################
# STEP 6: RESULTS ON TESTING #
##############################

# Model 1 & 2: All HDI - TEA & EEA
TEA_HDI_All_Residuals_Test   <- TEA_HDI_All_Test-Predicted_TEA_HDI_All$reweighted.response[,1]
TEA_HDI_All_RMSE_Test   <- sqrt(mean((TEA_HDI_All_Test-Predicted_TEA_HDI_All$reweighted.response[,1])^2))
TEA_HDI_All_RMSE_Test

EEA_HDI_All_Residuals_Test   <- EEA_HDI_All_Test-Predicted_EEA_HDI_All$reweighted.response[,1]
EEA_HDI_All_RMSE_Test   <- sqrt(mean((EEA_HDI_All_Test-Predicted_EEA_HDI_All$reweighted.response[,1])^2))
EEA_HDI_All_RMSE_Test

# Model 3 & 4: High HDI - TEA & EEA
TEA_HDI_High_Residuals_Test  <- TEA_HDI_High_Test-Predicted_TEA_HDI_High$reweighted.response[,1]
TEA_HDI_High_RMSE_Test  <- sqrt(mean((TEA_HDI_High_Test-Predicted_TEA_HDI_High$reweighted.response[,1])^2))
TEA_HDI_High_RMSE_Test

EEA_HDI_High_Residuals_Test  <- EEA_HDI_High_Test-Predicted_EEA_HDI_High$reweighted.response[,1]
EEA_HDI_High_RMSE_Test  <- sqrt(mean((EEA_HDI_High_Test-Predicted_EEA_HDI_High$reweighted.response[,1])^2))
EEA_HDI_High_RMSE_Test

# Model 5 & 6: Low HDI - TEA & EEA
TEA_HDI_Low_Residuals_Test   <- TEA_HDI_Low_Test-Predicted_TEA_HDI_Low$reweighted.response[,1]
TEA_HDI_Low_RMSE_Test   <- sqrt(mean((TEA_HDI_Low_Test-Predicted_TEA_HDI_Low$reweighted.response[,1])^2))
TEA_HDI_Low_RMSE_Test

EEA_HDI_Low_Residuals_Test   <- EEA_HDI_Low_Test-Predicted_EEA_HDI_Low$reweighted.response[,1]
EEA_HDI_Low_RMSE_Test   <- sqrt(mean((EEA_HDI_Low_Test-Predicted_EEA_HDI_Low$reweighted.response[,1])^2))
EEA_HDI_Low_RMSE_Test