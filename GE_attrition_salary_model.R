# Title: "General Electric Salary and Attrition Model"
# Class: DAT 690- Capstone in Data Analytics
# Author: Timothy Harrison-Reyes
# Date: July 28, 2022

==============================================================================

## Statement of Problem:
# GE has requested that the SNHU Data Analytics Team help their Human Resources (HR) department improve their employee retention rate. HR has spoken with mid-level managers and has identified that the company is losing capital and talent because of an increase in job postings across department, mainly regarding high potential employees. HR is requesting that the SNHU Data Analytics Team utilizes predictive analytic strategies based on the extensive data set they have provided to create a model to identify current employees that are at high risk of leaving the company. HR needs this information so they may work to retain them using various strategies based on the variables found from the analysis. Once the initial model is implemented and has shown success, HR is requesting that the Team predict the salary range needed to retain the employee. 
==============================================================================
## Environment Setup & Packages
# OS: Windows 11 64 Bit
# R version 4.1.3 (2022-03-10) -- "One Push-Up"
# Rattle version 5.5.1

install.packages("https://access.togaware.com/RGtk2_2.20.36.2.zip", repos=NULL)
install.packages("https://access.togaware.com/cairoDevice_2.28.zip", repos=NULL)
install.packages("rattle")

Documentation for install: https://rattle.togaware.com

## Data Preparation
# file: "DAT 690 Attrition-Proj2EmpSalaryTrain"
# Using Excel, remove employeeID, trainingtime, stockoptions, joblevel, and relationshipsatisfaction.

##Initiate Rattle
packages("rattle")
rattle()

## Data Input - TrainingData
# Load TrainingData dataset from file: "DAT 690 Attrition-Proj2EmpSalaryTrain"

library(readxl, quietly=TRUE)

 crs$dataset <- read_excel("D:/Dropbox/Documents/College_Univserity/SNHU/DAT 690- Advanced Data Analytics Capstone/DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx", guess_max=1e4)

 crs$dataset

# Build the train/validate/test datasets.

# nobs=1269 train=888 validate=190 test=191

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("DistanceFromHome", "Education",
                   "EnvironmentSatisfaction", "JobInvolvement",
                   "JobSatisfaction", "NumCompaniesWorked",
                   "PercentSalaryHike", "PerformanceRating",
                   "TotalWorkingYears", "WorkLifeBalance",
                   "YearsInCurrentRole", "YearsSinceLastPromotion",
                   "DiffFromSalary", "CurrentSalary")

crs$numeric   <- c("DistanceFromHome", "Education",
                   "EnvironmentSatisfaction", "JobInvolvement",
                   "JobSatisfaction", "NumCompaniesWorked",
                   "PercentSalaryHike", "PerformanceRating",
                   "TotalWorkingYears", "WorkLifeBalance",
                   "YearsInCurrentRole", "YearsSinceLastPromotion",
                   "DiffFromSalary", "CurrentSalary")

crs$categoric <- NULL

crs$target    <- "AnnualIncomeNeeded"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

## Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

============================================================================================================
# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(AnnualIncomeNeeded ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=500,
  mtry=3,
  importance=TRUE,
  na.action=randomForest::na.roughfix,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- crs$rf %>%
    randomForest::importance() %>%
    round(2)
    rn[order(rn[,1], decreasing=TRUE),]

============================================================================================================
# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(AnnualIncomeNeeded ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")
============================================================================================================
# Regression model 

# Build a Regression model.

crs$glm <- lm(AnnualIncomeNeeded ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")
============================================================================================================
# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

# Evaluate model performance on the training dataset. 

# RPART: Generate a Predicted v Observed plot for rpart model on DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx [**train**].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$train, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$train, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(AnnualIncomeNeeded=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="AnnualIncomeNeeded", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Decision Tree Model
 DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx [**train**]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# GLM: Generate a Predicted v Observed plot for glm model on DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx [**train**].

crs$pr <- predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[crs$train, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$train, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(AnnualIncomeNeeded=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="AnnualIncomeNeeded", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Linear Model
 DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx [**train**]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# Evaluate model performance on the training dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$train, c(crs$input, crs$target)]))

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$train, c(crs$input, crs$target)])$AnnualIncomeNeeded)
print(riskchart(crs$pr, 
    na.omit(crs$dataset[crs$train, c(crs$input, crs$target)])$AnnualIncomeNeeded, 
    title="Performance Chart Random Forest DAT 690 Attrition-Proj2EmpSalaryTrain.xlsx [**train**] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))

============================================================================================================

## Verification Data Set Model Evaluation
# Load verification dataset from file: DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx.

library(readxl, quietly=TRUE)

 crs$dataset <- read_excel("D:/Dropbox/Documents/College_Univserity/SNHU/DAT 690- Advanced Data Analytics Capstone/AttritionModelWorkspace_2022/DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx", guess_max=1e4)

 crs$dataset

#=======================================================================
# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=201 train=141 validate=30 test=30

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.
# All new variables remained to check for variable correlation. This should be done whenever a new variable(s) is introduced to continuously update the model. The orginal variables that were left out in the training dataset were ignored in this evaluation. 

crs$input     <- c("DistanceFromHome", "Education",
                   "EnvironmentSatisfaction", "JobInvolvement",
                   "JobLevel", "JobSatisfaction",
                   "NumCompaniesWorked", "AvgOverTime",
                   "PercentSalaryHike", "PerformanceRating",
                   "TotalWorkingYears", "WorkLifeBalance",
                   "YearsAtCompany", "YearsInCurrentRole",
                   "YearsSinceLastPromotion", "YearsWithCurrManager",
                   "DiffFromSalary", "CurrentSalary")

crs$numeric   <- c("DistanceFromHome", "Education",
                   "EnvironmentSatisfaction", "JobInvolvement",
                   "JobLevel", "JobSatisfaction",
                   "NumCompaniesWorked", "AvgOverTime",
                   "PercentSalaryHike", "PerformanceRating",
                   "TotalWorkingYears", "WorkLifeBalance",
                   "YearsAtCompany", "YearsInCurrentRole",
                   "YearsSinceLastPromotion", "YearsWithCurrManager",
                   "DiffFromSalary", "CurrentSalary")

crs$categoric <- NULL

crs$target    <- "AnnualIncomeNeeded"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("EMPID", "Age", "RelationshipSatisfaction", "StockOption", "TrainingTimesLastYear")
crs$weights   <- NULL

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(AnnualIncomeNeeded ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=500,
  mtry=4,
  importance=TRUE,
  na.action=randomForest::na.roughfix,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- crs$rf %>%
    randomForest::importance() %>%
    round(2)
    rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 0.09 secs

#=======================================================================
# Rattle timestamp: 2022-07-28 14:45:08 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

crs$eval <- evaluateRisk(crs$pr, na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$AnnualIncomeNeeded)
print(riskchart(crs$pr, 
    na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)])$AnnualIncomeNeeded, 
    title="Performance Chart Random Forest DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx [validate] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))


#=======================================================================
# Rattle timestamp: 2022-07-28 14:45:11 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# RF: Generate a Predicted v Observed plot for rf model on DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx [validate].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]))

# Obtain the observed output for the dataset.

obs <- subset(na.omit(crs$dataset[crs$validate, c(crs$input, crs$target)]), select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(AnnualIncomeNeeded=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="AnnualIncomeNeeded", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Random Forest Model
 DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

#=======================================================================
# Rattle timestamp: 2022-07-28 14:45:35 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(AnnualIncomeNeeded ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.01 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#=======================================================================
# Rattle timestamp: 2022-07-28 14:45:44 x86_64-w64-mingw32 

# Evaluate model performance on the validation dataset. 

# Risk Chart: requires the ggplot2 package.

library(ggplot2)

# Generate a risk chart.

# Rattle provides evaluateRisk() and riskchart().

crs$pr <- predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[crs$validate, c(crs$input, crs$target)])

crs$eval <- evaluateRisk(crs$pr, crs$dataset[crs$validate, c(crs$input, crs$target)]$AnnualIncomeNeeded)
print(riskchart(crs$pr, 
    crs$dataset[crs$validate, c(crs$input, crs$target)]$AnnualIncomeNeeded, 
    title="Performance Chart Linear DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx [validate] ", show.lift=FALSE, show.precision=FALSE, legend.horiz=FALSE))


#======================================================================

# Evaluate model performance on the validation dataset. 

# GLM: Generate a Predicted v Observed plot for glm model on DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx [validate].

crs$pr <- predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[crs$validate, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$validate, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(AnnualIncomeNeeded=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="AnnualIncomeNeeded", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Linear Model
 DAT 690 Attrition-Proj2EmpSalaryVerify.xlsx [validate]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()