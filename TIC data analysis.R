# Author: Timothy Harrison-Reyes
# Date: March 6, 2022 


#Problem to address: TIC has hired the SNHU Data Analytics teams to answer their research question: “Can we predict who would be interested in buying a caravan insurance policy and why?” 





#import data
tic_data<-read.delim('C:/Users/Profe/Documents/ticdata2000.txt', header=FALSE)

#view data
View(tic_data)

#create columns for analysis

colnames(tic_data) <- c("MOSTYPE","MAANTHUI","MGEMOMV","MGEMLEEF","MOSHOOFD","MGODRK","MGODPR","MGODOV","MGODGE","MRELGE",  "MRELSA","MRELOV","MFALLEEN","MFGEKIND","MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG", "MBERZELF","MBERBOER","MBERMIDD","MBERARBG","MBERARBO","MSKA","MSKB1","MSKB2","MSKC","MSKD","MHHUUR", "MHKOOP","MAUT1","MAUT2","MAUT0","MZFONDS","MZPART","MINKM30","MINK3045","MINK4575","MINK7512","MINK123M", "MINKGEM","MKOOPKLA","PWAPART","PWABEDR","PWALAND","PPERSAUT","PBESAUT","PMOTSCO","PVRAAUT","PAANHANG","PTRACTOR","PWERKT","PBROM","PLEVEN","PPERSONG","PGEZONG","PWAOREG","PBRAND","PZEILPL","PPLEZIER","PFIETS", "PINBOED","PBYSTAND","AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT","AMOTSCO","AVRAAUT","AAANHANG", "ATRACTOR","AWERKT","ABROM","ALEVEN","APERSONG","AGEZONG","AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS", "AINBOED","ABYSTAND","CARAVAN")

#view formatted data with new column headings
View(tic_data)

#setting target variable as categorical and assigned to dependent variable CARAVAN
tic_data$CARAVAN <-as.factor(tic_data$CARAVAN)

#split our data set into training and validation data sets. 70% of original will be for training and 30% will be for validation
set.seed(100)
train <- sample(nrow(tic_data), 0.7*nrow(tic_data), replace = FALSE)
TrainSet <- tic_data[train,]
ValidSet <- tic_data[train,]

#summary analysis
summary(TrainSet)
summary(ValidSet)

#Create first model (modelA) using random forest and training set, this is a general model with no limitations
modelA <- randomForest(CARAVAN ~ ., data = TrainSet, importance = TRUE)

#create second model (modelB) by running the data through 1000 decision trees and only 6 variables
modelB <- randomForest(CARAVAN ~ ., data = TrainSet,ntree = 1000, mtry = 6, importance = TRUE)

#create predication based on training set and applying model B
PTrain <- predict(modelB, TrainSet, type = "class")
table(PTrain, TrainSet$CARAVAN)

#Create validation model predictions using model b and valid set
PValid <- predict(modelB, ValidSet, type = "class")
mean(PValid == ValidSet$CARAVAN)

#Create a table of the validated set
table(PValid, ValidSet$CARAVAN)

#Next Level of Analysis: adding eval data to apply to our model, first we need to import and assign columns like before
tic_eval<-read.delim('C:/Users/Profe/Documents/ticeval2000.txt', header=FALSE)
tic_eval$x86 <- NA
colnames(tic_eval) <- c("MOSTYPE","MAANTHUI","MGEMOMV","MGEMLEEF","MOSHOOFD","MGODRK","MGODPR","MGODOV","MGODGE","MRELGE",  "MRELSA","MRELOV","MFALLEEN","MFGEKIND","MFWEKIND","MOPLHOOG","MOPLMIDD","MOPLLAAG","MBERHOOG", "MBERZELF","MBERBOER","MBERMIDD","MBERARBG","MBERARBO","MSKA","MSKB1","MSKB2","MSKC","MSKD","MHHUUR", "MHKOOP","MAUT1","MAUT2","MAUT0","MZFONDS","MZPART","MINKM30","MINK3045","MINK4575","MINK7512","MINK123M", "MINKGEM","MKOOPKLA","PWAPART","PWABEDR","PWALAND","PPERSAUT","PBESAUT","PMOTSCO","PVRAAUT","PAANHANG","PTRACTOR","PWERKT","PBROM","PLEVEN","PPERSONG","PGEZONG","PWAOREG","PBRAND","PZEILPL","PPLEZIER","PFIETS", "PINBOED","PBYSTAND","AWAPART","AWABEDR","AWALAND","APERSAUT","ABESAUT","AMOTSCO","AVRAAUT","AAANHANG", "ATRACTOR","AWERKT","ABROM","ALEVEN","APERSONG","AGEZONG","AWAOREG","ABRAND","AZEILPL","APLEZIER","AFIETS", "AINBOED","ABYSTAND","CARAVAN")

View(tic_eval)

#Use Model B on the evaluation data set created above
PEval <- predict(modelB, tic_eval, type = "class")
tic_eval$CARAVAN <- PEval
View(tic_eval)
table(tic_eval$CARAVAN)

#Save as external CSV file
write.csv(PEval, file = "C:/Users/Profe/Documents/final_prediction_analysis_output.csv")diction_analysis_output.csv")
