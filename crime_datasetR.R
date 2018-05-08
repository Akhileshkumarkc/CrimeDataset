###################################################################
# Final Project Part 1: Crime Dataset.
# Member: 
# 1. Akhilesh Kumar Kagalvadi Chinnaswamy (axk167131)
# 2. Shalin Anilkumar Amin (saa161030)
# 3. Saikrishna Kanukuntla (sxk160432)
# 4. Nanditha Valsaraj (nxv160930)
##################################################################
# Crime Dataset contains 127 attribute, with target attribute ViolentCrimesPerPop.
# This is carried out in following steps:
# Step 1: Read the dataset.
# Step 2: Clean the dataset.
# Step 3 : DO PCA
# step 4: Apply linear regression with cross validation with k=10 using glm.net
# step 5; calculate MSE 
###################################################################

# Step 1: Read the dataset.

# load the data set, add it to my data.
# use base, mark NA as ?
# communities.data <- read.csv("communities.data.csv", header=FALSE, na.strings="?")

col_names = c (
  "state","county","community","communityname","fold","population","householdsize","racepctblack","racePctWhite","racePctAsian",
  "racePctHisp","agePct12t21","agePct12t29","agePct16t24","agePct65up", "numbUrban", "pctUrban", "medIncome", "pctWWage", "pctWFarmSelf", 
  "pctWInvInc", "pctWSocSec", "pctWPubAsst", "pctWRetire", "medFamInc", "perCapInc", "whitePerCap", "blackPerCap", "indianPerCap", "AsianPerCap",
  "OtherPerCap" ,"HispPerCap", "NumUnderPov", "PctPopUnderPov", "PctLess9thGrade", "PctNotHSGrad", "PctBSorMore", "PctUnemployed", "PctEmploy", "PctEmplManu",
  "PctEmplProfServ", "PctOccupManu", "PctOccupMgmtProf", "MalePctDivorce" , "MalePctNevMarr" , "FemalePctDiv", "TotalPctDiv", "PersPerFam", "PctFam2Par", "PctKids2Par",
  "PctYoungKids2Par", "PctTeen2Par", "PctWorkMomYoungKids", "PctWorkMom", "NumIlleg", "PctIlleg", "NumImmig", "PctImmigRecent", "PctImmigRec5", "PctImmigRec8",
  "PctImmigRec10", "PctRecentImmig", "PctRecImmig5", "PctRecImmig8", "PctRecImmig10", "PctSpeakEnglOnly", "PctNotSpeakEnglWell", "PctLargHouseFam", "PctLargHouseOccup", "PersPerOccupHous",
  "PersPerOwnOccHous", "PersPerRentOccHous", "PctPersOwnOccup", "PctPersDenseHous", "PctHousLess3BR", "MedNumBR", "HousVacant", "PctHousOccup", "PctHousOwnOcc", "PctVacantBoarded",
  "PctVacMore6Mos", "MedYrHousBuilt", "PctHousNoPhone", "PctWOFullPlumb", "OwnOccLowQuart", "OwnOccMedVal", "OwnOccHiQuart", "RentLowQ", "RentMedian", "RentHighQ",
  "MedRent", "MedRentPctHousInc", "MedOwnCostPctInc", "MedOwnCostPctIncNoMtg", "NumInShelters", "NumStreet", "PctForeignBorn", "PctBornSameState", "PctSameHouse85", "PctSameCity85",
  "PctSameState85", "LemasSwornFT", "LemasSwFTPerPop", "LemasSwFTFieldOps", "LemasSwFTFieldPerPop", "LemasTotalReq", "LemasTotReqPerPop", "PolicReqPerOffic", "PolicPerPop", "RacialMatchCommPol",
  "PctPolicWhite", "PctPolicBlack", "PctPolicHisp", "PctPolicAsian", "PctPolicMinor", "OfficAssgnDrugUnits", "NumKindsDrugsSeiz", "PolicAveOTWorked", "LandArea", "PopDens",
  "PctUsePubTrans", "PolicCars", "PolicOperBudg", "LemasPctPolicOnPatr", "LemasGangUnitDeploy", "LemasPctOfficDrugUn", "PolicBudgPerPop", "ViolentCrimesPerPop")
colnames(communities.data) = col_names;
myData <- communities.data
dim(myData)
#1994 128

# Step 2: clean up the data.

# remove name data and make fold and state as factors.
myData$communityname = NULL
as.factor(myData$fold)
as.factor(myData$state)

# remove all missing data columns, they will not help us predict.
myDataClean = myData[,c(2,3,c(101:117),c(121:124),126)] =NULL
myDataClean = myData
dim(myDataClean)

####################################################################
# Step 3 : DO PCA
# Target attribute: ViolentCrimesPerPop

myData2 = model.matrix(ViolentCrimesPerPop ~. -1, data = myDataClean)
head(myData2)

pca_CD = prcomp(myData2,scale.=TRUE)
pcaVar = pca_CD$sdev^2
pve=pcaVar/sum(pcaVar)
plot(pve)
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')

# around 20 PCA explain the 90% Variance.
# lets take 20 PCA.
pca_20_attr = pcaCredit$x[,1:20]
combined_data = as.data.frame(cbind(pca_20_attr,myDataClean$ViolentCrimesPerPop))


# Take a 75 - 25 split.
set.seed(101)
sample <- sample.int(n = nrow(combined_data), size = floor(.75*nrow(combined_data)), replace = F)
train <- combined_data[sample, ]
test  <- combined_data[-sample, ]
summary(test)

# Step 4: Apply linear regression with cross validation with k=10 using glm.net

# Attach the "boot" package
# V21 is ViolentCrimesPerPop attribute.
glm.fit = glm(V21~., data=train)
cv.out = cv.glm(train, glm.fit, K=10)

# step 5: calculate MSE  
trainMSE = cv.out$delta[1]
cv_data.pred = predict(glm.fit,test)
testMSE = sum((cv_data.pred -test$V21)^2)/nrow(test)
trainMSE
testMSE

# Result:
# MSE On Test data : 0.05366028
# MSE On Train data : 0.05791931

