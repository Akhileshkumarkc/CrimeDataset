##################
# Final Project :
######
# Step 1: Read the dataset.
# load the data set, add it to my data.
# load the attribute names and set the dataset.
# use base, mark NA as ?
#communities.data <- read.csv("communities.data.csv", header=FALSE, na.strings="?")
#
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

# step 2: clean up the data.
# remove the column community name.
myData$communityname = NULL
myDataClean = na.omit(myData)
as.factor(myData$fold)
as.factor(myData$state)
# remove all missing data columns.
myDataPred[,c(2,3,c(101:117),c(121:124),126)] =NULL

####################################################################
# lets do a PCA part 1
myData2 = model.matrix(ViolentCrimesPerPop ~. -1, data = myDataClean)

head(myData2)
pcaCredit = prcomp(myData2,scale.=TRUE)
plot(pcaCredit$x[1:2000])
head(pcaCredit$x)
pcaVar = pcaCredit$sdev^2
pve=pcaVar/sum(pcaVar)
plot(pve)
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,
     type='b')
# around 50 PCA explain the 90% Variance.
# around 40 PCA explain the 86% variance.
# lets take 40 PCA>


####################################################################################3
# lets do a PCA part 2
myDataPred = myDataClean[,-127]
myDataPred
# removing all the ? columns.
myDataPred[,c(2,3,c(101:117),c(121:124),126)] =NULL
dim(myDataPred)
cc_PCA = prcomp(myDataPred, scale. = TRUE, center = TRUE)
plot(cc_PCA)