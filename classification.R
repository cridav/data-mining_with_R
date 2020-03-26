#Author: Cristiam Martin Jackson
#Classification

#Set working directory
setwd('/home/cristiam/Documents/ISI_WEiTI/II/EDAMI/LAB3_CDMJ')
#getwd()

# Task: Build the best possible classifier for the Age of the Abalone,
# based on the given dataset and classes: "Young", "Middle" and "Old"

# Every experiment will show their respective confusion matrix and overall accuracy, which will be taken
# into account at the moment of selecting the best classifier for the present problem:
# Classify the Age of the Abalone, using different measurements.

# The data set contains the next variables:

#################################
# Name	        	Data         Type	  Meas.	Description
# ----        		--------- 	-----	  -----------
# Sex             nominal			M, F, and I (infant)
# Length          continuous	mm	    Longest shell measurement
# Diameter	      continuous	mm	    perpendicular to length
# Height		      continuous	mm	    with meat in shell
# Whole weight	  continuous	grams	  whole abalone
# Shucked weight	continuous	grams	  weight of meat
# Viscera weight	continuous	grams	  gut weight (after bleeding)
# Shell weight	  continuous	grams	  after being dried
# Rings		        integer			+1.5    gives the age in years
#################################

# The age will be separated into three different classes:
# Young: from 0 to 8 years
# Middle: from 9 to 10 years
# Old: more than 10 years

#library loading
library(caret)
library(party)
library(rpart)
library(e1071)
library(rpart.plot)

#Downloading the datased: abalone
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data', 'abalone.data')
abalone  = read.table("abalone.data", header = FALSE, sep=",", na.strings= "*")
colnames(abalone) <- c('Sex', 'Length','Diameter','Height','Whole', 'Shucked', 'Viscera','Shell','Rings')
# Duplicate, for correlation analisys
abalone1 = abalone

# Aplying the classes: Old, Middle and Young
abalone$Age = lapply(abalone[,'Rings'], function (x)
{
  if(x >10)  { "Old"}
  else if(x >8)  {"Middle"}
  else { "Young"}   
})
abalone$Age = unlist(abalone$Age);
abalone$Age = as.factor(abalone$Age)
#
##
####
######    CORRELATION
# Pre analisys using correlation  between variables
# Analize the correlation of the data, some variables might be more useful to be consider for future
# classification tasks

# Aplying the classes: Old=0.9, Middle=0.6 and Young=0.3
abalone1$Age = lapply(abalone1[,'Rings'], function (x)
{
  if(x >10)  {0.9}
  else if(x >8)  {0.6}
  else {0.3}   
})
abalone1$Age = unlist(abalone1$Age);
abalone1$Age = as.numeric(abalone1$Age)
abalone1$Rings = NULL
# Correlation of Age with the independent variables
for(col in colnames(abalone1)){
  if(col != 'Age' & col != 'Sex'){
    print(cor(abalone1[col], abalone1["Age"], use = "everything", method = c("pearson", "kendall", "spearman")))
  }
}

# =====================================
#         Age
# Length 0.5836931
#         Age
# Diameter 0.5998852
#         Age
# Height 0.5622943
#         Age
# Whole 0.5819899
#         Age
# Shucked 0.4935167
#         Age
# Viscera 0.5643571
#         Age
# Shell 0.6327891

# The correlation between the data variables and the Age (Old, Middle, Young), show that there is a highest correlation
# of the Age with the Shell weight, diameter and length, however is not stronger enough to consider not to use the rest
# of the variables
# It is possible to predict that the shell weight represents a variable of importance for the Age classification
# while the weight of the meat (SHUCKED) is not so relevant (as we could have intuitively said).
######

#GDelete the column "Rings" in abalone, only "Age" is required
abalone$Rings <- NULL

#show(abalone)

#duplicate the dataset, "abalone_test" will be used
abalone_test = abalone
summary(abalone_test)

#Split the dataset into two: training (70%) and test (30%)
#The fixed random seed makes the results repeatable
set.seed(5235)

idTrainData <- unlist(createDataPartition(abalone$Age,p=0.7))
str(idTrainData)

trainData <-abalone_test[idTrainData,]
testData <-abalone_test[-idTrainData,]

table(trainData$Age)
table(testData$Age)

#####################
# Preprocess
#procMethod <- preProcess(abalone_test, method = c("center","scale"))
#print(procMethod)
#procData <- predict(procMethod,abalone_test)
################################################################

#Classification
# Three classificators will be implemented: ctree, rpart and naiveBayes

#decision tree buidling
abalone_ctree <- ctree(Age~., data=trainData)

#check the prediction - the confusion matrix
table(trainData$Age,predict(abalone_ctree))
cm <-confusionMatrix(predict(abalone_ctree), trainData$Age, mode="everything")
cm
# Class: Middle Class: Old Class: Young
# Sensitivity                 0.4283     0.6051       0.8660
# Specificity                 0.8128     0.8577       0.7856

#Obtain the overall accuracy
acc <- cm$overall['Accuracy']
acc
# Accuracy 
# 0.6369231

#classification of test data
testPred <- predict(abalone_ctree, newdata = testData)
#confusion matrix for the test 1
table(testData$Age,testPred)
cmt<-confusionMatrix(testPred, testData$Age, mode = "everything")
cmt
#                      Class: Middle Class: Old Class: Young
# Sensitivity                 0.4015     0.5876       0.8460
# Specificity                 0.8107     0.8301       0.7831
#accurracy
acct <- cmt$overall['Accuracy']
acct
# Accuracy 
# 0.6158147

#changing parameters of the algorithm
# ctree parameters
# The objective now is to obtain the best parameters for the classification tree, using a for loop
# the maximum depth parameter is going to be updated, in order to find the most appropriate depth 
# for the tree, considerating that the smaller the better, provided that the accuracy obtained is worth it
acc_best = 0
for (i in 2:20) {
  myParam =ctree_control(minsplit=35, maxdepth=i)
  abalone_Ctree2<-ctree(Age~., data=trainData,controls = myParam )
  cm2 <-confusionMatrix(predict(abalone_Ctree2), trainData$Age, mode="everything")
  cat(sprintf("depth:%s\n",i))
  print(cm2$byClass[0:3])
  acc2 <- cm2$overall['Accuracy']
  cat(sprintf("Overall accuracy:%s\n",acc2))
  # Save the best parameter
  if(acc2 > acc_best){
    acc_best = acc2
    myParam_best =ctree_control(minsplit=35, maxdepth=i)
    abalone_Ctree2<-ctree(Age~., data=trainData,controls = myParam_best )
  }
}
# ======================
# After 20 iterations, the sensitivity obtained with different depths is plot, the sensitivity correspond
# to the classes: Middle, Old and Young respectively,
# The best overall accuracy was present for a tree with maxdepth = 5
# 42.82% of the values were correctly classified for the class: Middle
# 60.51% of the values were correctly classified for the class: Old
# 86.59% of the values were correctly classified for the class: Young

# depth:2
# [1] 0.2437972 0.7897335 0.6934010
# Overall accuracy:0.584273504273504
# depth:3
# [1] 0.5911543 0.3869694 0.8060914
# Overall accuracy:0.592820512820513
# depth:4
# [1] 0.4843581 0.5992103 0.8060914
# Overall accuracy:0.632478632478632
# depth:5
# [1] 0.4282632 0.6051333 0.8659898
# Overall accuracy:0.636923076923077

# Best configuration obtained
myParam_best
# confusion Matrix
table(trainData$Age,predict(abalone_Ctree2) )

# Here can be appreciated that the class that obtained the highest number of right classifications
# is the Young one (86.60%), the class "Middle" obtained the worst (42.83%)
# Middle Old Young
# Middle    397 232   298
# Old       282 613   118
# Young      92  40   853
cm2 <-confusionMatrix(predict(abalone_Ctree2), trainData$Age, mode="everything")
cm2
# Class: Middle Class: Old Class: Young
# Sensitivity                 0.4283     0.6051       0.8660
# Specificity                 0.8128     0.8577       0.7856
#Obtain the overall accuracy
acc2 <- cm2$overall['Accuracy']
acc2
# Accuracy 
# 0.6369231 


#classification of the TEST DATA +++++++++++++++++++++++++++++++++++++
testPred <- predict(abalone_Ctree2, newdata = testData)
cmt2<-confusionMatrix(testPred, testData$Age, mode = "everything")
cmt2
table( testData$Age,testPred)
#accurracy
mean(testPred == testData$Age) 


# The class (Middle) happens to classify the true positives with a rate of (40.15%) which is too low, is not
# even over 50%, the classifier for the Young class presents a good accuracy (84.60%)

# Statistics by Class:
#   
#                    Class: Middle Class: Old Class: Young
# Sensitivity                 0.4015     0.5876       0.8460
# Specificity                 0.8107     0.8301       0.7831
# Pos Pred Value              0.4953     0.6472       0.6648
# Neg Pred Value              0.7454     0.7914       0.9091
# Precision                   0.4953     0.6472       0.6648
# Recall                      0.4015     0.5876       0.8460
# F1                          0.4435     0.6159       0.7445
# Prevalence                  0.3163     0.3466       0.3371
# Detection Rate              0.1270     0.2037       0.2851
# Detection Prevalence        0.2564     0.3147       0.4289
# Balanced Accuracy           0.6061     0.7088       0.8146

# > table( testData$Age,testPred)
# testPred
# Middle Old Young
# Middle    159 109   128
# Old       127 255    52
# Young      35  30   357
# > #accurracy
#   > mean(testPred == testData$Age) 
# [1] 0.6158147


# Obtained tree for the best result using the algorith CTREE
#print tree
print(abalone_Ctree2)
#graphical presentation of the tree
# from left to right: Middle, Old and Young
plot(abalone_Ctree2, type="simple")

# The overall accuracy is 61.58%
# The true positive classifications per class are shown below
#                   Class: Middle     Class:Old     Class: Young
# Sensitivity             0.4015          0.5876       0.8460
#
###
#####
########
############
#################
################################################################
# Classification using RPART

# Recursive partitioning and regression trees
# Build the tree
rpTree <- rpart(Age~.,  method="class", data=trainData)

# information about a tree
# The plot shows the best point for a tree of size = 5 (the same depth found after the loop iteration in the CTREE algorithm)
printcp(rpTree)
plotcp(rpTree)
#prp(rpTree, faclen = 0, cex = 0.7, extra = 1, main="Classification for Abalone")

#classification of the training data
trainPred = predict(rpTree,trainData,type = "class")
cm<-confusionMatrix(trainPred, trainData$Age, mode = "everything")
table(trainData$Age,trainPred)
cm

# The Sensitivity (elements correctly classified per class) are over 50% for the three classes,
# showing a improvements copared to the CTREE algorithm, however, the class "Young" shows a worsen of around 15%
# compared to the CTREE algorithm
# The overall accuracy of this classifier is 62.66% (for the training data)

# Class: Middle Class: Old Class: Young
# Sensitivity                 0.5868     0.5982       0.6934
# Specificity                 0.6892     0.8593       0.8959
acc_rpart<-cm$overall['Accuracy']
acc_rpart
# Accuracy 
# 0.6266667

#classification of test data
testPred = predict(rpTree,testData,type = "class")
cmtest<-confusionMatrix(testPred, testData$Age, mode = "everything")
table(testData$Age,testPred)
cmtest
# Class: Middle Class: Old Class: Young
# Sensitivity                 0.5606     0.5576       0.7204
# Specificity                 0.6881     0.8460       0.8904

# testPred
# Middle Old Young
# Middle    180 105   111
# Old       122 256    56
# Young      48  18   356
acctest_rpart<-cmtest$overall['Accuracy']
acctest_rpart
# Accuracy 
# 0.6325879

# After classifying the test data, the classifier threw an overall accuracy of 63.25% and a sensitivity over 55%
# for all the classes, the highes value was found again for "Young" with 72.04%

#////////////////////////////////////////
#application of loss matrix
#minimize penalti on the main diagonal and maximize penalti elsewhere
# 0 1 1
# 1 0 1
# 1 2 0
lossM=matrix(c(0,1,1,1,0,1,1,2,0), byrow=TRUE, nrow=3)
rpTree2 <- rpart(Age~.,  method="class", data=trainData, parms = list(loss = lossM ))

#classification of the training data
trainPred2 = predict(rpTree2,trainData,type = "class")
cmrp2<-confusionMatrix(trainPred2, trainData$Age, mode = "everything")
cmrp2
#                       Class: Middle Class: Old Class: Young
# Sensitivity                 0.4822     0.5933       0.8122
# Specificity                 0.7798     0.8672       0.8026
table(trainData$Age,trainPred2)
# trainPred2
# Middle Old Young
# Middle    447 232   248
# Old       277 601   135
# Young     163  22   800
acc_rpart2<-cmrp2$overall['Accuracy']
acc_rpart2
# Accuracy 
# 0.6317949

#classification of test data
testPred2 = predict(rpTree2,testData,type = "class")
table(testData$Age,testPred2)
# testPred2
# Middle Old Young
# Middle    180 105   111
# Old       122 256    56
# Young      48  18   356
mean(testPred2 == testData$Age) 
# Accuracy
# 0.6325879


## Using the algorithm  RPART, the best solution is given below
# Obtained tree with the best accuracy (0.6325)
prp(rpTree, faclen = 0, cex = 0.7, extra = 1, main="Classification for Iris")


#///////////////////////////////////////////////////
###
####
######
########
###########
##################

#Naive Bayes classifier

nbClasif <- naiveBayes(Age~., data=trainData, laplace = 0)

print(nbClasif)
#the confusion matrix for train data
table(trainData$Age,predict(nbClasif,trainData))
# Middle Old Young
# Middle    361 340   226
# Old       327 549   137
# Young     179  34   772
trainPred = predict(nbClasif,trainData)
cmtrainbay<-confusionMatrix(trainPred, trainData$Age, mode = "everything")
cmtrainbay
#                     Class: Middle Class: Old Class: Young
# Sensitivity                 0.3894     0.5420       0.7838
# Specificity                 0.7467     0.8044       0.8129

mean(trainPred == trainData$Age)
# Overall accuracy for training
# 0.5750427


#classification of test data
testPred = predict(nbClasif,testData)
cmtestbay<-confusionMatrix(testPred, testData$Age, mode = "everything")
cmtestbay
# Class: Middle Class: Old Class: Young
# Sensitivity                 0.3510     0.5691       0.8199
# Specificity                 0.7850     0.7824       0.8096
table(testData$Age,testPred)
# testPred
# Middle Old Young
# Middle    139 158    99
# Old       128 247    59
# Young      56  20   346
mean(testPred == testData$Age)  
# Overall accuracy for test
# 0.5846645

# Since Naive Bayes is a parametric classifier, it implies that it won't
# change the result after different iterations, provided that the data remains the
# same.


###################################################################################################
###################################################################################################
###################################################################################################
###################################################################################################

# In summary, after testing three different algorithms, with different configurations, it was found that
# for the same data set,the best classifier is the tree given below, created by using the rpart algorithm
prp(rpTree, faclen = 0, cex = 0.7, extra = 1, main="Classification for Abalone")

# Despite the classifier shows an accuracy  ~63% it was a good result, based on the classification per class
# As expected, in the resulting tree it can be seen that the main variable used for the classification
# is the Shell weight (anticipated by the correlation analisys), this classification throw a better results
# for the class "Young" (if the shell weight is below 0.15 grams, it can be said that with an accuracy of ~72%, 
# the Abalone is young)

# The parameters used in order to obtain this classifier are:
trainPred = predict(rpTree,trainData,type = "class")

cm<-confusionMatrix(trainPred, trainData$Age, mode = "everything")
table(trainData$Age,trainPred)
cm

# The main criterium why this was selected as the best one, is because it was the only classifier that showed
# a sensitivy over 50% for all the classes, especially for the "Middle", which presented more troubles for the
# other classifiers already tested to reach this point.

# The overall accuracy of this classifier is 62.66% (for the training data)
# and 63.25% for the testing data

# CLASSIFIER RPTREE WITH TRAINING DATA

# Reference
# Prediction Middle Old Young
# Middle    544 342   279
# Old       246 606    23
# Young     137  65   683
# 
# Overall Statistics
# 
# Accuracy : 0.6267          
# 95% CI : (0.6088, 0.6442)
# No Information Rate : 0.3463          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4414          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
# 
# Statistics by Class:
# 
#                      Class: Middle Class: Old Class: Young
# Sensitivity                 0.5868     0.5982       0.6934
# Specificity                 0.6892     0.8593       0.8959
# Pos Pred Value              0.4670     0.6926       0.7718
# Neg Pred Value              0.7824     0.8015       0.8520
# Precision                   0.4670     0.6926       0.7718
# Recall                      0.5868     0.5982       0.6934
# F1                          0.5201     0.6419       0.7305
# Prevalence                  0.3169     0.3463       0.3368
# Detection Rate              0.1860     0.2072       0.2335
# Detection Prevalence        0.3983     0.2991       0.3026
# Balanced Accuracy           0.6380     0.7288       0.7946

acc_rpart<-cm$overall['Accuracy']
acc_rpart
# Accuracy 
# 0.6266667


# CLASSIFIER RPTREE WITH TESTING DATA

#classification of test data
testPred = predict(rpTree,testData,type = "class")
cmtest<-confusionMatrix(testPred, testData$Age, mode = "everything")
table(testData$Age,testPred)
cmtest

# testPred
# Middle Old Young
# Middle    222 108    66
# Old       167 242    25
# Young     100  18   304
# > cmtest
# Confusion Matrix and Statistics
# 
# Reference
# Prediction Middle Old Young
# Middle    222 167   100
# Old       108 242    18
# Young      66  25   304
# 
# Overall Statistics
# 
# Accuracy : 0.6134          
# 95% CI : (0.5858, 0.6405)
# No Information Rate : 0.3466          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4215          
# 
# Mcnemar's Test P-Value : 0.000118        
# 
# Statistics by Class:
# 
#                      Class: Middle Class: Old Class: Young
# Sensitivity                 0.5606     0.5576       0.7204
# Specificity                 0.6881     0.8460       0.8904
# Pos Pred Value              0.4540     0.6576       0.7696
# Neg Pred Value              0.7720     0.7828       0.8623
# Precision                   0.4540     0.6576       0.7696
# Recall                      0.5606     0.5576       0.7204
# F1                          0.5017     0.6035       0.7442
# Prevalence                  0.3163     0.3466       0.3371
# Detection Rate              0.1773     0.1933       0.2428
# Detection Prevalence        0.3906     0.2939       0.3155
# Balanced Accuracy           0.6243     0.7018       0.8054

acctest_rpart<-cmtest$overall['Accuracy']
acctest_rpart
# Accuracy 
# 0.6325879

# After classifying the test data, the classifier threw an overall accuracy of 63.25% and a sensitivity over 55%
# for all the classes, the highes value was found again for "Young" with 72.04%


