#author: Cristiam Martin Jackson

#Clustering

#TASK
# In the analysis (clustering) the quality field should not be taken into consideration
# • Maximum number of groups 10 (the references grouping has 6 groups)
# • The reference grouping is defined by the quality attribute.
# • Minimum 10 tests are required.
# • The obtained result should be better than the result obtained by application k-means algorithm for 6 groups
# with default values of parameters and without preprocessing of data. 

# Seeds allows to create a starting point for randomly generated numbers
# so that each time the code is run, the same answer is generated.
set.seed(1234)
# Useful to plot clusters
library(cluster)


#calculation of accuracy
accuracyCalc <- function(confTbl, startCol)
{
  corr = 0;
  for(i in startCol:ncol(confTbl))
  {
    corr = corr + max(confTbl[,i])  
  }
  accuracy = corr/sum(confTbl)
  accuracy  
}

# Wine datasets
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv', 'wine_red.csv');
download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv', 'wine_white.csv');
wineRed_ds = read.table("wine_red.csv", header = TRUE, sep=";", na.strings= "*")
wineRed_dsC <- wineRed_ds[,-12]
wineWhite_ds = read.table("wine_white.csv", header = TRUE, sep=";", na.strings= "*")
wineWhite_dsC <- wineWhite_ds[,-12]

#+++++++++++++++++ CALCULATE Kmeans with default parameters, K=6, no preprocessing
#Some considerations: Algorith of Hartigan and Wong is used by default, this algorithm generally works
#better when trying several random starts (nstart>1)
#If an initial matrix of centres is supplied, it is possible that no point will be closest to one or more
#centres, which is currently an error for the Hartigan–Wong method.

# Lets try the data without pre-processing and without using the distance matrix
####################### Default values - Red wine
#distC = dist(wineRed_dsC)
#redwine.kmeans.default = kmeans(distC,6)
redwine.kmeans.default = kmeans(wineRed_dsC,6)
red_default = table(wineRed_ds$quality,redwine.kmeans.default$cluster )
red_default
defa_acc_red = accuracyCalc(red_default,1)
#plot the clusters
clusplot(wineRed_dsC, redwine.kmeans.default$cluster)
# > red_default
# 
#     1   2   3   4   5   6
# 3   0   0   1   3   0   6
# 4   1   4  11   8   6  23
# 5  59 113 147 110  91 161
# 6   6  38 188 137  88 181
# 7   2  11  54  38   7  87
# 8   0   2   4   2   1   9
# > accuracyCalc(red_default,1)
# [1] 0.4809256

######################### Default values - White wine
#distC_white = dist(wineWhite_dsC)
#whitewine.kmeans.default = kmeans(distC_white,6)
whitewine.kmeans.default = kmeans(wineWhite_dsC,6)
white_default = table(wineWhite_ds$quality,whitewine.kmeans.default$cluster )
white_default
defa_acc_white = accuracyCalc(white_default,1)
#plot the clusters
clusplot(wineWhite_dsC, whitewine.kmeans.default$cluster)
# > white_default
# 
#     1   2   3   4   5   6
# 3   3   3   4   6   2   2
# 4  28  34  45   8  21  27
# 5 356 188 113 159 343 298
# 6 325 524 182 145 492 530
# 7  56 263  74  17 190 280
# 8  14  51  15   6  35  54
# 9   0   2   0   0   0   3
# > accuracyCalc(white_default,1)
# [1] 0.457942

#############################################################################
#############################################################################
#############################################################################

# nstart, number of random sets of clusters, kmeans tries them and computes the best one
# this increases the chances to find a better initial K points

# It will plot the knee function for the 'betweenss' (between-cluster sum of squares)
# and for 'tot,withinss' (total within-cluster sum of squares)
# This plots can give an idea about the ideal number of clusters to choose
betr<-numeric()
witr<-numeric()
betw<-numeric()
witw<-numeric()

for (i in 6:10) {
  betr[i] <- kmeans(wineRed_dsC, i, nstart = 5)$betweenss
  witr[i] <- kmeans(wineRed_dsC, i, nstart = 5)$tot.withinss
  betw[i] <- kmeans(wineWhite_dsC, i, nstart = 5)$betweenss
  witw[i] <- kmeans(wineWhite_dsC, i, nstart = 5)$tot.withinss
}
# In some experiments, it was found that for the red wine clustering, the knee is around 8, meaning that it
# can be an optimal number of clusters for this clustering problem
pl1 <- plot(6:10, betr[6:10], type = "b", xlab = "K clusters", ylab = "Total between cluster", main = "Red Wine")
pl2 <- plot(6:10, witr[6:10], type = "b", xlab = "K clusters", ylab = "Total within cluster", main = "Red Wine")
# > witr
#[1]       NA       NA       NA       NA       NA 177373.6 177389.0 132079.6 139802.2 132505.2
# For the white wine, after K=6 it was not appreciate a big change with respect of the  within-cluster sum of squares
# it can be assumed that 6 might be a good number of clusters for this data set
pl3 <- plot(6:10, betw[6:10], type = "b", xlab = "K clusters", ylab = "Total between cluster", main = "White Wine")
pl4 <- plot(6:10, witw[6:10], type = "b", xlab = "K clusters", ylab = "Total within cluster", main = "White Wine")


#### Iterate 5 experiments per data set in order to find the best solution (measuring the accuracy using accuracyCalc())
best_acc_red = 0
best_acc_white = 0
best_K_red = 0
best_K_white = 0

for (i in 6:10) {
  # Apply K-means for different K (from 6 to 10)
  redwine.kmeans <- kmeans(wineRed_dsC, i)
  whitewine.kmeans <- kmeans(wineWhite_dsC, i)
  
  # Find accuracy and compare, select the best one with their respective K
  redwine = table(wineRed_ds$quality, redwine.kmeans$cluster)
  whitewine = table(wineWhite_ds$quality, whitewine.kmeans$cluster)
  acc_red = accuracyCalc(redwine,1)
  print(paste('red wine,      K=',i,', ','acc= ',acc_red))
  acc_white = accuracyCalc(whitewine, 1)
  print(paste('white wine,    K=',i,', ','acc= ',acc_white))
  if (acc_red > best_acc_red){
    best_acc_red = acc_red
    best_K_red = i
    #save the best solution found so far
    redwine.best = redwine.kmeans
  }
  if (acc_white > best_acc_white){
    best_acc_white = acc_white
    best_K_white = i
    #save the best solution found so far
    whitewine.best = whitewine.kmeans
  }
}

# [1] "red wine,      K= 6 ,  acc=  0.482176360225141"
# [1] "white wine,    K= 6 ,  acc=  0.456512862392813"
# [1] "red wine,      K= 7 ,  acc=  0.48780487804878"
# [1] "white wine,    K= 7 ,  acc=  0.454471212739894"
# [1] "red wine,      K= 8 ,  acc=  0.496560350218887"
# [1] "white wine,    K= 8 ,  acc=  0.453246222948142"
# [1] "red wine,      K= 9 ,  acc=  0.496560350218887"
# [1] "white wine,    K= 9 ,  acc=  0.450796243364639"
# [1] "red wine,      K= 10 ,  acc=  0.487179487179487"
# [1] "white wine,    K= 10 ,  acc=  0.451817068191098"

#Found parameters
# For this experiments, it was found that the number of clusters that best accuracy gave,
# was 8 for the red wine data set (0.4965)
# and 6 for the white wine data set (0.4565)
best_K_red
best_acc_red
best_K_white
best_acc_white

# The best clustering is shown below:
redwine.best
# Accuracy: 0.4965 (The accuracy with the default parameters (K=6) was: > "defa_acc_red": 0.4809256)

# > redwine.best
# K-means clustering with 8 clusters of sizes 247, 155, 140, 353, 150, 331, 155, 68
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar  chlorides free.sulfur.dioxide total.sulfur.dioxide
# 1      8.307287        0.5212753   0.2534413       2.326518 0.09004453           14.615385             38.39271
# 2      8.442581        0.5544194   0.2749677       2.591613 0.10346452           14.983871             55.10968
# 3      7.760000        0.5122143   0.2206429       2.336429 0.07595000           29.335714             50.46429
# 4      8.630595        0.5208640   0.2895751       2.473796 0.08673654            5.708215             14.36261
# 5      8.362000        0.5050333   0.3149333       2.769667 0.09404000           27.233333             72.89333
# 6      8.472205        0.5135196   0.2648036       2.300453 0.08055589           10.734139             25.90030
# 7      7.765161        0.5811935   0.2440000       3.037742 0.08687742           22.483871             97.80000
# 8      8.050000        0.5574265   0.3272059       3.457353 0.08961765           32.448529            139.35294
# density       pH sulphates   alcohol
# 1 0.9967556 3.337449 0.6435628 10.446559
# 2 0.9973109 3.285097 0.6935484 10.139247
# 3 0.9961135 3.373929 0.6861429 10.623214
# 4 0.9964959 3.296714 0.6321813 10.722380
# 5 0.9971721 3.312267 0.6607333 10.188000
# 6 0.9967356 3.307976 0.6697583 10.505337
# 7 0.9967529 3.309677 0.6311613 10.128710
# 8 0.9971351 3.236176 0.7069118  9.805882


whitewine.best
# Accuracy: 0.4565 (The accuracy with the default parameters (K=6) was: > "defa_acc_white": 0.457942)

# > whitewine.best
# K-means clustering with 6 clusters of sizes 833, 538, 1099, 379, 1298, 751
# 
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar  chlorides free.sulfur.dioxide total.sulfur.dioxide
# 1      7.016567        0.2919148   0.3502521       8.715366 0.05193517            42.72989            182.44778
# 2      6.827138        0.2818216   0.3128253       3.284108 0.03884572            17.10037             72.60409
# 3      6.874386        0.2815969   0.3331938       5.842630 0.04509463            29.76661            135.19927
# 4      7.012137        0.3075198   0.3568865      10.188522 0.05211346            56.14644            222.27045
# 5      6.741680        0.2674653   0.3182974       4.679160 0.04169954            27.81626            106.34784
# 6      6.782557        0.2594474   0.3491611       7.885952 0.04872969            50.66112            154.17643
# density       pH sulphates   alcohol
# 1 0.9959717 3.180444 0.5120648  9.807403
# 2 0.9917533 3.169591 0.4672305 11.256289
# 3 0.9937948 3.196242 0.4833030 10.614465
# 4 0.9968764 3.176781 0.5167282  9.521636
# 5 0.9926078 3.194083 0.4840447 11.037018
# 6 0.9948558 3.194394 0.4874434 10.217554


################################################################################################
################################################################################################
################################################################################################

#Do some pre-processing, apply normalization, the results can be improved, since Kmeans
# is based on means procedures

summary(wineRed_dsC)
summary(wineWhite_dsC)

wineRed_norm = scale(wineRed_dsC)
wineWhite_norm = scale(wineWhite_dsC)

summary(wineRed_norm)
summary(wineWhite_norm)



#### Iterate 5 experiments per data set in order to find the best solution (measuring the accuracy using accuracyCalc())
best_acc_red_norm = 0
best_acc_white_norm = 0
best_K_red_norm = 0
best_K_white_norm = 0

for (i in 6:10) {
  # Apply K-means for different K (from 6 to 10)
  redwine.kmeans.norm <- kmeans(wineRed_norm, i)
  whitewine.kmeans.norm <- kmeans(wineWhite_norm, i)
  
  # Find accuracy and compare, select the best one with their respective K
  redwinenorm = table(wineRed_ds$quality, redwine.kmeans.norm$cluster)
  whitewinenorm = table(wineWhite_ds$quality, whitewine.kmeans.norm$cluster)
  acc_red_norm = accuracyCalc(redwinenorm,1)
  print(paste('red wine,      K=',i,', ','acc= ',acc_red_norm))
  acc_white_norm = accuracyCalc(whitewinenorm, 1)
  print(paste('white wine,    K=',i,', ','acc= ',acc_white_norm))
  if (acc_red_norm > best_acc_red_norm){
    best_acc_red_norm = acc_red_norm
    best_K_red_norm = i
    #save the best solution found so far
    redwine.best.norm = redwine.kmeans.norm
  }
  if (acc_white_norm > best_acc_white_norm){
    best_acc_white_norm = acc_white_norm
    best_K_white_norm = i
    #save the best solution found so far
    whitewine.best.norm = whitewine.kmeans.norm
  }
}
# [1] "red wine,      K= 6 ,  acc=  0.549718574108818"
# [1] "white wine,    K= 6 ,  acc=  0.450796243364639"
# [1] "red wine,      K= 7 ,  acc=  0.557223264540338"
# [1] "white wine,    K= 7 ,  acc=  0.474479379338506"
# [1] "red wine,      K= 8 ,  acc=  0.563477173233271"
# [1] "white wine,    K= 8 ,  acc=  0.47284605961617"
# [1] "red wine,      K= 9 ,  acc=  0.543464665415885"
# [1] "white wine,    K= 9 ,  acc=  0.475704369130257"
# [1] "red wine,      K= 10 ,  acc=  0.544715447154472"
# [1] "white wine,    K= 10 ,  acc=  0.478562678644345"

# for K=8, accuracy was found = 0.5634772 for the red wine data set
best_K_red_norm
best_acc_red_norm
# for K=10, accuracy was found = 0.47.85627 for the white wine data set
best_K_white_norm
best_acc_white_norm

# Best clustering obtained for the red wine data set
redwine.best.norm

# Accuracy with preprocessed data: 0.5634
# Accuracy without data preprocessing: 0.4965 (The accuracy with the default parameters (K=6) was: > "defa_acc_red": 0.4809256)

# > redwine.best.norm
# K-means clustering with 8 clusters of sizes 28, 217, 484, 45, 190, 213, 175, 247
# 
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar   chlorides free.sulfur.dioxide
# 1    0.09538646      0.002199115  1.18118314    -0.38975023  5.78295058         -0.04950011
# 2    1.78032515     -0.528671882  1.23860907     0.15861582  0.02386727         -0.46957117
# 3   -0.42756432      0.634511478 -0.79326708    -0.20725497 -0.04173936         -0.49695898
# 4   -0.21293962     -0.079045401  0.41821540     4.28632984  0.19972516          1.58830954
# 5   -0.32958992     -0.632423504  0.15845131    -0.23320729 -0.23110588          1.24546025
# 6    0.02593278      0.443611859 -0.03224198    -0.03168534  0.03497554          0.76644136
# 7   -1.13667798      0.487671666 -0.94662785    -0.23016762 -0.41530683          0.11247506
# 8    0.33821492     -1.006306442  0.83276090    -0.10017153 -0.18926548         -0.59609485
# total.sulfur.dioxide      density         pH  sulphates    alcohol
# 1            0.5101700  0.180015517 -1.7352487  3.6622665 -0.8694593
# 2           -0.4052109  1.279181397 -1.0662560  0.2694641 -0.1259273
# 3           -0.4355420  0.004890452  0.3344512 -0.3939968 -0.5102736
# 4            1.7604321  1.034138800 -0.1741796  0.1013792 -0.3009953
# 5            0.5497693 -0.180089856  0.3493793  0.4499650  0.0567960
# 6            1.4713566  0.288499305 -0.4165241 -0.4710071 -0.7596408
# 7           -0.2110354 -1.390045026  1.2652919 -0.2264061  1.2767027
# 8           -0.7113159 -0.467617094 -0.2961955  0.3221354  0.9707593

#best clustering obtained for the white wine data set
whitewine.best.norm

# Accuracy with preprocessed data: 0.4785
# Accuracy without data preprocessing: 0.4565 (The accuracy with the default parameters (K=6) was: > "defa_acc_white": 0.457942)
# > whitewine.best.norm
# K-means clustering with 10 clusters of sizes 397, 461, 687, 94, 759, 168, 376, 683, 710, 563
# 
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar   chlorides free.sulfur.dioxide
# 1     0.07357654     1.1926216712 -0.71441621    -0.09932013  0.25852742          -0.6609775
# 2    -0.09975526    -0.3449631308  0.08384486    -0.67448451 -0.32715295          -0.2429792
# 3     1.23635422    -0.3479146689  0.41905438    -0.48579429 -0.23109338          -0.5241959
# 4    -0.17838371     0.3266951879  1.04220663    -0.44327763  5.65873672           0.2793195
# 5    -0.39950889     0.1186879393 -0.12227393    -0.55048143 -0.51169384          -0.3819474
# 6    -1.23795231     2.1174314601 -1.38919476    -0.55932931 -0.40288039          -0.5725041
# 7     0.56225325     0.2396210765  1.67534549     1.26240676  0.08930113           0.8608182
# 8    -0.17353673     0.0008180368 -0.21175338     0.32909721  0.10041982           1.1654377
# 9    -0.64274644    -0.6096796641 -0.23143657    -0.52574837 -0.11973736          -0.1871410
# 10    0.10451436    -0.3725241278 -0.24099249     1.61883120  0.20236434           0.1910752
# total.sulfur.dioxide     density          pH   sulphates     alcohol
# 1           -0.11535159  0.22404730 -0.29862687 -0.33656804 -0.61586347
# 2           -0.38247311 -0.65964623  0.12223708  1.69696218  0.61510018
# 3           -0.50201007 -0.30301337 -0.70957218 -0.44174540  0.24496054
# 4            0.09990397  0.08236529 -0.65570150 -0.21585043 -0.80776667
# 5           -0.73083198 -1.16691585 -0.04008726 -0.56410677  1.37216100
# 6           -0.75459885 -0.85560775  0.99145004  0.04828238  0.87702462
# 7            0.98189032  1.34520007 -0.39150641  0.57554925 -0.90429872
# 8            1.21788927  0.51781493  0.08487924  0.05727648 -0.61406213
# 9           -0.22219957 -0.26742904  1.17311992 -0.03309026 -0.03372046
# 10           0.34783529  1.37730196 -0.47691479 -0.24316108 -0.95358660



# Experiments changing nstart, so that more random clusters centers are evaluated
redwine.kmeans.norm8 <- kmeans(wineRed_norm, 8,nstart = 50)
whitewine.kmeans.norm10 <- kmeans(wineWhite_norm, 10,nstart = 50)
# Find accuracy
redwinenorm8 = table(wineRed_ds$quality, redwine.kmeans.norm8$cluster)
whitewinenorm10 = table(wineWhite_ds$quality, whitewine.kmeans.norm10$cluster)
acc_red_norm8 = accuracyCalc(redwinenorm8,1)
acc_white_norm10 = accuracyCalc(whitewinenorm10, 1)
acc_red_norm8
acc_white_norm10

# Accuracy for the white data set improved to (0.4869334)

whitewine.kmeans.norm10
# > whitewine.kmeans.norm10
# K-means clustering with 10 clusters of sizes 434, 574, 383, 407, 280, 731, 697, 100, 734, 558
# 
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar   chlorides free.sulfur.dioxide
# 1    -0.01345529      -0.39688448  0.13081772     -0.6743026 -0.26642034         -0.17513775
# 2     1.38001441      -0.33454401  0.51441569     -0.5498020 -0.16588602         -0.58402829
# 3    -0.12494890       1.61468106 -0.99665079     -0.2009998  0.22337937         -0.56680189
# 4    -1.12474499       0.69840330 -0.44636923     -0.5967831 -0.55648678         -0.43563016
# 5     0.66089979       0.35565581  2.02937205      1.2993837  0.11193266          0.73304018
# 6    -0.61950042      -0.61460039 -0.27913704     -0.5427862 -0.09671313         -0.11749393
# 7     0.01175327      -0.01208561 -0.08845773     -0.4456199 -0.47880982         -0.36706972
# 8    -0.18224133       0.30764443  0.94289107     -0.4173680  5.51619461          0.32615219
# 9    -0.04990989       0.01400343 -0.06559340      0.6310155  0.09491468          1.17035858
# 10    0.06058566      -0.39664318 -0.24609943      1.5237008  0.18562829          0.09042046
# total.sulfur.dioxide     density          pH   sulphates     alcohol
# 1            -0.2611409 -0.56086389  0.09876175  1.72027868  0.46910025
# 2            -0.4485557 -0.20665081 -0.67188338 -0.38257952  0.07391347
# 3            -0.1251559  0.13633561 -0.07514923 -0.30682439 -0.58151535
# 4            -0.7442332 -1.21188967  0.85336527 -0.01265207  1.44442146
# 5             0.9362204  1.36545371 -0.70256720  0.23385448 -0.97268417
# 6            -0.1698682 -0.25723761  1.08946838 -0.03905338 -0.08333027
# 7            -0.6941114 -0.99662741 -0.47252321 -0.65023337  1.18849868
# 8             0.1329318  0.08737267 -0.60772368 -0.21946719 -0.77624824
# 9             1.3169466  0.79349533  0.10251585  0.30324567 -0.72844739
# 10            0.1568806  1.27644296 -0.46693048 -0.33815653 -0.88528202


##############################################
##############################################
##############################################
#
#CONCLUSIONS
#
#The best clustering was obteined for both cases when normalization of data was applied
#For the red wine data set, it can be said, that the wines with properties close to the cluster number 8
#tend to be from medium to high quality (from 6 to 8), the wines contained by cluster number 3 tend to
#have quality mainly between 5 and 6. It can be assumed based on the table, the size of the clusters and
#the means of the clusters that are shown below.


#In summary the best found clustering were: 

# ****** FOR RED WINE DATA SET

# Best clustering obtained for the red wine data set, with normalized data and 8 clusters
# Accuracy with preprocessed data: 0.5634
# Accuracy without data preprocessing: 0.4965 (The accuracy with the default parameters (K=6) was: > "defa_acc_red": 0.4809256)

# > redwine.best.norm
# K-means clustering with 8 clusters of sizes 28, 217, 484, 45, 190, 213, 175, 247
# 
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar   chlorides free.sulfur.dioxide
# 1    0.09538646      0.002199115  1.18118314    -0.38975023  5.78295058         -0.04950011
# 2    1.78032515     -0.528671882  1.23860907     0.15861582  0.02386727         -0.46957117
# 3   -0.42756432      0.634511478 -0.79326708    -0.20725497 -0.04173936         -0.49695898
# 4   -0.21293962     -0.079045401  0.41821540     4.28632984  0.19972516          1.58830954
# 5   -0.32958992     -0.632423504  0.15845131    -0.23320729 -0.23110588          1.24546025
# 6    0.02593278      0.443611859 -0.03224198    -0.03168534  0.03497554          0.76644136
# 7   -1.13667798      0.487671666 -0.94662785    -0.23016762 -0.41530683          0.11247506
# 8    0.33821492     -1.006306442  0.83276090    -0.10017153 -0.18926548         -0.59609485
# total.sulfur.dioxide      density         pH  sulphates    alcohol
# 1            0.5101700  0.180015517 -1.7352487  3.6622665 -0.8694593
# 2           -0.4052109  1.279181397 -1.0662560  0.2694641 -0.1259273
# 3           -0.4355420  0.004890452  0.3344512 -0.3939968 -0.5102736
# 4            1.7604321  1.034138800 -0.1741796  0.1013792 -0.3009953
# 5            0.5497693 -0.180089856  0.3493793  0.4499650  0.0567960
# 6            1.4713566  0.288499305 -0.4165241 -0.4710071 -0.7596408
# 7           -0.2110354 -1.390045026  1.2652919 -0.2264061  1.2767027
# 8           -0.7113159 -0.467617094 -0.2961955  0.3221354  0.9707593

redwinenorm = table(wineRed_ds$quality, redwine.best.norm$cluster)
redwinenorm
#     1   2   3   4   5   6   7   8
# 3   0   2   7   0   1   0   0   0
# 4   1   3  28   1   3   4  10   3
# 5  17  71 273  27  60 167  35  31
# 6   9 100 158  12 102  42 103 112
# 7   1  37  18   5  23   0  23  92
# 8   0   4   0   0   1   0   4   9

###############################################################################################
# ******** FOR WHTE WINE DATA SET

#CONCLUSIONS
#
#White wines contained in the cluster number 2, 7 and 9 are most likely to have quality between 5 and 6
#However, the clustering in this case is not reliable, since there is not a dominant cluster for
#wines with higher or lower quality

# Accuracy for the white data set, with normalized data and 10 clusters = 0.4869334
# Accuracy without data preprocessing: 0.4565 (The accuracy with the default parameters (K=6) was: > "defa_acc_white": 0.457942)
# > whitewine.kmeans.norm10
# K-means clustering with 10 clusters of sizes 434, 574, 383, 407, 280, 731, 697, 100, 734, 558
# 
# Cluster means:
#   fixed.acidity volatile.acidity citric.acid residual.sugar   chlorides free.sulfur.dioxide
# 1    -0.01345529      -0.39688448  0.13081772     -0.6743026 -0.26642034         -0.17513775
# 2     1.38001441      -0.33454401  0.51441569     -0.5498020 -0.16588602         -0.58402829
# 3    -0.12494890       1.61468106 -0.99665079     -0.2009998  0.22337937         -0.56680189
# 4    -1.12474499       0.69840330 -0.44636923     -0.5967831 -0.55648678         -0.43563016
# 5     0.66089979       0.35565581  2.02937205      1.2993837  0.11193266          0.73304018
# 6    -0.61950042      -0.61460039 -0.27913704     -0.5427862 -0.09671313         -0.11749393
# 7     0.01175327      -0.01208561 -0.08845773     -0.4456199 -0.47880982         -0.36706972
# 8    -0.18224133       0.30764443  0.94289107     -0.4173680  5.51619461          0.32615219
# 9    -0.04990989       0.01400343 -0.06559340      0.6310155  0.09491468          1.17035858
# 10    0.06058566      -0.39664318 -0.24609943      1.5237008  0.18562829          0.09042046
# total.sulfur.dioxide     density          pH   sulphates     alcohol
# 1            -0.2611409 -0.56086389  0.09876175  1.72027868  0.46910025
# 2            -0.4485557 -0.20665081 -0.67188338 -0.38257952  0.07391347
# 3            -0.1251559  0.13633561 -0.07514923 -0.30682439 -0.58151535
# 4            -0.7442332 -1.21188967  0.85336527 -0.01265207  1.44442146
# 5             0.9362204  1.36545371 -0.70256720  0.23385448 -0.97268417
# 6            -0.1698682 -0.25723761  1.08946838 -0.03905338 -0.08333027
# 7            -0.6941114 -0.99662741 -0.47252321 -0.65023337  1.18849868
# 8             0.1329318  0.08737267 -0.60772368 -0.21946719 -0.77624824
# 9             1.3169466  0.79349533  0.10251585  0.30324567 -0.72844739
# 10            0.1568806  1.27644296 -0.46693048 -0.33815653 -0.88528202

whitewinenorm = table(wineWhite_ds$quality, whitewine.kmeans.norm10$cluster)
whitewinenorm
#     1   2   3   4   5   6   7   8   9  10
# 3   0   3   2   2   1   2   2   1   5   2
# 4   7  33  49   9  10  17  14   3  12   9
# 5  64 220 223  23 149 158  50  46 333 191
# 6 216 243  96 161 103 394 344  46 329 266
# 7 123  65  10 171  14 142 232   2  43  78
# 8  24   9   3  40   3  18  52   2  12  12
# 9   0   1   0   1   0   0   3   0   0   0
