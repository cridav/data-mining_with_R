#Autor: Cristiam Martin Jackson
#Indeks: 302134
#Script lab 1 EDAMI "Discovering association rules"

#ORDER OF THE SCRIPT:
#Discover frequent items
#Discover association rules
#     modify parameters to find more association rules
#Conclusions with the summary of the more important rules found

#DATA SET:
#Market dataset (data.frame)


#Main task: Discover the best association rules

#Load library
#Association rules
library(arules)
#Visualization of rules
library(arulesViz)


download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/supermarket.csv','supermarket.csv')
marketSet = read.csv('supermarket.csv',sep=';')
marketSet= as.data.frame(sapply(marketSet, function(x) as.logical(x)))

#Statistics about the data set
summary(marketSet)
#Some records inside the dataset, show x number of rows
head(marketSet,1)

###########
#Convert to "Transactions" datatype
marketSetTR <- as(marketSet, "transactions")
#Information about the new structure
str(marketSetTR)
#Statistics
#It is possible to see the most frequent items in the data set
summary(marketSetTR)

###########
#Frequency of items
#?itemFrequency
freqItems = itemFrequency(marketSetTR, type = "relative") 
str(freqItems)
summary(freqItems)
print(freqItems)
#There are items with extremely low frequency <<1%

#Sort items according to their relative support in order to find the most frequent items
freqItems = sort(freqItems, decreasing = TRUE)
print(freqItems)
# bread and cake, fruit and vegetables are the most common items
# show only items with support > 30%
print(freqItems [freqItems > 0.3])
length(freqItems [freqItems > 0.3])

# show only items with support > 10%
print(freqItems [freqItems > 0.1])
length(freqItems [freqItems > 0.1])

# show only items with support > 5%
print(freqItems [freqItems >= 0.05])
length(freqItems [freqItems >= 0.05])
# Length of the whole set of frequent items
length(freqItems)
#Percentage [%] of the items with support >= 5%
100*length(freqItems [freqItems >= 0.05])/length(freqItems)

# around 50% of the dataset contains data with worthy support
itemFrequencyPlot(marketSetTR, type = "relative", support = 0.3)

#################
#Discovering frequent itemsets, using Apriori algorithm
# Support=0.3 is used, because the set of frequent items is merely 23 items long, something interesting can be found
apParam = new ("APparameter", "confidence"=0.6, "support"=0.3, "minlen"=1)
print(apParam)
apParam@target = "frequent itemsets"
asets<-apriori(marketSetTR,apParam)
length(asets)
summary(asets)
inspect(sort(asets, by="support"))
size(asets)
str(asets)
plot(asets[size(asets)==1], method = "graph")
plot(asets[size(asets)==2], method = "graph")
plot(asets[size(asets)>=3], method = "graph")


#Discovering frequent itemsets, using ECLAT algorithm
ecParam = new("ECparameter", "confidence"=0.6, "support"=0.3)
print(ecParam)
fsets <- eclat(marketSetTR, ecParam)
length(fsets)

# Check if there are different rules found by the algorithms ECLAT and Apriori
inspect(fsets[which((fsets %in% asets)==FALSE)])
length(fsets[which((fsets %in% asets)==FALSE)])

###########################################################################################################
#################################### Discovering Association rules
############## Discovering number of rules for different support values
#Set a minimum value and check the summary in order to find more appropriate values, minLen = 2 to avoid unuseful antecedents (empty)
aParam = new("APparameter", "confidence"=0.6, "support"=0.1, "minlen"=2, target = "rules")
print(aParam)
aRules1 <- apriori(marketSetTR,aParam)
summary(aRules1)
#Max support available = 0.5051, max confidence = 0.92, lift isalways < 1 for two rules (shown ahead)
length(aRules1)
#Some items with support ~ 0.1 and confidence > 0.9, rules with 5 antecedents were found
#Association rules with high confidence
inspect(head(sort(aRules1, by="confidence"),5))
#Association rules with negative dependence (lift < 1)
inspect(head(sort(aRules1, by="lift",decreasing = FALSE),2))

#Support = 0.2
aParam@support = 0.2
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Length for support = 0.2
length(aRules)

#Support = 0.3
aParam@support = 0.3
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Length for support = 0.3
length(aRules)

#Support = 0.4
aParam@support = 0.4
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Length for support = 0.4
length(aRules)

#Support = 0.5
aParam@support = 0.5
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Length for support = 0.5
length(aRules)
#Four items with the highest support (0.5) found in the dataset for conf=0.6
inspect(head(sort(aRules, by="confidence"),4))

#############################

#create parameters for apriori algorithm
aParam = new("APparameter", "confidence"=0.6, "support"=0.3, "minlen"=2, target = "rules")
print(aParam)
#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Discover size of rules, for taking into account antecedent's length
size(aRules)
#Summary of rules
summary(aRules)

# Maximum support found is 0.5051; min lift is 1.041
# Since lift > 1, precedent and consequent are dependent positively
# Number of rules for support = 0.3
length(aRules)
str(aRules)
inspect(sort(aRules, by="support"))

#Association rules for the highest lifts
#Filtering rules - lift parameter > 1.1
rulesLift1.1 <- subset(aRules, subset = lift > 1.1)
inspect(sort(rulesLift1.1, by="lift"))


#Represent association rules
#plot(rulesLift1.1, shading="order", control=list(main = "Two-key plot" ))
#plot(rulesLift1.1, measure = "lift",method = "scatterplot")

############## Look for interesting rules with support = 0.2
#Changing support to 0.2
print(aParam)
aParam@support = 0.2
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
#Number of rules found for support = 0.2
length(aRules)
str(aRules)
inspect(head(sort(aRules, by="support"),20))

#Find interesting rules
#rules with improvement idicator greater than 0.01

aParam@support = 0.3
aRules <- apriori(marketSetTR,aParam)
resTbl <- interestMeasure(aRules,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)
intersRule <- aRules[intres] 
length(intersRule)
inspect(intersRule)

# Association rules show bread and cake as the most repeated consequent, check for another rules with different items
#Check antencedents for the different consequents found as frequent items according to the interesting rules
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "milk.cream")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "fruit")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "bread.and.cake")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "baking.needs")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "vegetables")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "biscuits")
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "frozen.foods")
inspect(head(sort(rulesInGivenConseq, by="support"),10))



#Changing support to 0.3 and confidence to 0.8
#Finding more selective association rules
print(aParam)
aParam@support = 0.3
aParam@confidence = 0.8
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
#Number of rules
length(aRules)
str(aRules)
#Bread and cake found as the only consequent for support=0.3 and confidence 0.8
inspect(head(sort(aRules, by="support"),20))

#Look for different items by decreasing confidence
#Changing support to 0.3 and confidence to 0.7
print(aParam)
aParam@support = 0.3
aParam@confidence = 0.7
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
#Number of rules
length(aRules)
str(aRules)
inspect(head(sort(aRules, by="support"),66))


# Set the scope on each of the elements previously found as a consequent
#Check different consequents included as frequent items for the previous case
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "milk.cream")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "fruit")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),11))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "bread.and.cake")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),31))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "baking.needs")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),2))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "vegetables")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),12))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "biscuits")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),1))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "frozen.foods")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),1))




#################################### Check rules with longer antecedents
#Focus on the average confidence
#Changing min length of antecedent to 2L
#Changing support to 0.2
print(aParam)
aParam@target = "rules"
aParam@minlen = 2L
aParam@support = 0.2
aParam@confidence = 0.8
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
length(aRules)
#str(aRules)
#inspect(head(sort(aRules, by="support"),20))
#inspect(head(aRules,186))

#Changing min length of antecedent to 4L


aParam@minlen = 4L
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
length(aRules)
#str(aRules)
#inspect(head(sort(aRules, by="support"),20))
#inspect(head(aRules,186))

#Changing min length of antecedent to 5L
#Changing support to 0.2
aParam@minlen = 5L
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
length(aRules)
#str(aRules)
#inspect(head(sort(aRules, by="support"),20))
#inspect(head(aRules,186))


############ Check summary for different size of antecedents, with confidence = 0.6 and support = 0.2
aParam@confidence = 0.6
aParam@support = 0.2
#Minimum length = 2L
aParam@minlen = 2L
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Minimum length = 3L
aParam@minlen = 3L
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Minimum length = 4L
aParam@minlen = 4L
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Minimum length = 5L
aParam@minlen = 5L
aRules <- apriori(marketSetTR,aParam)
summary(aRules)

################################################# CONCLUSIONS #################################
### The most interesting experiments are put togheter (Those experiments where already shown)
# the rules were chosen based on the highest support and confidence
#additionally particular cases where lift < 1 and "Longer" antecedents are shown as well
#No experiment had confidence below 0.6!
#Sort items according to their relative support in order to find the most frequent items
#The most frequent items are shown ahead
freqItems = sort(freqItems, decreasing = TRUE)
#print(freqItems)
# bread and cake, fruit and vegetables are the most common items
# show only items with support > 30%
print(freqItems [freqItems > 0.3])
length(freqItems [freqItems > 0.3])

#################################Finding important associating rules
aParam = new("APparameter", "confidence"=0.6, "support"=0.1, "minlen"=2, target = "rules")
print(aParam)
aRules <- apriori(marketSetTR,aParam)
summary(aRules)
#Max support available = 0.5051, max confidence = 0.92
length(aRules)
#Some items with support ~ 0.1 and confidence > 0.9, rules with 5 antecedents were found
#
#Association rules with small support, but the highest confidence, found for the longest rules (with 5 items as antecedent)
#If presented the case were the supermarkets are selling that combination of items shown in the antecedent, is likely that bread and cake is sold too
inspect(head(sort(aRules, by="confidence"),5))
#Association rules with negative dependence (lift < 1), small support factor
inspect(head(sort(aRules, by="lift",decreasing = FALSE),2))

#Association rules with support > 0.5, This rules have confidence > 0.6, it was found to be the rules with the highest support
#Support = 0.5
aParam@support = 0.5
aRules <- apriori(marketSetTR,aParam)
#summary(aRules)
#Length for support = 0.5
length(aRules)
#Four items with the highest support (0.5) found in the dataset for conf=0.6
#the supermarket can assume with high confidence, that if they sell milk cream, they will sell bread and cake
#as well as selling fruits will lead to sell bread and cake and viceversa.
inspect(head(sort(aRules, by="confidence"),4))


#Interesting rules found, for support = 0.3
#rules with improvement idicator greater than 0.01
aParam@support = 0.3
aRules <- apriori(marketSetTR,aParam)
resTbl <- interestMeasure(aRules,"improvement", asets)
intres <- which(sapply(resTbl, function(x) {x > 0.01  && x <= 1 })==TRUE)
intersRule <- aRules[intres] 
length(intersRule)
inspect(intersRule)


#The next association rules show a good values of confidence (0.8 and 0.7) with a support > 0.3
#Changing support to 0.3 and confidence to 0.8, the Association rules for a high confidence and support > 0.3
#threw bread and cake as the dominant consequent
#Finding more selective association rules
print(aParam)
aParam@support = 0.3
aParam@confidence = 0.8
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
#Number of rules
length(aRules)
str(aRules)
#Bread and cake found as the only consequent for support=0.3 and confidence 0.8
inspect(head(sort(aRules, by="support"),20))

#Increasing the scope, checking for more ordinary rules that fulfil the conditions for support >0.3 and confidence 0.7
#Look for different items by decreasing confidence
#Changing support to 0.3 and confidence to 0.7
print(aParam)
aParam@support = 0.3
aParam@confidence = 0.7
print(aParam)

#Association rules using Apriori algorithm
aRules <- apriori(marketSetTR,aParam)
#Summary of rules
summary(aRules)
#Number of rules
length(aRules)
str(aRules)
inspect(head(sort(aRules, by="support"),66))


# Set the scope on each of the elements previously found as a consequent
#Filtrate using consequents included as frequent items for the previous case
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "milk.cream")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),10))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "fruit")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),11))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "bread.and.cake")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),31))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "baking.needs")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),2))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "vegetables")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),12))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "biscuits")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),1))
rulesInGivenConseq <- subset(aRules, subset = rhs %in% "frozen.foods")
length(rulesInGivenConseq)
inspect(head(sort(rulesInGivenConseq, by="support"),1))

#The previous associating rules can be used for a chain of supermarkets in order to determine the likeability of different
#items to be purchased based on the adquisition of certain items, parameters like support and confidence are useful to determine
#which products are can be bought


