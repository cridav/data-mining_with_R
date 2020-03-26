#Author: Cristiam Martin Jackson
#SEQUENTIAL RULES DISCOVERY

#Objective:
#To discover the best sequential rules involving the unusual meal ingestion or unusual exercise activity events

#The given data set contains diabetes records from different patients, taken at different time and under certain circumstances


setwd('/home/cristiam/Documents/ISI - WEiTI/II/EDAMI/LAB2CDMJ');

library(arules)
library(arulesSequences)

download.file('http://staff.ii.pw.edu.pl/~gprotazi/dydaktyka/dane/diab_trans.data','diab_trans.data')
#reading data - into dataframe
diab.df <- read.csv("diab_trans.data", header=TRUE, stringsAsFactors = FALSE)
#View(diab.df)

#The next codes are being used in order to process the data before starting the process of data mining
#source: https://archive.ics.uci.edu/ml/datasets/diabetes

#-=-=-=-=-=-=-= CODES RELATED TO GLUCOSE MEASUREMENT
#48 = Unspecified blood glucose measurement 
#57 = Unspecified blood glucose measurement 
#58 = Pre-breakfast blood glucose measurement 
#59 = Post-breakfast blood glucose measurement 
#60 = Pre-lunch blood glucose measurement 
#61 = Post-lunch blood glucose measurement 
#62 = Pre-supper blood glucose measurement 
#63 = Post-supper blood glucose measurement 
#64 = Pre-snack blood glucose measurement 
#-=-=-=-=-=-=-= RELATED TO UNUSUAL MEAL INGESTION AND EXERCISE ACTIVITY

#It is understood as unusual, the amount of meal ingestion or physical activity that is not normal, it is: it can be either more or less

#67 = More-than-usual meal ingestion 
#68 = Less-than-usual meal ingestion 
#70 = More-than-usual exercise activity 
#71 = Less-than-usual exercise activity 

# Goals for Blood Glucose Control
#source: https://www.joslin.org/info/goals_for_blood_glucose_control.html
#Based on the values provided, three events are created:

#(1)-> People with diabetes
#(2)-> People without diabetes
#(3)-> Uncertain

#The data is going to be discretized based on the following values

#             No Diab.  Diab                Threshold level
#fasting        <100    70-130      (2)--/70/--(3)--/100/--(1)--/130/    
#before meals   <110    70-130      (2)--/70/--(3)--/110/--(1)--/130/ 
#after meals    <140    <180            (3)--/140/--(1)--/180/ 
#Others         <120    90-150      (2)--/90/--(3)--/120/--(1)--/150/ 

############### DISCRETIZATION OF VALUES ----- DATA PREPROCESSING
#create column for discretization
diab.df$discr = 5
#Use number 5 as NaN
#diab.df[diab.df$discr==0]<-5
#Unusual meal ingestion and exercise activity events
##
##THE IDs OF THE EVENTS OF INTEREST WILL BE CHANGED FOR THE NEXT IDs
#67 = More-than-usual meal ingestion      ->  10
#68 = Less-than-usual meal ingestion      ->  11
#70 = More-than-usual exercise activity   ->  12
#71 = Less-than-usual exercise activity   ->  13
##

diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_67",10,discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_68",11,discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_70",12,discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_71",13,discr))

# Unespecified values
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_48" | diab.df$code=="id_57",ifelse(diab.df$value<150,1,5),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_48" | diab.df$code=="id_57",ifelse(diab.df$value<120,3,discr),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_48" | diab.df$code=="id_57",ifelse(diab.df$value<90,2,discr),discr))
#After meals
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_59" | diab.df$code=="id_61" | diab.df$code=="id_63",ifelse(diab.df$value<180,1,5),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_59" | diab.df$code=="id_61" | diab.df$code=="id_63",ifelse(diab.df$value<140,3,discr),discr))
#Before meals
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_60" | diab.df$code=="id_62" | diab.df$code=="id_64",ifelse(diab.df$value<130,1,5),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_60" | diab.df$code=="id_62" | diab.df$code=="id_64",ifelse(diab.df$value<110,3,discr),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_60" | diab.df$code=="id_62" | diab.df$code=="id_64",ifelse(diab.df$value<70,2,discr),discr))
#Fasting
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_58",ifelse(diab.df$value<130,1,5),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_58",ifelse(diab.df$value<100,3,discr),discr))
diab.df <- transform(diab.df, discr = ifelse(diab.df$code=="id_58",ifelse(diab.df$value<70,2,discr),discr))

View(diab.df)

#Data is now discretized, the column discr contains the information whether if the patient has diabetes or not, and when was taken the measurement
#Data with value 5 does no belong to any range, it will be deleted as well
#diab_proc.df <- subset(diab.df, discr!=0)
diab_proc.df <- subset(diab.df, discr!=5)
diab_proc.df$value <- NULL
diab_proc.df$code <- NULL

#-=-=-=-=-=-= Prepare data
#saving data into a file  - removing the header line
write.table(diab_proc.df, "diab_trans2.data", sep = "," , row.names = FALSE, col.names = FALSE )

#reading data in transactional form
diabSeq <- read_baskets(con = "diab_trans2.data", sep =",", info = c("sequenceID","eventID"))
View(as(diabSeq,"data.frame"))

summary(diabSeq)

#setting parameters
#time(eventid) in the diab_trans.data set is given as a number of seconds from some date.
#The minimum gap is set to be between 10 (600) minutes and 48 hours (172800)
#The maximum size and length depends on the number of events presented, in this case the maximum allowed is 2
#it can be either unusual ingestion or unusual activity, or both, provided that make sense, it means:
#ingest more than usual and less than usual cannot belong to the same pattern, they are opposite

seqParam = new ("SPparameter",support = 0.05, maxsize = 3, mingap=600, maxgap =672800, maxlen = 3 )
patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE))
#patSeq= cspade(diabSeq,seqParam, control = list(verbose = TRUE, tidLists = FALSE, summary= TRUE), tmpdir ="/home/users/cmartin2/Documents/CD/Lab2/temp")

#discovery sequential rules
seqRules = ruleInduction(patSeq,confidence = 0.6)

length(seqRules)
#summary of set of rules
summary(seqRules)
#view of rules
inspect(head(seqRules,10))
str(seqRules)
####################################################

#size(lhs(seqRules))
#inspect(head(lhs(seqRules),10))
#inspect(head(rhs(seqRules),10))

#Check the presence of the events (10, 11, 12, 13)
seqRules@elements@items@itemInfo

#We are interested in the impact of the events "unusual ingestion" and "unusual activity" have in the measurement of the
#levels of glucose in different persons, there are some rules to consider:
#the events 10 and 11 as well as 12 and 13 cannot occur at the same time, because either the ingestion is more, or less
#(not in the same sequence) as well as the exercise activity, given this, we proceed to check the existence of the rules

#Check the impact of the events in the measurements (check the consequents of interest: 1-diabetes, 2-no diabetes,
#3-uncertain)

rulesrI <- subset(seqRules, rhs(x) %ain% "1" & lift >= 1)
rulesrII <- subset(seqRules, rhs(x) %ain% "2" & lift >= 1)
rulesrIII <- subset(seqRules, rhs(x) %ain% "3" & lift >= 1)
length(rulesrI)
length(rulesrII)
length(rulesrIII)
#Find the best rules, look for the larger degree of dependence
inspect(head(sort(rulesrI, by="lift"),14))
inspect(head(sort(rulesrII, by="lift"),16))
inspect(head(sort(rulesrIII, by="lift"),10))
#Some antecedents do not include any of the event of interest (10, 11, 12, 13)
#Create the subset with valid rules, focused on the events (10, 11, 12, 13)
diabectic <- subset(rulesrI, lhs(x) %in% c("10","11","12","13"))
no_diabetic <- subset(rulesrII, lhs(x) %in% c("10","11","12","13"))
uncertain <- subset(rulesrIII, lhs(x) %in% c("10","11","12","13"))
inspect(diabectic)
inspect(no_diabetic)
inspect(uncertain)

View(as(diabectic,"data.frame"))
View(as(no_diabetic,"data.frame"))
View(as(uncertain,"data.frame"))
###### IDs -References for the conclusions

#67 = More-than-usual meal ingestion      ->  10
#68 = Less-than-usual meal ingestion      ->  11
#70 = More-than-usual exercise activity   ->  12
#71 = Less-than-usual exercise activity   ->  13

#(1)-> People with diabetes
#(2)-> People without diabetes
#(3)-> Uncertain

#-=-=-=-=-=-= RESULTS and CONCLUSIONS

##  It was found that the measurement of glucose lead to the levels presents in people with diabetes, when:
# Ingestion is more than usual (with the precedent of no diabetes of uncertain level)
# Activity is more than usual
# More activity and more ingestion

# > inspect(diabectic)
# lhs       rhs       support confidence lift 
# 1 <{10},              
# {3}>  => <{1}>  0.50000000          1    1 
# 2 <{10},              
# {2}>  => <{1}>  0.48484848          1    1 
# 3 <{11},              
# {2}>  => <{1}>  0.22727273          1    1 
# 4 <{12},              
# {2}>  => <{1}>  0.34848485          1    1 
# 5 <{11},              
# {13}> => <{1}>  0.09090909          1    1 
# 6 <{11},              
# {12}> => <{1}>  0.10606061          1    1 
# 7 <{10},              
# {10}> => <{1}>  0.40909091          1    1 
# 8 <{12},              
# {10}> => <{1}>  0.28787879          1    1 


## People in this cases tend to improve their levels of glucose, giving as a result levels among the
# normal values of people without diabetes, since lift is larger than 1, it means that they are dependent in larger
# degree.

#People maintained their "no diabetes" value or improved from "diabetes" to "no diabetes" when:
# Ingestion is less than usual (improved from "diabetes" to "no diabetes")
# Ingestion is more than usual (Values maintained)
# Ingest more than usual and more activity than usual
# less ingestion more activity is the conclusion in order to improve the levels of glucose

# inspect(no_diabetic)
# lhs       rhs       support confidence     lift 
# 1 <{11},              
# {3}>  => <{2}>  0.24242424          1 1.015385 
# 2 <{10},              
# {2}>  => <{2}>  0.48484848          1 1.015385 
# 3 <{11},              
# {2}>  => <{2}>  0.22727273          1 1.015385 
# 4 <{12},              
# {2}>  => <{2}>  0.34848485          1 1.015385 
# 5 <{11},              
# {13}> => <{2}>  0.09090909          1 1.015385 
# 6 <{12},              
# {13}> => <{2}>  0.21212121          1 1.015385 
# 7 <{10},              
# {12}> => <{2}>  0.28787879          1 1.015385 
# 8 <{11},              
# {12}> => <{2}>  0.10606061          1 1.015385 
# 9 <{12},              
# {12}> => <{2}>  0.19696970          1 1.015385 
# 10 <{11},              
# {10}> => <{2}>  0.21212121          1 1.015385 
# 11 <{12},              
# {10}> => <{2}>  0.28787879          1 1.015385 
# 12 <{12},              
# {1}>  => <{2}>  0.31818182          1 1.015385 



# > inspect(uncertain)
# lhs       rhs       support confidence lift 
# 1 <{10},              
# {2}>  => <{3}>  0.48484848          1    1 
# 2 <{11},              
# {2}>  => <{3}>  0.22727273          1    1 
# 3 <{12},              
# {2}>  => <{3}>  0.34848485          1    1 
# 4 <{11},              
# {13}> => <{3}>  0.09090909          1    1 
# 5 <{11},              
# {12}> => <{3}>  0.10606061          1    1 
# 6 <{11},              
# {11}> => <{3}>  0.07575758          1    1 
# 7 <{11},              
# {10}> => <{3}>  0.21212121          1    1 
# 8 <{12},              
# {10}> => <{3}>  0.28787879          1    1 


#####
#####
#####
#It was observed that people improved their levels of glucose when:
# * ingestion is less than usual
# * Activity is more than usual
# 
# On the other hand, People got worse levels of glucose when:
# * Ingestion is more than usual
