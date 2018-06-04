#Include the relevant library.
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Read in the source file into a dataframe and then split into Train and Test. Please change your path to the source file below. :)
setwd("C:\\Users\\mdw.ACL\\OneDrive - Adatis\\Conferences\\SQL Plovdiv\\The Maths Behind Machine Learning")
dfTitanic <- read.csv("titanic3.csv")
iNoRow <- nrow(dfTitanic)
set.seed(10)
iTrainIndex <- sample(1:iNoRow, size = round(0.7*iNoRow), replace = FALSE)
dfTrain = dfTitanic[iTrainIndex ,]
dfTest = dfTitanic[-iTrainIndex ,]

#What percentage of people survived?
prop.table(table(dfTitanic$survived))

#What perentage of men & women survived?
prop.table(table(dfTitanic$sex, dfTitanic$survived),1)

#What percentage of men, women & children survived?
dfTitanic$child <- 0
dfTitanic$child[dfTrain$age < 16] <- 1
aggregate(survived ~ sex + child, data=dfTitanic, FUN=sum)
aggregate(survived ~ sex + child, data=dfTitanic, FUN=function(x) {length(x)-sum(x)})
aggregate(survived ~ sex + child, data=dfTitanic, FUN=function(x) {sum(x)/length(x)})

#What percentage of men, women, children, rich & poor survived?
aggregate(survived ~ sex + child + pclass, data=dfTitanic, FUN=sum)
aggregate(survived ~ sex + child + pclass, data=dfTitanic, FUN=function(x) {length(x)-sum(x)})
aggregate(survived ~ sex + child + pclass, data=dfTitanic, FUN=function(x) {(length(x)-sum(x))/length(x)})

#Tree time!
fitG <- rpart(formula=survived ~ pclass + sex + age + sibsp + parch + fare + embarked, data=dfTrain, method="class")
fancyRpartPlot(fitG)
printcp(fitG)
summary(fitG)

#Compute the accuracy of the tree.
dfTest$pred <- predict(fitG, dfTest, type = "class")
mean(dfTest$pred == dfTest$survived)