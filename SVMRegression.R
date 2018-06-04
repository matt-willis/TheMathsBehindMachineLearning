#Load package.
library(e1071)

#Import dataset and split into test and train sets. Please change your path to the source file below. :)
setwd("C:\\Users\\mdw.ACL\\OneDrive - Adatis\\Conferences\\SQL Plovdiv\\The Maths Behind Machine Learning")
dfCar <- read.csv("autosSubset.csv")
dfCar <- dfCar[(!dfCar$price>200000) & (!dfCar$yearOfRegistration<1980) & (!dfCar$yearOfRegistration>2018),]
iNoRow <- nrow(dfCar)
set.seed(10)
iTrainIndex <- sample(1:iNoRow, size = round(0.7*iNoRow), replace = FALSE)
dfTrain <- dfCar[iTrainIndex,]
dfTest <- dfCar[-iTrainIndex,]

#Enter details for my car!
dfTest <- rbind(dfTest,data.frame(seller="privat",offerType="Angebot",price=0,abtest="control",vehicleType="kleinwagen",yearOfRegistration=1997,gearbox="manuell",powerPS=29,model="polo",kilometer=90000,monthOfRegistration=8,fuelType="benzin",brand="volkswagen",notRepairedDamage="nein"))

#Scatter Plot.
plot(dfTrain$yearOfRegistration, dfTrain$price, col="blue", pch="*", cex=2)

#Build support vector machine regression model.
modelSVR <- svm(price ~ yearOfRegistration, dfTrain)

#Predict using support vector machine regression.
dfTest$predPrice <- predict(modelSVR, dfTest)

#Order dataset and plot car price against year of registration. If you view dfTest now, the row with a price of 0, 
#is my car. If you look at predPrice you can see how much it predicted my car to be worth. I forgot and left this 
#out of the session!
dfTest <- dfTest[order(dfTest$yearOfRegistration), ]
plot(dfTest$yearOfRegistration, dfTest$price, col="blue", pch="*", cex=2)

#Plot the predicted prices.
points(dfTest$yearOfRegistration, dfTest$predPrice, col = "red", type="l", lwd=5)