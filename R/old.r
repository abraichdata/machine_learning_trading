#PACKAGE TIMEEEEE
pkgs = c("TTR", "quantmod",
         "caret", "fscaret", "neuralnet", "kernlab", "gmodels", "C50", "nnet",
         "shiny")
for (p in pkgs) {
  if (! require(p, character.only = TRUE)) {
    install.packages(p)
    require(p, character.only = TRUE)
  }
}

library(quantmod)
library(TTR)
library(caret);
library(fscaret);
library(neuralnet);
library(kernlab);
library(gmodels);
library(C50);
library(nnet);
library(shiny)

read.table(file = "marcel.csv", sep=";", dec=",")

fulldata <- fulldata_temp[20:nrow(fulldata_temp),]

move_cols <- sapply(fulldata, is.character)
move_cols[["RSI Move"]] <- TRUE
move_cols[["Target"]] <- TRUE
move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
move_col_names <- names(move_data)
non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
fulldata <- cbind(non_move_data,move_data)

Target <- fulldata$Target
fulldata$Target <- NULL
fulldata <- cbind(fulldata,Target)
fulldata <- fulldata[,2:ncol(fulldata)]
numRows = dim(fulldata)[1]
lastday = fulldata[numRows, ] 
fulldata = fulldata[1:numRows-1, ]

# Training / Testing Data Sets
set.seed(123)
splitIndex <- createDataPartition(fulldata$Target, p = .9, list = FALSE, times = 1) 

#random train / testing
trainDF <- fulldata[splitIndex,]
testDF <- fulldata[-splitIndex,]  

# Modell
svm.model <- ksvm(Target ~., data = trainDF, kernel = "polydot", C=9)

#Modellperformance-Auswertung
#svm.predict <- predict(svm.model,testDF)
#table(svm.predict,testDF$Target)


#Modeltraining
svm.model.linear <- ksvm(Target ~., data = trainDF, kernel = "vanilladot",C=9)

#confusion matrix
svm.confusion.linear <-  predict(svm.model.linear,trainDF)

#predicted values
svm.predict.linear <-  predict(svm.model.linear,testDF)

#accuracy für modell
agreement.linear <- svm.predict.linear == testDF$Target

accu.linear = sum(agreement.linear) / length(agreement.linear)

print(svm.model.linear)

#Save in file


lastdayPred <- predict(svm.model.linear, lastday)

if (lastdayPred == 0)

  print("Going down") else
    print ("Going up") 
