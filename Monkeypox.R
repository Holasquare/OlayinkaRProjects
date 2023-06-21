library(caret)
library(ggplot2)
library(lattice)

#load the dataset

monkey_pox <- read.csv("C:/Users/Olaiya Olayinka/Documents/monkey_pox.csv")
monkey_pox1 <- monkey_pox[2:11]
View(monkey_pox1)

# convert monkeypx column to factor

monkey_pox1$MonkeyPox <- factor(monkey_pox1$MonkeyPox)

set.seed(123)  # Set a seed for reproducibility
trainIndex <- createDataPartition(monkey_pox1$MonkeyPox, p = 0.8, list = FALSE)
trainData <- monkey_pox1[trainIndex, ]
testData <- monkey_pox1[-trainIndex, ]

# K Nearest Neighbor Classifier

knnmodel <- knn3(
  formula= MonkeyPox ~ .,
  data = trainData,
  k = 3 #Always use odd numbers, bigger k can cause underfitting, smaller value of k can cause overfitting This is hyperparameter tuning
)

#Predict the testdate

knnPredict <- predict(
  object = knnmodel,
  newdata = testData,
  type = "class"
)


# check and align factor levels
levels_testData <- levels(testData$MonkeyPox)
levels_knnpredict <- levels(knnPredict)


# Add missing levels to predictions
missing_levels <- levels_testData[!(levels_testData %in% levels_knnpredict)]
if (length(missing_levels) > 0) {
  knnPredict <- factor(knnpredict, levels = c(levels(knnPredict), missing_levels))
}

# Add missing levels to testData$MonkeyPox
missing_levels <- levels_knnpredict[!(levels_knnpredict %in% levels_testData)]
if (length(missing_levels) > 0) {
  testData$MonkeyPox <- factor(testData$MonkeyPox, levels = c(levels(testData$MonkeyPox), missing_levels))
}


# Create Confusion Matrix

knnMatrix <- confusionMatrix(
  data = knnPredict,
  reference = testData$MonkeyPox
)

# Print the ConfusionMatrix Model

print(knnMatrix)
