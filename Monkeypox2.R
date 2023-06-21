# Load the dataset into a data frame called mp_data
install.packages('readxl')
library('readxl')
mp_data <- read_excel("C:/Users/Olaiya Olayinka/Documents/Excel Project Dataset.xlsx")

# Inspect the data with the head function 
head(mp_data)
tail(mp_data)
View(mp_data)

# Create Training and Testing Set

# Install packages
install.packages("caret")
install.packages("lattice")
library("caret")
library("lattice")

# Set the seed for reproducibility
set.seed(123)

# Create the training and testing sets
trainIndex <- createDataPartition(mp_data$PurchasedBike, p = 0.8, list = FALSE)
train_mpdata <- mp_data[trainIndex, ]
test_mpdata <- mp_data[-trainIndex, ]

# Verify the number of rows in the training and testing sets
nrow(train_mpdata)
nrow(test_mpdata)

# Model 1: K-Nearest Neighbors Classifier
library("class")

knnmodel <- knn(train = train_mpdata[, -1], test = test_mpdata[, -1], cl = train_mpdata$PurchasedBike, k = 3)

# Predict the test set with the model
knnPredict <- as.factor(knnmodel)

# Create Confusion Matrix
library("caret")
knnMatrix <- confusionMatrix(knnPredict, test_mpdata$PurchasedBike)
print(knnMatrix)
