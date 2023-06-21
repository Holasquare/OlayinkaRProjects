### Machine learning Data Science 28th May 2023###
### Working with Linear Regression using R ###


##Load the Dataset##
Eco_Data <- read.csv("C:/Users/Olaiya Olayinka/Documents/macro_economic_var.csv")
View(Eco_Data)

## Remove unwanted dataset-Year 
Eco_Data1 <- Eco_Data[2:5]
View(Eco_Data1)

# Correlation
cor(Eco_Data1)

cor(Eco_Data1$LogM2, Eco_Data1$LogFXR)
plot(Eco_Data1)

# Create Training and Testing Datasets

#caret is classification and regression
install.packages("ggplot2")
library(caret)
install.packages("ggplot2")
loaded_packages  <- library()$results[,1]

data_split <- createDataPartition(
  y= Eco_Data1$LogM2,
  p=0.80,
  list=FALSE
)

train_eco <- Eco_Data1[data_split,]
test_eco <- Eco_Data1[-data_split,]

nrow(train_eco)
nrow(test_eco)
nrow(Eco_Data1)


#Linear Reg
Lin_model <- lm(
  formula = LogM2 ~ LogFXR,
  data = train_eco
)

summary(Lin_model)

#Create Predictions

lin_predict <- predict(
  object = Lin_model,
  newdata = test_eco
)

# Performance of the Linear Model
rmse_eco <-  sqrt(mean((test_eco$LogM2 - lin_predict) ^2))
print(rmse_eco)
RMSE(test_eco$LogM2, lin_predict)

# Multiple Regression
# method 1 beginners step
mult_model_beg <- lm(
  formula = LogM2 ~ LogFXR + LogINF + LogEXR,
  data = train_eco
)

# method 2 professional method
mult_model <- lm(
  formula = LogM2 ~ .,
  data = train_eco
)

#inspect model
summary(mult_model)

#Create Predictions
mul_predict <- predict(
  object = mult_model,
  newdata = test_eco
)

# Compute the RMSE for the prediction
RMSE(test_eco$LogM2, mul_predict)
