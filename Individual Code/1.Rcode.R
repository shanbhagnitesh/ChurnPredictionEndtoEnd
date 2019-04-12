library(readr)
library(dplyr)
library(lubridate)
library(caret)

# Read the training data
setwd("C:/Users/nshanbhag/Desktop/BigDataAnalytics/Machine learning for Marketing/Individual Code")
library(readxl)
data <- read_excel("Telcodata.xlsx")


colnames(data)

#Data Cleaning 
unique(data$gender)
data$gender <- ifelse(data$gender == "Female" , 1,0)

unique(data$SeniorCitizen)
unique(data$InternetService)
data$Partner <- ifelse(data$Partner == "Yes" , 1,0)
data$Dependents <- ifelse(data$Dependents == "Yes" , 1,0)
data$PhoneService <- ifelse(data$PhoneService == "Yes" , 1,0)
hist(data$tenure)

data$MultipleLines <- ifelse(data$MultipleLines == "Yes" , 1,0)
data$InternetService

data$DSL <- ifelse(data$InternetService == "DSL", 1,0)
data$FiberOptic <- ifelse(data$InternetService == "Fiber optic", 1,0)
data$No <- ifelse(data$DSL != 1 & data$DSL != 1, 1,0)

data$InternetService <- NULL
data$OnlineSecurity <- ifelse(data$OnlineSecurity == "Yes" , 1,0)
data$OnlineBackup <- ifelse(data$OnlineBackup == "Yes" , 1,0)
data$DeviceProtection <- ifelse(data$DeviceProtection == "Yes",1,0)


unique(data$StreamingTV)

data$TechSupport <- ifelse(data$TechSupport == "Yes",1,0)
data$StreamingTV <- ifelse(data$StreamingTV == "Yes",1,0)
data$StreamingMovies <- ifelse(data$StreamingMovies == "Yes", 1,0)

unique(data$Contract)

data$MTM <- ifelse(data$Contract == "Month-to-month", 1,0)
data$Oneyear <- ifelse(data$Contract == "One year", 1,0)
data$Twoyear <- ifelse(data$Contract == "Two year", 1,0)
data$Contract <- NULL


unique(data$PaymentMethod)
data$PaperlessBilling <- ifelse(data$PaperlessBilling == "Yes",1,0)
data$PaymentMethod <- ifelse(data$PaymentMethod == "Electronic check", 1,data$PaymentMethod)
data$PaymentMethod <- ifelse(data$PaymentMethod == "Mailed check", 2,data$PaymentMethod)
data$PaymentMethod <- ifelse(data$PaymentMethod == "Bank transfer (automatic)", 3,data$PaymentMethod)
data$PaymentMethod <- ifelse(data$PaymentMethod == "Credit card (automatic)", 4,data$PaymentMethod)


length(unique(data$customerID))
data$churn <- data$Churn
data$Churn <- NULL
data$churn <- ifelse(data$churn == "Yes", 1,0)


#######################################################################################################
#######################################################################################################
#######################################################################################################

data$PaymentMethod <- as.numeric(data$PaymentMethod)

table(data$churn)
1869/ 5174


smp_size <- floor(0.50 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)
data

data[] <- lapply(data, function(x) if(is.double(x)) as.numeric(x) else x)

train <- data[train_ind, ]
test <- data[-train_ind, ]
table(train$churn)
table(test$churn)

# Load-in the libraries required
library(pROC)
library(rpart)
library(randomForest)


