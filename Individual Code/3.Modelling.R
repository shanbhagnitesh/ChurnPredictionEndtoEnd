# Custom function to calculate AUC:
install.packages("ROCR")
library(ROCR)
library(pROC)
library(rpart)
library(randomForest)
library(caret)
library(readr)
library(dplyr)
library(lubridate)
calculate_lift <- function(predicted, reference, val){
  
  if (is.factor(reference)) 
    reference <- as.integer(as.character(reference))
  
  lift_val <- data.frame(predicted, reference)
  lift_val <- lift_val[order(-lift_val[, 1]), ]
  lift_val <- lift_val[1:floor(nrow(lift_val)*val/100),]
  lift_val$predicted <- ifelse(lift_val$predicted > 0.5,1,0)
  lift_val$result <- ifelse(lift_val$predicted == lift_val$reference & lift_val$predicted == 1, 1,0)
  
  res <- as.numeric(mean(lift_val$result)/mean(reference))
  return(res)
}


auc = function(trueval, predval){
  df = as.data.frame(cbind(trueval,predval))
  names(df) = c("trueval","predval")
  auc = roc(trueval~predval,data=df)$auc
  return(auc)
}

#FILTER (PEARSON CORRELATION)
colnames(train)
# Remove churn variable
vars = names(train)[-25]
vars = vars[-1]
selected = c()





length(vars)
for(v in vars){
  print(v)
  cortest = cor.test(as.vector(unlist(train[,c(v)])),as.vector(unlist(train[,c("churn")])),method="pearson")
  pvalue = cortest$p.value
  print(pvalue)
  if(pvalue<0.001){
    selected = c(selected,v)
  }
}

f = paste("churn~", paste(selected,collapse="+"))
colnames(train)
lrmodel = glm(as.formula(f),data=train[,], family="binomial")

predictions_train = predict(lrmodel,newdata=train,type="response")
predictions_test = predict(lrmodel,newdata=test,type="response")
auc(train$churn,predictions_train)
auc(test$churn,predictions_test)




# Auc train 0.8466
# Auc train 0.8455

predicted <- as.numeric(predictions_train>=0.5)
reference <- train$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)

Top10Lift <- calculate_lift(predictions_train, reference, 10)
Top20Lift <- calculate_lift(predictions_train, reference, 20)
Top30Lift <- calculate_lift(predictions_train, reference, 30)
Top40Lift <- calculate_lift(predictions_train, reference, 40)
Top50Lift <- calculate_lift(predictions_train, reference, 50)
Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift

predicted <- as.numeric(predictions_test>=0.5)
reference <- test$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)

Top10Lift <- calculate_lift(predictions_test, reference, 10)
Top20Lift <- calculate_lift(predictions_test, reference, 20)
Top30Lift <- calculate_lift(predictions_test, reference, 30)
Top40Lift <- calculate_lift(predictions_test, reference, 40)
Top50Lift <- calculate_lift(predictions_test, reference, 50)
Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift


################################################################################


# STEPWISE LOGISTIC REGRESSION

# All possible variables:
variables = names(train)[-25]
vars = vars[-1]
variablesorder = c()


# Construct a logistic regression model with no variables
model = glm(churn ~ 1,data=train,family=binomial)

# Construct a formula with all the variables
formula <-formula(paste("churn","~",paste(variables,collapse="+")))


colnames(train)
install.packages("leaps")
library(leaps)
install.packages("rpart")
library(rpart)
regfit.fwd = regsubsets(churn ~., data=train[2:25],nvmax=10,method="forward")
reg.summary = summary(regfit.fwd)
names(reg.summary)

install.packages("mlbench")
library(mlbench)
install.packages("caret")
library(caret)
library(ROCR)
##############################
install.packages("gplots")
library(gplots)


# Decision tree

ctrl = rpart.control(minsplit = 10,cp = 0.0001, minbucket = 25,maxdepth = 10) 
tree <- rpart(churn ~ .,data = train[,2:25], method="class", control = ctrl)

predictions_test <- as.data.frame(predict(object = tree, newdata = test, type = "prob"))
predictions_train <- as.data.frame(predict(object = tree, newdata = train, type = "prob"))

predictions_test <- as.numeric(predictions_test$`1`)
predictions_train <- as.numeric(predictions_train$`1`)


auc(train$churn,predictions_train)
auc(test$churn,predictions_test)

predicted <- as.numeric(predictions_train > 0.5)
reference <- train$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)


Top10Lift <- calculate_lift(predictions_train, reference, 10)
Top20Lift <- calculate_lift(predictions_train, reference, 20)
Top30Lift <- calculate_lift(predictions_train, reference, 30)
Top40Lift <- calculate_lift(predictions_train, reference, 40)
Top50Lift <- calculate_lift(predictions_train, reference, 50)
Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift



predicted <- as.numeric(predictions_test > 0.5)
reference <- test$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)



Top10Lift <- calculate_lift(predictions_test, reference, 10)
Top20Lift <- calculate_lift(predictions_test, reference, 20)
Top30Lift <- calculate_lift(predictions_test, reference, 30)
Top40Lift <- calculate_lift(predictions_test, reference, 40)
Top50Lift <- calculate_lift(predictions_test, reference, 50)
Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift



# AUC for both train and test is 75.24 and 71.96

# Random Forest
unique(train$churn)
train$churn <- as.factor(as.character(train$churn))
rForest <- randomForest(churn ~ .,data = train[,2:25], ntree=200,na.action=na.exclude)

predictions_test <- as.data.frame(predict(object = rForest, newdata = test, type = "prob"))
predictions_train <- as.data.frame(predict(object = rForest, newdata = train, type = "prob"))

auc(train$churn,predictions_train$`1`)
auc(test$churn,predictions_test$`1`)

# Auc train 99.98
# Auc train 83.09

predictions_test <- as.numeric(predictions_test$`1`)
predictions_train <- as.numeric(predictions_train$`1`)

predicted <- as.numeric(predictions_train>0.5)
reference <- train$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)


Top10Lift <- calculate_lift(predictions_train, reference, 10)
Top20Lift <- calculate_lift(predictions_train, reference, 20)
Top30Lift <- calculate_lift(predictions_train, reference, 30)
Top40Lift <- calculate_lift(predictions_train, reference, 40)
Top50Lift <- calculate_lift(predictions_train, reference, 50)

Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift



predicted <- as.numeric(predictions_test>0.5)
reference <- test$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)
# Better AUC but again We are unable to predict any donors

table(test$churn)
#   0      1 
# 25493   152

Top10Lift <- calculate_lift(predictions_test, reference, 10)
Top20Lift <- calculate_lift(predictions_test, reference, 20)
Top30Lift <- calculate_lift(predictions_test, reference, 30)
Top40Lift <- calculate_lift(predictions_test, reference, 40)
Top50Lift <- calculate_lift(predictions_test, reference, 50)
Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift


##############################################################################
# Trying out SVM 

install.packages("caret")
library(caret)

train
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

train$churn <- as.factor(as.character(train$churn))
svm_Linear <- train(churn ~., data = train[,2:25], method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10,na.action=na.exclude)


test_pred <- predict(svm_Linear, newdata = test)
test_pred

predictions_test <- predict(object = svm_Linear, newdata = test)
predictions_train <- predict(object = svm_Linear, newdata = train)

predictions_test <- as.numeric(predictions_test$`1`)
predictions_train <- as.numeric(predictions_train$`1`)


auc(train$churn,predictions_train)
auc(test$churn,predictions_test)

predicted <- as.numeric(predictions_train > 0.5)
reference <- train$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)


Top10Lift <- calculate_lift(predictions_train, reference, 10)
Top20Lift <- calculate_lift(predictions_train, reference, 20)
Top30Lift <- calculate_lift(predictions_train, reference, 30)
Top40Lift <- calculate_lift(predictions_train, reference, 40)
Top50Lift <- calculate_lift(predictions_train, reference, 50)
Top10Lift 
Top20Lift
Top30Lift
Top40Lift
Top50Lift