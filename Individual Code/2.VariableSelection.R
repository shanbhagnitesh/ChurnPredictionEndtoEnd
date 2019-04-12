library(ISLR)
library(MASS)
install.packages("pROC")
library(pROC)
######################## Forward Selection Method 
#logistic regression model
lrm <- glm(churn ~ ., data=train[, 2:25], family = binomial)

#forward stepwise selection
step.model <- stepAIC(lrm, direction = "forward", 
                      trace = FALSE)
summary(step.model)


######################## Backward Selection Method 
#logistic regression model
lrm <- glm(churn ~ ., data=train[, 2:25], family = binomial)

#forward stepwise selection
step.model <- stepAIC(lrm, direction = "backward", 
                      trace = FALSE)
summary(step.model)


######################## Hybrid Selection Method 
#logistic regression model
lrm <- glm(churn ~ ., data=train[, 2:25], family = binomial)

#forward stepwise selection
step.model <- stepAIC(lrm, direction = "both", 
                      trace = FALSE)
summary(step.model)






# STEPWISE Procedure

# All possible variables:
variables = names(train)[-25]
variables = variables[-1]

variablesorder = c()


# Construct a logistic regression model with no variables
model = glm(churn ~ 1,data=train,family=binomial)

# Construct a formula with all the variables
formula<-formula(paste("churn","~",paste(variables,collapse="+")))

# Stepwise procedure
for(i in c(1:length(variables))){
  #calculate AIC of each model
  info = add1(model,scope=formula,data=train)
  print(info)
  #get variable with lowest AIC
  orderedvariables = rownames(info[order(info$AIC),])
  v = orderedvariables[orderedvariables!="<none>"][1]
  #add variable to formula
  variablesorder = append(variablesorder,v)
  formulanew = formula(paste("churn","~",paste(variablesorder,collapse = "+")))
  model = glm(formulanew,data=train,family=binomial)
  print(v)
}

auctrain = rep(0,length(variablesorder))
auctest = rep(0,length(variablesorder))
for(i in c(1:(length(variablesorder)))){
  vars = variablesorder[1:i]
  print(vars)
  formula<-paste("churn","~",paste(vars,collapse="+"))
  model<-glm(formula,data=train,family="binomial")	
  predicttrain<-predict(model,newdata=train,type="response")
  predicttest<-predict(model,newdata=test,type="response")
  auctrain[i] = auc(train$churn,predicttrain)
  auctest[i] = auc(test$churn,predicttest)
} 

#SOLUTION
plot(auctrain, main="AUC", col="red", ylim=c(0.55,0.75))
par(new=TRUE)
plot(auctest,col="blue",add=TRUE, ylim=c(0.55,0.75))

finalvariables = variablesorder[c(0:12)]
formula<-paste("churn","~",paste(finalvariables,collapse="+"))
model<-glm(formula,data=train,family="binomial")	
predicttrain<-predict(model,newdata=train,type="response")
predicttest<-predict(model,newdata=test,type="response")
auctrain = auc(train$churn,predicttrain)
auctest = auc(test$churn,predicttest)
auctrain
auctest

# Auc train 0.8443
# Auc train 0.8435

predicted <- as.numeric(predicttest>0.5)
reference <- test$churn
u <- union(predicted, reference)
xtab <- table(factor(predicted, u), factor(reference, u))
confusionMatrix(xtab)



##### The Variables are selected and tried and it all give the same AUC