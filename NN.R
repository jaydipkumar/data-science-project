concrete <- read.csv(file.choose())
View(concrete)
str(concrete)
#attach(concrete)
#normal_concrete<-scale(concrete)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
concrete_norm<-as.data.frame(lapply(concrete[,-9],FUN=normalize))
#summary(concrete_norm$strength)
#summary(normal_concrete)
summary(concrete$strength)

concrete_norm <- cbind(concrete_norm,concrete$strength)
colnames(concrete_norm)[9] <- "strength"
concrete_train<-concrete_norm[1:773,]
concrete_test<-concrete_norm[774:1030,]

# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("strength",paste(colnames(concrete[-9]),collapse ="+"),sep="~")
#concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data = concrete_train)
concrete_model <- neuralnet(formula = formula_nn,data = concrete_train)
str(concrete_model)
plot(concrete_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(concrete_model,concrete_test[1:8])
str(model_results)
predicted_strength <- model_results$net.result
# predicted_strength
# model_results$neurons
cor(predicted_strength,concrete_test$strength)
plot(predicted_strength,concrete_test$strength)
model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,data= concrete_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,concrete_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,concrete_test$strength)
plot(pred_strn_5,concrete_test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

