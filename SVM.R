setwd("D:\\ML\\R\\Support Vector Machine")
letters<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data",header = F)
View(letters)
colnames(letters)<-c("lettr","x-box","y-box","width","high","onpix","x-bar","y-bar","x2bar","y2bar","xybar","x2ybar","xy2bar","x-ege","xegvy","y-ege","yegvx")
View(letters)
write.csv(letters,file="letters.csv",col.names = F,row.names = F)
# For SVM all the features must be in numeric 
# All the feature values should be in same range 
# If not we should normalize 
# SVM model will perform Rescalling automatically 

# data is randomly arranged 
letters<-read.csv(file.choose())
letters_train<-letters[1:16000,]
letters_test<-letters[16001:20000,]
View(letters)
# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 


# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(lettr ~.,data = letters_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(lettr ~.,data = letters_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=letters_test)
mean(pred_rfdot==letters_test$lettr) # 93.075

# kernel = vanilladot
model_vanilla<-ksvm(lettr ~.,data = letters_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=letters_test)
mean(pred_vanilla==letters_test$lettr) # 93.075


# kernal = besseldot
model_besseldot<-ksvm(lettr ~.,data = letters_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=letters_test)
mean(pred_bessel==letters_test$lettr)

# kernel = polydot

model_poly<-ksvm(lettr ~.,data = letters_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = letters_test)
mean(pred_poly==letters_test$lettr) # 83.925




