# Read the dataset
wbcd <- read.csv(file.choose())
View(wbcd)
#First colum in dataset is id which is not required so we will be taking out
wbcd <- wbcd[-1]
View(wbcd)
#table of diagonis B <- 357 and M <- 212
table(wbcd$diagnosis)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B","M"), labels = c("Benign","Malignant"))

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(wbcd$diagnosis))*100,1)
summary(wbcd[c("radius_mean","texture_mean","perimeter_mean")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
norm(c(1,2,3,4,5))
norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
wbcd_n <- as.data.frame(lapply(wbcd[2:31], norm))
View(wbcd_n)

#create training and test datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test <- wbcd_n[470:569,]

#Get labels for training and test datasets

wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels <- wbcd[470:569,1]

# Build a KNN model on taining dataset
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_wbcd_pred <- knn(train=wbcd_train,test=wbcd_train,cl=wbcd_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_wbcd_pred==wbcd_train_labels))
  test_wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_wbcd_pred==wbcd_test_labels))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))


wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

