data<-read.csv("C:\\Users\\bonnieb\\Downloads\\Cleansed data2.csv")
table(data$Returnflag)
round(prop.table(table(data$Returnflag))*100,2)
library(rpart)
library(rpart.plot)
library(caret)

library(Information)
library(riv)
library(devtools)
library(woe)
library(gridExtra)

stat<-create_infotables(data=data,y="Returnflag")
grid.table(stat$Summary,rows=NULL)
newdata<-subset(data,select=c(TeamName,Maker,Type.of.activity,Other_Sources,Returnflag))

set.seed(1234)
data1<-createDataPartition(data$Returnflag,times=1,p=0.70,list = F)
traindata<-data[data1,]
testdata<-data[-data1,]
round(prop.table(table(traindata$Returnflag))*100,2)
round(prop.table(table(testdata$Returnflag))*100,2)

###Executing the model - CART
treedata <- rpart(Returnflag ~ ., data = traindata,method = "class")
prp(treedata,type=2,extra = 2,under = TRUE)
pred.treedata <- predict(treedata, testdata,type = "class")
confusionMatrix(pred.treedata,testdata$Returnflag)

##Over sampling
library(ROSE)
data_balanced_over <- ovun.sample(Returnflag ~ ., data = traindata, method = "over",N = 223786)$data
table(data_balanced_over$Returnflag)

set.seed(1234)
data1<-createDataPartition(data_balanced_over$Returnflag,times=1,p=0.70,list = F)
traindata<-data_balanced_over[data1,]
testdata<-data_balanced_over[-data1,]
round(prop.table(table(traindata$Returnflag))*100,2)
round(prop.table(table(testdata$Returnflag))*100,2)


###Under sampling
data_balanced_under <- ovun.sample(Returnflag ~ ., data = traindata, method = "under", N = 100000, seed = 1)$data
table(data_balanced_under$Returnflag)

set.seed(1234)
data1<-createDataPartition(data_balanced_under$Returnflag,times=1,p=0.70,list = F)
traindata<-data_balanced_under[data1,]
testdata<-data_balanced_under[-data1,]
round(prop.table(table(traindata$Returnflag))*100,2)
round(prop.table(table(testdata$Returnflag))*100,2)

###Both over and under sampling
data_balanced_both <- ovun.sample(Returnflag ~ ., data = traindata, method = "both", p=0.5,N=100000, seed = 1)$data
table(data_balanced_both$Returnflag)

set.seed(1234)
data1<-createDataPartition(data_balanced_both$Returnflag,times=1,p=0.70,list = F)
traindata<-data_balanced_both[data1,]
testdata<-data_balanced_both[-data1,]
round(prop.table(table(traindata$Returnflag))*100,2)
round(prop.table(table(testdata$Returnflag))*100,2)

###Synthetic data generation
data.rose <- ROSE(Returnflag ~ ., data = traindata, seed = 1)$data
table(data.rose$Returnflag)

set.seed(1234)
data1<-createDataPartition(data.rose$Returnflag,times=1,p=0.70,list = F)
traindata<-data.rose[data1,]
testdata<-data.rose[-data1,]
round(prop.table(table(traindata$Returnflag))*100,2)
round(prop.table(table(testdata$Returnflag))*100,2)


####Additinal stats
install.packages("gains")
library(gains)
# gains table
actual <- ifelse(termCrossSell$target==1,1,0)
gains.cross <- gains(actual=actual , 
                     predicted=termCrossSell$predicted,
                     groups=10)
print(gains.cross)


install.packages("ineq")
library(ineq)

# Gini Index
ineq(termCrossSell$predicted,type="Gini")



library("ROCR")

perf.obj <- prediction(predictions=termCrossSell$predicted,
                       labels=termCrossSell$target)


# Get data for ROC curve
roc.obj <- performance(perf.obj, measure="tpr", x.measure="fpr")
plot(roc.obj,
     main="Cross-Sell - ROC Curves",
     xlab="1 - Specificity: False Positive Rate",
     ylab="Sensitivity: True Positive Rate",
     col="blue")
abline(0,1,col="grey")