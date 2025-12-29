library(randomForest)
set.seed(555)
traindata <- read.csv("Local_JM.csv")
testdata <- read.csv("Local_YZ.csv")
traindata_SOC <- traindata[,6]
testdata_SOC <- testdata[,6]
traindata_BAND <- traindata[,8:59]
testdata_BAND <- testdata[,8:59]
train_data <- cbind(traindata_SOC,traindata_BAND)
test_data <- cbind(testdata_SOC,testdata_BAND)
len <- nrow(test_data)

q<-c()
for (i in 1:80){
  set.seed(555)
  yhh.forest <- randomForest(traindata_SOC~., data= train_data, ntree=500, nPrem=10, mtry=7, nodesize=i, maxnodes=i,importance=TRUE, proximity=T) #模型建立
  pp <- predict(yhh.forest, test_data[,-1])
  aa <-sqrt(sum((test_data[,1]-pp)^2)/len)
  R2R2 <- 1-sum((test_data[,1]-pp)^2)/sum((test_data[,1]-sum(test_data[,1])/len)^2)
  R2R2
  print(R2R2)
  q<-c(q,R2R2)
} 

n_max <- which.max(as.numeric(q)) 
cat('最优的c=', n_max, "\n")


set.seed(555)
yhh.forest <- randomForest(traindata_SOC~., data= train_data, ntree=500, nPrem=10, mtry=3, nodesize=80, maxnodes=80,importance=TRUE, proximity=T) #模型建立
p <- predict(yhh.forest, test_data[,-1]) 
p

p1 <- predict(yhh.forest, train_data[,-1]) 
p1
write.table(p1, file = "Result_JM.txt")

RMSE <-sqrt(sum((test_data[,1]-p)^2)/len)
R2 <- 1-sum((test_data[,1]-p)^2)/sum((test_data[,1]-sum(test_data[,1])/len)^2)
RPD <- sd(p)/RMSE
RPIQ <- (quantile(p,0.75)-quantile(p,0.25))/RMSE
Bias <- mean(test_data[,1]-p)

RMSE
R2
RPD
RPIQ
Bias
index <- cbind(RMSE,R2,RPD,RPIQ)

setwd('D:\\Desktop')
write.table(p, file = "Result_Local.txt")
write.table(index, file = "Index-RF.txt")
write.table(Bias, file = "Bias-RF.txt")

