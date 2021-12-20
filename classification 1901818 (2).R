Seeds =read.csv('C:/Users/User/Downloads/seeds_dataset(1).csv' , sep=",")
seedsreal = read.csv('C:/Users/User/Downloads/seeds_real.csv', sep = ",")
--install.packages("rpart")

seeds = cbind(seedsreal,Seeds)
#randomise the data
seeds =seeds[sample(209,209),]

#separating the diff variables
seedsclass = seeds[1:1]
seedsvalues = seeds[2:8]

#set up training set 
seedsclassTrain = seedsclass[1:150,]
seedsvaluesTrain = seedsvalues[1:150,]

#set up Test set 
seedsclassTest = seedsclass[150:209,]
seedsvaluesTest = seedsvalues[150:209,]

#buidling a decision tree
library(rpart)
fit <- rpart(seedsclassTrain~., method = "class", data= seedsvaluesTrain)
plot(fit, uniform = TRUE, main ="Decision Tree for seeds")
text(fit, use.n=TRUE, all = TRUE, cex =.8)

#test the classifier using predictions 
treepred <- predict(fit, seedsvaluesTest, type ='class')

#comparing actual test values to get accurancy 
n = length(seedsclassTest) #no of test cases
ncorrect = sum(treepred== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

#2
#effect of pruning 0.1,0.01,0.001, reduces overfitting of the data 
pfit <- prune(fit, cp=0.1)
plot(pfit, uniform = TRUE, main = "Pruned Decision Tree for seeds")
text(pfit, use.n = TRUE, all = TRUE, cex=.8)

treepred1 <-predict(pfit, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #no of test cases
ncorrect = sum(treepred1== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

--0.01
pfit1 <- prune(fit, cp=0.01)
plot(pfit1, uniform = TRUE, main = "Pruned Decision Tree for seeds")
text(pfit1, use.n = TRUE, all = TRUE, cex=.8)

treepred2 <-predict(pfit1, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #no of test cases
ncorrect = sum(treepred2== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

--0.001
pfit2 <- prune(fit, cp=0.001)
plot(pfit2, uniform = TRUE, main = "Pruned Decision Tree for seeds")
text(pfit2, use.n = TRUE, all = TRUE, cex=.8)

treepred3 <-predict(pfit2, seedsvaluesTest, type = 'class')
n = length(seedsclassTest) #no of test cases
ncorrect = sum(treepred3== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

#3
select = seeds[6:7]
selectTrain = select[1:150,]
selectTest = select[150:209,]

fit1 <-rpart(seedsclassTrain~., method = 'class', data = selectTrain)
pfit3<-prune(fit1, cp=0.001)
plot(pfit3, uniform = TRUE, main ="Pruned Decision Tree for select")
text(pfit3, use.n = TRUE, all = TRUE, cex=.8)

treepred4<-predict(pfit3,selectTest, type = 'class')
n = length(seedsclassTest) #no of test cases
ncorrect = sum(treepred4== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

plot(select, col=treepred4)

#4
library(class)
#generate predicted classes
knn3pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=3)
n = length(seedsclassTest) #no of test cases
ncorrect = sum(knn3pred== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

#value of k=5
knn5pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=5)
n = length(seedsclassTest) #no of test cases
ncorrect = sum(knn5pred== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)

#value of k=8
knn8pred = knn(seedsvaluesTrain, seedsvaluesTest, seedsclassTrain, k=8)
n = length(seedsclassTest) #no of test cases
ncorrect = sum(knn8pred== seedsclassTest)#no of correctly
accuracy = ncorrect/n
print(accuracy)







