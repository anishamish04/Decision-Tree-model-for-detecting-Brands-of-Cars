getwd()
setwd("C:/Users/mishra33/Downloads")

dataset <- read.csv('cars.csv')
summary(dataset)
str(dataset)

#removing NA by filling NA values with most probable values
colSums(is.na(dataset))
cubicinchesNA <- which(is.na(dataset$cubicinches))
dataset[cubicinchesNA, ]$cubicinches <- median(dataset$cubicinches, na.rm = T)



weightNA <- which(is.na(dataset$weightlbs))
dataset[weightNA, ]$weightlbs <- median(dataset$weightlbs, na.rm = T)
dataset$mpg = as.numeric(dataset$mpg)
dataset$cylinders = as.factor(dataset$cylinders)
dataset$cubicinches = as.numeric(dataset$cubicinches)
dataset$hp = as.numeric(dataset$hp)
dataset$weightlbs = as.numeric(dataset$weightlbs)
dataset$time.to.60 = as.numeric(dataset$time.to.60)
dataset$brand = as.factor(dataset$brand)

str(dataset)

#checking ratios of output class
table(dataset$brand)

#preparing dataset using all features
datasetFinal <- dataset[c(1,2,3,4,5,6,7,8)]



#splitting into train and test
library("caTools")

set.seed(2)
split <- sample.split(datasetFinal$brand, SplitRatio = 0.7)
train <- subset(datasetFinal, split == TRUE)
test <- subset(datasetFinal, split == FALSE)


#Build Decision Tree Model
library(rpart)
library(rpart.plot)

#Tree 1

dTree1 <- rpart(brand ~ ., data = train, method = 'class')
rpart.plot(dTree1)


#prediction
prediction1 <- predict(dTree1, test, type = 'class')

#confusion matrix
library(caret)
confusionMatrix(data = prediction1, reference = test$brand)


#Tree 2

dTree2 <- rpart(brand ~ mpg + cylinders + cubicinches, data = train, method = 'class')
rpart.plot(dTree2)

#prediction
prediction2 <- predict(dTree2, test, type = 'class')

#confusion matrix
confusionMatrix(data = prediction2, reference = test$brand)



#Tree 3
dTree3 <- rpart(brand ~ cubicinches + hp + weightlbs, data = train, method = 'class')
rpart.plot(dTree3)

#prediction
prediction3 <- predict(dTree3, test, type = 'class')

#confusion matrix
confusionMatrix(data = prediction3, reference = test$brand)

# Tree 4
dTree4 <- rpart(brand ~ mpg + cylinders + cubicinches + hp + weightlbs + time.to.60, data = train, method = 'class')
rpart.plot(dTree4)

#prediction
prediction4 <- predict(dTree4, test, type = 'class')

#confusion matrix
confusionMatrix(data = prediction4, reference = test$brand)




#test tree
dtreeTest <- rpart(brand ~ cubicinches + weightlbs + time.to.60 + hp, data = train, method = 'class')
rpart.plot(dtreeTest)

prediction5 <- predict(dtreeTest, test, type = 'class')

#confusion matrix
confusionMatrix(data = prediction5, reference = test$brand)
































