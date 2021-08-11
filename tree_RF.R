#RandomForest

library(randomForest)

#1. 데이터 불러오기
d <- read.csv('./20210810.csv', header = T)

#1-1. 데이터 확인
head(d)
str(d)
summary(d)

d <- d[,-c(1)]

#1-2. 데이터 factor화
for (i in (1:ncol(d))){
  d[,i]<-factor(d[,i])
}
head(d)
str(d)
summary(d)

#2. train-test split
set.seed(12345)
d.train <- sample(nrow(d), 0.8*nrow(d), replace = F)
trainSet <- d[d.train,]
validSet <- d[-d.train,]

summary(trainSet)
summary(validSet)

#3.randomForest 적용
d.model1 <- randomForest(X.1~., data = trainSet, importance = T)
d.model
#3-1. ntree, mtry 추가
d.model2 <- randomForest(X.1~., data = trainSet, ntree = 500, mtry = 6, importance = T)
d.model2

#3-2. train set 적용
predTrain <- predict(d.model2, trainSet, type = 'class')
table(predTrain, trainSet$X.1)

#3-3. test set 예측
predValid <- predict(d.model2, validSet, type = 'class')
#3-4. accuracy
mean(predValid == validSet$X.1)
table(predValid, validSet$X.1)

#importance(d.model2)
