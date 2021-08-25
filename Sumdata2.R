setwd("C:/Users/82104/Desktop/상아매니지먼트")

library(readxl)

dataset <- read.csv("0819original.csv")
project_df <- as.data.frame(read_excel("codeless.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,6,9,16,12)],dataset[,-1])
newdata

#발주처를 넣을까요, 말까요

names(newdata)[c(1,2,3,4)] = c("프로젝트분야","플랜트종류","Location","설계변경공종","발주처")

#write.csv(newdata,file="C:/Users/82104/Desktop/상아매니지먼트/0820Sumdata.csv")


set.seed(123)
y_data <- newdata[,100]
x_data <- newdata[,c(-98,-99,-100,-101)]


str(x_data)

# 범주형 변수 : 프로젝트 분야, 규모/용량, Location, 설계변경공종, 설계변경유형
x_data$프로젝트분야 <- as.factor(x_data$프로젝트분야)
x_data$플랜트종류 <- as.factor(x_data$플랜트종류)
x_data$Location <- as.factor(x_data$Location)
x_data$설계변경공종 <-as.factor(x_data$설계변경공종)
x_data$발주처 <-as.factor(x_data$발주처)




for (i in c(6:97)){
  x_data[,i] <- as.factor(x_data[,i])
}


# 가변수(dummy 변수화)
#library(dummies)

#dummy.data.frame(x_data)


# 모델링 (랜덤포레스트)

# train, test 데이터 분리
train <- sample(nrow(project_df), 0.8*nrow(project_df)) #훈련데이터 예측변수
x.train <- x_data[train,]
x.test <- x_data[-train,]

y.train <- y_data[train]
y.test <- y_data[-train]

y.train <- factor(y.train)


# 일정: ntree = 400, maxnodes=150, minsplit=100, mtry = 10
# 금액: ntree = 400 ,maxnodes=100, minsplit=50, mtry = 8


? randomForest
# randomForest 적용
library(randomForest)
set.seed(123)
randomforest <-  randomForest(y.train ~ ., ntree = 400 ,maxnodes=100, minsplit=50, mtry = 8, data=x.train, importance=T)


importance(randomforest)
varImpPlot(randomforest, type=2,pch=19, col=1, cex=1, main="")

#학습데이터 정확도
predtrain <- predict(randomforest, x.train, type='class') 
table(y.train, predtrain) 
mean(predtrain==y.train)

#검증데이터 정확도
predtest <- predict(randomforest, x.test, type='class')
table(y.test,predtest)
mean(predtest==y.test)

library(MLmetrics)
F1_Score(predtrain,y.train)
F1_Score(predtest, y.test)

