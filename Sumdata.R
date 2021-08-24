#knn_sumdata-siyeon-hwang

library(readxl)

data.d <- read.csv("./0819original.csv")
project_df <- as.data.frame(read_excel("codeless.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,6,9,12,16)],data.d[,-1])
newdata

#발주처를 넣을까요 네

names(newdata)[c(1,2,3,4,5)] = c("프로젝트분야","플랜트종류","Location","발주처","설계변경공종")


set.seed(123)
#newdata <- newdata[,c(-98,-99,-101)]
#newdata <- rename(newdata, "x.1"="금액심각도.범주." )
newdata <- newdata[,c(-99,-100,-101)]
newdata <- rename(newdata, "x.1"="일정심각도.범주." )


str(newdata)

# 범주형 변수 : 프로젝트 분야, 규모/용량, Location, 설계변경공종, 설계변경유형
newdata$프로젝트분야 <- as.factor(x_data$프로젝트분야)
newdata$플랜트종류 <- as.factor(x_data$플랜트종류)
newdata$Location <- as.factor(x_data$Location)
newdata$발주처 <- as.factor(x_data$발주처)
newdata$설계변경공종 <-as.factor(x_data$설계변경공종)

str(newdata)

for (i in c(6:98)){
  newdata[,i] <- factor(newdata[,i])
}
# numeric으로 바꿈 -> xgb modeling 할 때 데이터 numeric으로 넣어야 해서
#for (i in c(1:97)){
#  x_data[,i] <- as.numeric(factor(x_data[,i]))
#}


str(newdata)

# train, test 데이터 분리
ind <- sample(2, nrow(newdata), replace = T, prob = c(0.8, 0.2)) #훈련데이터 예측변수
training <- newdata[ind == 1,]
test <- newdata[ind == 2,]

str(training)

#모델링 (knn)

library(caret)
#install.packages("mlbench")
library(mlbench)
library(pROC)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3, 
                          classProbs = T,
                          )

set.seed(123)
fit <- train(x.1~.,
                  data = training,
                  method = 'knn',
                  tuneLength = 20,
                  trControl = trControl,
                  tuneGrid = expand.grid(k = 1:60)
             )
fit
plot(fit)
varImp(fit)

# 예측 정확도 & confusionMatrix
pred <- predict(fit, newdata = test)
confusionMatrix(pred, test$x.1)

#f1 score
library(MLmetrics)
F1_Score(pred, test$x.1)
