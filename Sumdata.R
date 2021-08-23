#xgboost_sum : 일정만

library(readxl)

dataset <- read.csv("./0819original.csv")
project_df <- as.data.frame(read_excel("codeless.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,6,9,12,16)],dataset[,-1])
newdata

#발주처를 넣을까요 네

names(newdata)[c(1,2,3,4,5)] = c("프로젝트분야","플랜트종류","Location","발주처","설계변경공종")


set.seed(123)
newdata$일정심각도.범주. <- ifelse(newdata$일정심각도.범주.=="안전",0, ifelse(newdata$일정심각도.범주.=="경계",1,2))
y_data <- newdata[,98]
x_data <- newdata[,c(-98,-99,-100,-101)]


str(x_data)

# 범주형 변수 : 프로젝트 분야, 규모/용량, Location, 설계변경공종, 설계변경유형
x_data$프로젝트분야 <- as.factor(x_data$프로젝트분야)
x_data$플랜트종류 <- as.factor(x_data$플랜트종류)
x_data$Location <- as.factor(x_data$Location)
x_data$발주처 <- as.factor(x_data$발주처)
x_data$설계변경공종 <-as.factor(x_data$설계변경공종)

str(x_data)

for (i in c(6:97)){
  x_data[,i] <- factor(x_data[,i])
}
# numeric으로 바꿈 -> xgb modeling 할 때 데이터 numeric으로 넣어야 해서
for (i in c(1:97)){
  x_data[,i] <- as.numeric(factor(x_data[,i]))
}


str(x_data)

# 가변수(dummy 변수화)
#library(dummies)

#dummy.data.frame(x_data)


# 모델링 (XGBoost)

library(xgboost)
library(dplyr)
library(caret)
#install.packages("caTools")
library(caTools)


# train, test 데이터 분리
train <- sample(nrow(project_df), 0.8*nrow(project_df)) #훈련데이터 예측변수
x.train <- x_data[train,]
x.test <- x_data[-train,]

y.train <- y_data[train]
y.test <- y_data[-train]

#y.train <- factor(y.train)
#y.test <- factor(y.test)


#3. modeling
xgb_train <- xgb.DMatrix(data = as.matrix(x.train), label = y.train)
xgb_test <- xgb.DMatrix(data = as.matrix(x.test), label = y.test)

xgb_params <- list(
  booster = 'gbtree',
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = 3
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 5000,
  verbose = 1
)
xgb_model

#4. predictions & evaluations
#학습데이터 예측&평가
train.preds <- predict(xgb_model, as.matrix(x.train), reshape = T)
train.preds <- as.data.frame(train.preds)
colnames(train.preds) <- c("안전","경계","심각")
train.preds

train.preds$PredictedClass <- apply(train.preds, 1, function(y) colnames(train.preds)[which.max(y)])
train.preds$ActualClass <- c("안전","경계","심각")[y.train + 1]
train.preds

#검증데이터 예측&평가
xgb_preds <- predict(xgb_model, as.matrix(x.test), reshape=TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- c("안전","경계","심각")
xgb_preds

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- c("안전","경계","심각")[y.test + 1]
xgb_preds

#xgb_preds$PredictedClass
#xgb_preds$ActualClass

#5. Accuracy
#학습데이터 정확도 >> 0.8736
train.acc <- sum(train.preds$PredictedClass == train.preds$ActualClass) / nrow(train.preds)
train.acc
#검증데이터 정확도 >> 0.8447
accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy

confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

#6. F1 score >> 0.5333
#install.packages("MLmetrics")
library(MLmetrics)
F1_Score(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))

