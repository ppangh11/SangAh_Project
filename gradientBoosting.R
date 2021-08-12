#Boosting_GradientBoosting

library(caret)

#1. 데이터 준비
gboost <- read.csv('./20210810.csv', header = T)
gboost <- gboost[,-c(1)]

gboost$X.1 <- ifelse(gboost$X.1=="안전",1, ifelse(gboost$X.1=="경계",2,3))

for (i in (1:ncol(gboost))){
  gboost[,i]<-factor(gboost[,i])
}

head(gboost)

#2. train-test split
set.seed(12345)
index <- createDataPartition(gboost$X.1, p = 0.8, list = FALSE)
train_data <- gboost[index,]
test_data <- gboost[-index,]

#3. modeling
model_gbm <- caret::train(X.1~.,
                          data = train_data,
                          method = "gbm",
                          #preProcess = c("scaled", "center"),
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 5,
                                                   repeats = 3,
                                                   verboseIter = FALSE),
                          verbose = 0)

model_gbm

#4. confusionMatrix_accuracy
caret::confusionMatrix(
  data = predict(model_gbm, test_data),
  reference = test_data$X.1
)

