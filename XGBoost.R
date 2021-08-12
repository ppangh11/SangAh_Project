#Boosting

library(xgboost)
library(dplyr)
library(caret)
#install.packages("caTools")
library(caTools)

#1. 데이터 준비
boost <- read.csv('./20210810.csv', header = T)
boost <- boost[,-c(1)]

boost$X.1 <- ifelse(boost$X.1=="안전",1, ifelse(boost$X.1=="경계",2,3))

head(boost)

#2. train-test split
set.seed(12345)
sample_split <- sample.split(Y = boost$X.1, SplitRatio = 0.8)
train_set <- subset(x = boost, sample_split == TRUE)
test_set <- subset(x = boost, sample_split == FALSE)

y_train <- as.integer(train_set$X.1) - 1
y_test <- as.integer(test_set$X.1) - 1
x_train <- train_set %>% select(-X.1)
x_test <- test_set %>% select(-X.1)

#3. modeling
xgb_train <- xgb.DMatrix(data = as.matrix(x_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(x_test), label = y_test)
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
#num_class = length(levels(boost$X.1)
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 5000,
  verbose = 1
)
#xgb_model

#4. predictions & evaluations
xgb_preds <- predict(xgb_model, as.matrix(x_test), reshape=TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- c("1","2","3")
#colnames(xgb_preds) <- levels(boost$X.1)
xgb_preds

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- c("1","2","3")[y_test + 1]
xgb_preds

#xgb_preds$PredictedClass
#xgb_preds$ActualClass

#5. accuracy
accuracy <- sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds)
accuracy

confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
