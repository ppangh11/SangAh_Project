#데이터불러오기 ####
#setwd("C:/Users/rim/Desktop/sangah")

#install.packages("readxl")
library(readxl)

project_df <- as.data.frame(read_excel("data.xlsx",skip=5)) #원데이터 불러오기
project_period <- project_df[,c(18,23)]  #설계변경사유,일정심각도 
# project_cost <- project_df[,c(18,26)]  #설계변경사유,금액심각도


names(project_period)<-c("text","type")
# names(project_cost)<-c("text","type")


#전처리 ####

#install.packages("dplyr")
library(dplyr)
#install.packages("tibble")
library(tibble)

#DataframeSource 함수는 text, id 열이 필요함. 그 형태로 만드는 과정
project_period <- project_period %>%
  select(text, type) %>%
  add_column(doc_id=1:nrow(.), .before=1)

# project_cost <- project_cost %>%
  # select(text, type) %>%
  #  add_column(doc_id=1:nrow(.), .before=1)

#install.packages("tm")
library(tm)

docs <- VCorpus(DataframeSource(project_period))
docs   # 1029개의 자료를 가진 corpus 객체

lapply(docs, content)

#meta(docs)

# 모두 소문자로 바꾸는 작업
docs <- tm_map(docs, content_transformer(tolower))

# 특정 단어를 다른 형태로 바꾸는 작업
changeWord <- content_transformer(function(x, pattern, changepattern){return(gsub(pattern, changepattern, x))})

#원본데이터에서 나온 단어 처리
changeword_df <- as.data.frame(read_excel("단어변경.xlsx", trim_ws=FALSE)) #데이터불러오기

i <- 1
for (i in c(1:nrow(changeword_df))){
  docs <- tm_map(docs, changeWord, changeword_df[i,1], changeword_df[i,2])
}

# 중요한 단어를 잘라내기 위해 양쪽에 띄어쓰기 삽입: ex)"변경" -> " 변경 "
plusSpace <- content_transformer(function(x, pattern){return(gsub(pattern,paste(" ",pattern," ",sep=""),x))})

# 원본데이터에서 나온 단어 처리

spaceword_df <- as.data.frame(read_excel("띄어쓰기.xlsx", trim_ws=FALSE)) #데이터불러오기

i <- 1
for (i in c(1:nrow(spaceword_df))){
  docs <- tm_map(docs, plusSpace, spaceword_df[i,1])
}


# " " 안에 제거할 문자를 입력하면, " "으로 대체됨.
# aa/aa 인경우 /를 그냥 제거하면 aaaa가 되어서 의미가 변질될 수 있는데,
# " "로 대체함으로써 aa aa로 변형할 수 있음.
toSpace <- content_transformer(function(x, pattern){return(gsub(pattern," ",x))})
#docs <- tm_map(docs, toSpace, c("[[:punct:]]"))
docs <- tm_map(docs, toSpace, c(","))
#docs <- tm_map(docs, toSpace, c("[.]"))
docs <- tm_map(docs, toSpace, c("\\("))
docs <- tm_map(docs, toSpace, c("\\)"))
#docs <- tm_map(docs, toSpace, c("\\-"))  #제품 코드 얻을거면 이거 넣으면 됨. #제품 코드를 얻으면 인치수는 얻을 수 있음. (숫자)" 형태. # 근데 제품 코드 쓸거면 필요없는 말은 수작업으로 빼야 할듯
docs <- tm_map(docs, toSpace, c("의"))
docs <- tm_map(docs, toSpace, c("에는"))
docs <- tm_map(docs, toSpace, c("을"))
docs <- tm_map(docs, toSpace, c("를"))
docs <- tm_map(docs, toSpace, c("에대한"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("하여"))
docs <- tm_map(docs, toSpace, c("됨"))
docs <- tm_map(docs, toSpace, c("로"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("따라"))
docs <- tm_map(docs, toSpace, c("따른"))
docs <- tm_map(docs, toSpace, c("대한"))
docs <- tm_map(docs, toSpace, c("인한"))
docs <- tm_map(docs, toSpace, c("되는"))
docs <- tm_map(docs, toSpace, c("위해"))
docs <- tm_map(docs, toSpace, c("위한"))
docs <- tm_map(docs, toSpace, c("하고"))
docs <- tm_map(docs, toSpace, c("하는"))
docs <- tm_map(docs, toSpace, c("하지"))
docs <- tm_map(docs, toSpace, c("에서"))
docs <- tm_map(docs, toSpace, c("한다."))


#" " 안에 제거할 문자를 입력하면, 그냥 제거됨.(띄어쓰기 상관없이)
myRemove <- content_transformer(function(x, pattern){return(gsub(pattern, "", x))})

#docs <- tm_map(docs, myRemove, c()) # " " 안에 제거할 문자 입력

docs <- tm_map(docs, stripWhitespace) #공백제거


# 입력한 단어 삭제(띄어쓰기 되어 있는 단어만)
mystopwords <- c(c("b/0","for","and","of","to","with")) 
docs <- tm_map(docs, removeWords, mystopwords)

dtm <- DocumentTermMatrix(docs)

# 단어행렬 생성(wordLengths: 글자 수 범위, global: 빈도수 범위)
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(2,10), bounds=list(global=c(10, 900))))
dtm # 1060개 행에서 378개의 단어

# dtm을 행렬 형태로 변환
termfreq <- colSums(as.matrix(dtm))
termfreq  
termfreq[head(order(termfreq, decreasing=TRUE))]
termfreq[tail(order(termfreq, decreasing=TRUE))]

# 지정된 최소 출현 빈도 이상 등장한 단어를 찾아줌
findFreqTerms(dtm, lowfreq=200)

#예측변수를 범주형 변수로 만드는 함수
toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, level=c(0,1))
  return(x)
}

project_period.dtm <- apply(dtm, MARGIN=2, toFactor) # 단어의 이진행렬화
write.csv(project_period.dtm,"이진행렬.csv")


#워드 클라우드####
# install.packages("wordcloud")
library(wordcloud)

# install.packages("RColorBrewer")
library(RColorBrewer)
set.seed(123)
#windows(width=7, height=7)
#wordcloud(words=names(termfreq), freq=termfreq, scale=c(4, 0.5) ,min.freq=0, max_words=200, rot.per=0, random.order=FALSE, random.color= FALSE, colors=brewer.pal(6, "Set2"))

# y의 범주별로 워드 클라우드를 만들기
# 행은 레코드, 열은 단어, value는 빈도수인 행렬 형태로 만들어야 함.
hamproject <- as.matrix(dtm)

rownames(hamproject) <- project_period$type
hamproject <- rowsum(hamproject, group=rownames(hamproject)) #범주별로 단어가 얼마나 등장하는지
hamproject

set.seed(123)

#windows(width=7, height=7)  워드클라우드 저장화면
#comparison.cloud(t(hamproject), colors=c("cornflowerblue","tomato","black"),title.size=2, title.colors=c("blue", "red","black"), title.bg.colors="wheat",rot.per=0, scale=c(5,0.4),max.words=200)
# 워드클라우드 확인 필요시 주석 제거



#훈련, 검증데이터 나누기 ####
set.seed(123)
newdata <- read.csv("이진행렬.csv")[,-1]
newdata <- cbind(project_df[,c(5,6,9,16,12)],newdata)
newdata <- cbind(newdata, project_df[,c(23,26)])
names(newdata)[c(1,2,3,4,5)] = c("프로젝트분야","플랜트종류","Location","설계변경공종","발주처")
train <- sample(nrow(newdata), 0.8*nrow(newdata))
y.train.period <- newdata[train,98] #훈련데이터용 일정심각도
y.test.period <- newdata[-train,98] #검증데이터용 일정심각도
y.train.cost <- newdata[train,99] #훈련데이터 금액심각도
y.test.cost <- newdata[-train,99] #검증데이터 금액심각도
x.train <- newdata[train,c(1:97)] #훈련데이터 독립변수
x.test <- newdata[-train,c(1:97)] #검증데이터 독립변수

#변수 factor화 ####
x.train$프로젝트분야 <- as.factor(x.train$프로젝트분야)
x.test$프로젝트분야 <- as.factor(x.test$프로젝트분야)

x.train$플랜트종류 <- as.factor(x.train$플랜트종류)
x.test$플랜트종류 <- as.factor(x.test$플랜트종류)

x.train$Location <- as.factor(x.train$Location)
x.test$Location <- as.factor(x.test$Location)

x.train$설계변경공종 <-as.factor(x.train$설계변경공종)
x.test$설계변경공종 <-as.factor(x.test$설계변경공종)

x.train$발주처 <-as.factor(x.train$발주처)
x.test$발주처 <-as.factor(x.test$발주처)

y.train.cost <- as.factor(y.train.cost)
y.train.period <- as.factor(y.train.period)
y.test.cost <- as.factor(y.test.cost)
y.test.period <- as.factor(y.test.period)



for (i in c(6:97)){
  x.train[,i] <- as.factor(x.train[,i])
  x.test[,i] <- as.factor(x.test[,i])
}

#모델링




# 1. Multinomial Regression ####

# 1-1 일정심각도

#install.packages("nnet")
library(nnet)
model_multi1 = multinom(y.train.period ~ ., data = x.train) # 다항회귀 모델 적합

multi_trainP = predict(model_multi1, newdata = x.train, "class") # 훈련모델로 훈련데이터 예측
tab = table(y.train.period, multi_trainP)
round((sum(diag(tab))/sum(tab))*100,2) # 훈련데이터 정확도 확인

multi_testP = predict(model_multi1, newdata = x.test, "class") # 훈련모델로 검증데이터 예측
tab_test = table(y.test.period, multi_testP) 
auc_P <- c((sum(diag(tab_test))/sum(tab_test))) # 검증데이터 정확도 확인


#install.packages("MLmetrics")
library(MLmetrics)
f1score_P <- c(F1_Score(multi_testP,as.factor(y.test.period)))


# 1-2 금액심각도
model_multi2 = multinom(y.train.cost ~ ., data = x.train) # 다항회귀 모델 적합

multi_trainC = predict(model_multi2, newdata = x.train, "class") # 훈련모델로 훈련데이터 예측
tab = table(y.train.cost, multi_trainC)
round((sum(diag(tab))/sum(tab))*100,2) # 훈련데이터 정확도 확인

multi_testC = predict(model_multi2, newdata = x.test, "class") # 훈련모델로 검증데이터 예측
tab_test = table(y.test.cost, multi_testC) 
auc_C <- c((sum(diag(tab_test))/sum(tab_test))) # 검증데이터 정확도 확인


#install.packages("MLmetrics")
library(MLmetrics)
f1score_C <- c(F1_Score(multi_testC,as.factor(y.test.cost)))




# 2. Random Forest ####

# randomForest 적용
#install.packages("randomForest")
library(randomForest)

# 일정심각도 최적 파라미터: ntree = 400, maxnodes=150, minsplit=100, mtry = 10
# 금액심각도 최적 파라미터: ntree = 400 ,maxnodes=100, minsplit=50, mtry = 8

# 2-1 일정심각도
model_RFP <- randomForest(y.train.period ~ ., ntree = 400 ,maxnodes=150, minsplit=100, mtry = 10, data=x.train, importance=T) #모델 적합

RF_trainP <- predict(model_RFP, x.train, type='class') #예측
table(y.train.period,RF_trainP) #오분류표 확인
mean(RF_trainP==y.train.period) #훈련데이터 정확도

x.test <- rbind(x.train[1, ] , x.test)
x.test <- x.test[-1,]
RF_testP <- predict(model_RFP, x.test, type='class') #예측
table(y.test.period,RF_testP) #오분류표 확인
auc_P <- c(auc_P,mean(RF_testP==y.test.period)) # 검증데이터정확도

library(MLmetrics)
f1score_P <- c(f1score_P,F1_Score(RF_testP, y.test.period)) #f1score


# 2-2 금액심각도
model_RFC <- randomForest(y.train.cost ~ ., ntree = 400 ,maxnodes=100, minsplit=50, mtry = 8, data=x.train, importance=T)

RF_trainC <- predict(model_RFC, x.train, type='class') #예측
table(y.train.cost,RF_trainC) #오분류표 확인
mean(RF_trainC==y.train.cost) #훈련데이터 정확도

x.test <- rbind(x.train[1, ] , x.test)
x.test <- x.test[-1,]
RF_testC <- predict(model_RFC, x.test, type='class') #예측
table(y.test.cost,RF_testC) #오분류표 확인
auc_C <- c(auc_C,mean(RF_testC==y.test.cost)) #검증데이터 정확도

library(MLmetrics)
f1score_C <- c(f1score_C,F1_Score(RF_testC, y.test.cost)) #f1score


# 3. NaiveBayes ####

##install.packages('e1071')
library(e1071)
#install.packages("caret")
library(caret)


# 3-1 일정심각도
model_NBP <-naiveBayes(x.train, y.train.period, laplace = 1) # 모델적합

NBP_trainP <- predict(model_NBP,x.train) #훈련데이터 예측
CM <- confusionMatrix(NBP_trainP,y.train.period) #훈련데이터 정확도
#CM$overall[1]*100

NBP_testP <- predict(model_NBP,x.test) #검증데이터 예측
CM <- confusionMatrix(NBP_testP,y.test.period) 
auc_P <- c(auc_P,CM$overall[1]) #검증데이터 정확도

library(MLmetrics)
f1score_P <- c(f1score_P,F1_Score(NBP_testP,y.test.period))

# 3-2 금액심각도

model_NBC <-naiveBayes(x.train, y.train.cost, laplace = 1) # 모델적합

NBP_trainC <- predict(model_NBC,x.train) #훈련데이터 예측
CM <- confusionMatrix(NBP_trainC,y.train.cost) #훈련데이터 정확도
#CM$overall[1]*100

NBP_testC <- predict(model_NBC,x.test) #검증데이터 예측
CM <- confusionMatrix(NBP_testC,y.test.cost) 
auc_C <- c(auc_C,CM$overall[1]) #검증데이터 정확도

library(MLmetrics)
f1score_C <- c(f1score_C,F1_Score(NBP_testC,y.test.cost))



# 4. KNN ####

#install.packages("caret")
library(caret)
#install.packages("mlbench")
library(mlbench)
#install.packages("pROC")
library(pROC)

trControl <- trainControl(method = "repeatedcv",
                          number = 10,
                          repeats = 3, 
                          classProbs = T,
)

# 4-1 일정심각도

knn_x_train <- cbind(x.train,y.train.period)
knn_x_test <- cbind(x.test,y.test.period)
model_knnP <- train(y.train.period~.,
                    data = knn_x_train,
                    method = 'knn',
                    tuneLength = 20,
                    trControl = trControl,
                    tuneGrid = expand.grid(k = 3)
)

knn_testP <- predict(model_knnP, newdata = knn_x_test)
CM <- confusionMatrix(knn_testP, knn_x_test$y.test.period)
auc_P <- c(auc_P,CM$overall[1]) #정확도

library(MLmetrics)
f1score_P <- c(f1score_P,F1_Score(knn_testP, knn_x_test$y.test.period))#f1 score


# 4-1 금액심각도

knn_x_train2 <- cbind(x.train,y.train.cost)
knn_x_test2 <- cbind(x.test,y.test.cost)
model_knnC <- train(y.train.cost~.,
                    data = knn_x_train2,
                    method = 'knn',
                    tuneLength = 20,
                    trControl = trControl,
                    tuneGrid = expand.grid(k = 3)
)

knn_testC <- predict(model_knnC, newdata = knn_x_test2)
CM <- confusionMatrix(knn_testC, knn_x_test2$y.test.cost)
auc_C <- c(auc_C,CM$overall[1]) #정확도

library(MLmetrics)
f1score_C <- c(f1score_C,F1_Score(knn_testC, knn_x_test2$y.test.cost))#f1 score
# 5. xgboost ####

# 5.1 일정심각도

library(readxl)

dataset <- read.csv("이진행렬.csv")
project_df <- as.data.frame(read_excel("data.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,6,9,12,16)],dataset[,-1])
newdata <- cbind(newdata,project_df[,c(23,26)])
newdata


names(newdata)[c(1,2,3,4,5)] = c("프로젝트분야","플랜트종류","Location","발주처","설계변경공종")


set.seed(123)
newdata$일정심각도 <- ifelse(newdata$일정심각도=="안전",0, ifelse(newdata$일정심각도=="경계",1,2))
newdata$금액심각도 <- ifelse(newdata$금액심각도=="안전",0, ifelse(newdata$금액심각도=="경계",1,2))

y_data <- newdata[,98]
x_data <- newdata[,c(-98,-99,-100,-101)]




x_data$프로젝트분야 <- as.factor(x_data$프로젝트분야)
x_data$플랜트종류 <- as.factor(x_data$플랜트종류)
x_data$Location <- as.factor(x_data$Location)
x_data$발주처 <- as.factor(x_data$발주처)
x_data$설계변경공종 <-as.factor(x_data$설계변경공종)


for (i in c(6:97)){
  x_data[,i] <- factor(x_data[,i])
}
# numeric으로 바꿈 -> xgb modeling 할 때 데이터 numeric으로 넣어야 해서
for (i in c(1:97)){
  x_data[,i] <- as.numeric(factor(x_data[,i]))
}


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

#학습데이터 예측&평가
train.preds <- predict(xgb_model, as.matrix(x.train), reshape = T)
train.preds <- as.data.frame(train.preds)
colnames(train.preds) <- c("안전","경계","심각")

train.preds$PredictedClass <- apply(train.preds, 1, function(y) colnames(train.preds)[which.max(y)])
train.preds$ActualClass <- c("안전","경계","심각")[y.train + 1]

#검증데이터 예측&평가
xgb_preds <- predict(xgb_model, as.matrix(x.test), reshape=TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- c("안전","경계","심각")

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- c("안전","경계","심각")[y.test + 1]


#학습데이터 정확도 
train.acc <- sum(train.preds$PredictedClass == train.preds$ActualClass) / nrow(train.preds)
train.acc
#검증데이터 정확도
auc_P <- c(auc_P,sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds))

#f1score
library(MLmetrics)
f1score_P <- c(f1score_P,F1_Score(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass)))


# 5-2 금액심각도

dataset <- read.csv("이진행렬.csv")
project_df <- as.data.frame(read_excel("data.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,6,9,12,16)],dataset[,-1])
newdata <- cbind(newdata,project_df[,c(23,26)])
newdata


names(newdata)[c(1,2,3,4,5)] = c("프로젝트분야","플랜트종류","Location","발주처","설계변경공종")


set.seed(123)
newdata$일정심각도 <- ifelse(newdata$일정심각도=="안전",0, ifelse(newdata$일정심각도=="경계",1,2))
newdata$금액심각도 <- ifelse(newdata$금액심각도=="안전",0, ifelse(newdata$금액심각도=="경계",1,2))

y_data <- newdata[,99]
x_data <- newdata[,c(-98,-99,-100,-101)]


x_data$프로젝트분야 <- as.factor(x_data$프로젝트분야)
x_data$플랜트종류 <- as.factor(x_data$플랜트종류)
x_data$Location <- as.factor(x_data$Location)
x_data$발주처 <- as.factor(x_data$발주처)
x_data$설계변경공종 <-as.factor(x_data$설계변경공종)


for (i in c(6:97)){
  x_data[,i] <- factor(x_data[,i])
}
# numeric으로 바꿈 -> xgb modeling 할 때 데이터 numeric으로 넣어야 해서
for (i in c(1:97)){
  x_data[,i] <- as.numeric(factor(x_data[,i]))
}

# train, test 데이터 분리
train <- sample(nrow(project_df), 0.8*nrow(project_df)) #훈련데이터 예측변수
x.train <- x_data[train,]
x.test <- x_data[-train,]

y.train <- y_data[train]
y.test <- y_data[-train]

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

#학습데이터 예측&평가
train.preds <- predict(xgb_model, as.matrix(x.train), reshape = T)
train.preds <- as.data.frame(train.preds)
colnames(train.preds) <- c("안전","경계","심각")

train.preds$PredictedClass <- apply(train.preds, 1, function(y) colnames(train.preds)[which.max(y)])
train.preds$ActualClass <- c("안전","경계","심각")[y.train + 1]

#검증데이터 예측&평가
xgb_preds <- predict(xgb_model, as.matrix(x.test), reshape=TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- c("안전","경계","심각")

xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- c("안전","경계","심각")[y.test + 1]


#학습데이터 정확도 
train.acc <- sum(train.preds$PredictedClass == train.preds$ActualClass) / nrow(train.preds)
train.acc
#검증데이터 정확도
auc_C <- c(auc_C,sum(xgb_preds$PredictedClass == xgb_preds$ActualClass) / nrow(xgb_preds))

#f1score
library(MLmetrics)
f1score_C <- c(f1score_C,F1_Score(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass)))


#결과(정확도, F1Score 정리) ####
names(auc_P) <- c("multinomial","RandomForest","NaiveBayes","KNN","XgBoost")
names(auc_C) <- c("multinomial","RandomForest","NaiveBayes","KNN","XgBoost")
names(f1score_P) <- c("multinomial","RandomForest","NaiveBayes","KNN","XgBoost")
names(f1score_C) <- c("multinomial","RandomForest","NaiveBayes","KNN","XgBoost")
result 
#auc_P = 일정심각도 정확도, auc_C = 금액심각도 정확도, f1score_P = 일정심각도 f1-score, f1score_C = 금액심각도 f1-score

