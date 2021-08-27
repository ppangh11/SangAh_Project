#데이터불러오기 ####

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

# 변수 factor 화 ####
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



for (i in c(6:97)){
  x.train[,i] <- as.factor(x.train[,i])
  x.test[,i] <- as.factor(x.test[,i])
}

#모델링


# 1 다항회귀모델 ####

# 1-1 일정심각도

#install.packages("nnet")
library(nnet)
model_multi1 = multinom(y.train.period ~ ., data = x.train) # 다항회귀 모델 적합

multi_trainP = predict(model_multi1, newdata = x.train, "class") # 훈련모델로 훈련데이터 예측
tab = table(y.train.period, multi_trainP)
round((sum(diag(tab))/sum(tab))*100,2) # 훈련데이터 정확도 확인

multi_testP = predict(model_multi1, newdata = x.test, "class") # 훈련모델로 검증데이터 예측
tab_test = table(y.test.period, multi_testP) 
auc_P <- c(round((sum(diag(tab_test))/sum(tab_test))*100,2)) # 검증데이터 정확도 확인


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
auc_C <- c(round((sum(diag(tab_test))/sum(tab_test))*100,2)) # 검증데이터 정확도 확인


#install.packages("MLmetrics")
library(MLmetrics)
f1score_C <- c(F1_Score(multi_testC,as.factor(y.test.cost)))




# 2 랜덤포레스트 ####

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
auc_P <- c(auc_P,mean(RF_testP==y.test.period)*100) # 검증데이터정확도

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
auc_C <- c(auc_C,mean(RF_testC==y.test.cost)*100) #검증데이터 정확도

library(MLmetrics)
f1score_C <- c(f1score_C,F1_Score(RF_testC, y.test.cost)) #f1score

