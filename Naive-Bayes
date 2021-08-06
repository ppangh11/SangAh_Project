#1. 데이터 불러오기
cdata <- read.csv("binary_df1.csv",
                  header = T, stringsAsFactors = F)
#1-1. 데이터 확인
head(cdata)
str(cdata)
dim(cdata)

#1-2. 일정심각도:안전,경계,심각->1,2,3 으로 대체
cdata$일정심각도 <- ifelse(cdata$일정심각도=="안전",1, ifelse(cdata$일정심각도=="경계",2,3))

#1-2. 금액심각도:안전,경계,심각->1,2,3 으로 대체
cdata$금액심각도 <- ifelse(cdata$금액심각도=="안전",1, ifelse(cdata$금액심각도=="경계",2,3))

#2. shuffle, sample, train-test
set.seed(12345)
idx <- sample(x = c("train", "test"), size = nrow(cdata),
              replace=TRUE, prob = c(8,2))
cdata$idx <- idx
#2-1. train data : label, idx 제외
train <- cdata[cdata$idx=="train",][,1:35]
head(train, 3)
#2-2. train data label :label 컬럼 따로 뽑기
train_label1 <- cdata[cdata$idx =="train",][,36] #일정
train_label1 <- cdata[cdata$idx =="train",][,38] #금액
#2-3. test data
test <- cdata[cdata$idx=="test",][,1:35]
test_label<-cdata[cdata$idx=="test",][,36] #일정
test_label<-cdata[cdata$idx=="test",][,38] #금액


#3. 정규화 진행
normalize<-function(x){
  return( (x-min(x))/( max(x)-min(x)))
}
train<-as.data.frame(lapply(train[,2:35], normalize))
test<-as.data.frame(lapply(test[,2:35], normalize))

#4. 필요 데이터 factor로 변경 >naivebayes 사용 위함
for (i in (1:ncol(train))){
  train[,i]<-factor(train[,i])
}
for (i in (1:ncol(test))){
  test[,i]<-factor(test[,i])
}
## factor로 변경됐는지 확인
train_label <- factor(train_label1)
str(train)

#5. NaiveBayes
install.packages('e1071')
library(e1071)
#5-1. 모델생성
model <-naiveBayes(train, train_label, laplace = 1)
#5-2. 예상값 출력
result <- predict(model, test)
#5-3. 예상값과 실제값 비교
head(data.frame(test_label, result), 10)
#5-4. 동일여부 파악 위해 예상값(result) factor에서 numeric으로 변환
result <- as.numeric(result)
#5-5. 같으면 'O', 다르면 'X'
prop.table(table(ifelse(test_label == result, 'O','X')))
