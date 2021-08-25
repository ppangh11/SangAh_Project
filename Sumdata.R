#Naive Bayes
#1. 데이터 불러오기
library(readxl)

cdata <- as.data.frame(read_excel("data_final.xlsx")) #데이터불러오기

cdata

library(dplyr)
set.seed(123)
cdata <- cdata[,c(-1,-99,-100,-102)]
cdata <- rename(cdata, "x.1"="금액심각도.범주." )
#cdata <- cdata[,c(-1,-100,-101,-102)]
#cdata <- rename(cdata, "x.1"="일정심각도.범주." )


#str(cdata)

# 범주형 변수 : 프로젝트 분야, 규모/용량, Location, 설계변경공종, 설계변경유형
cdata$프로젝트분야 <- as.factor(cdata$프로젝트분야)
cdata$플랜트종류 <- as.factor(cdata$플랜트종류)
cdata$Location <- as.factor(cdata$Location)
cdata$발주처 <- as.factor(cdata$발주처)
cdata$설계변경공종 <-as.factor(cdata$설계변경공종)

#str(cdata)

for (i in c(6:98)){
  cdata[,i] <- factor(cdata[,i])
}

#str(cdata)

#1-1. 데이터 확인
head(cdata)
str(cdata)
dim(cdata)

label_column = 98 #dim으로 확인 후 마지막 컬럼 번호 넣어줌. 데이터 확인
lastword_col = label_column - 1

#1-2. 일정심각도:안전,경계,심각->1,2,3 으로 대체
cdata$x.1 <- ifelse(cdata$x.1=="안전",1, ifelse(cdata$x.1=="경계",2,3))

#2. shuffle, sample, train-test
set.seed(123)
idx <- sample(x = c("train", "test"), size = nrow(cdata),
              replace=TRUE, prob = c(8,2))
cdata$idx <- idx
#2-1. train data : label, idx 제외
train <- cdata[cdata$idx=="train",][,1:lastword_col]
head(train, 3)
#2-2. train data label :label 컬럼 따로 뽑기
train_label <- cdata[cdata$idx =="train",][,label_column]
#2-3. test data
test <- cdata[cdata$idx=="test",][,1:lastword_col]
test_label<-cdata[cdata$idx=="test",][,label_column]

str(train)

#5. NaiveBayes
##install.packages('e1071')
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

#-------------
#이원교차표
#library(gmodels)
#install.packages("gmodels")
#va <- CrossTable(result, test_label)
#다시 정확도
#print(paste0(round(sum(diag(va$prop.tbl))*100, 3), '%'))

#f1score
library(MLmetrics)
F1_Score(result, test_label)

