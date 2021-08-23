setwd("C:/Users/82104/Desktop/상아매니지먼트")

library(readxl)


dataset <- read.csv("prob_x.csv")
project_df <- as.data.frame(read_excel("codeless.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,6,9,16,12)],dataset[,-1],project_df[,c(23,26)])
newdata

names(newdata)[c(1,2,3,4,5)] = c("프로젝트분야","플랜트종류","Location","설계변경공종","발주처")



set.seed(123)
y_data <- newdata[,19]  # 일정 심각도
y_data <- newdata[,20]  # 금액 심각도

x_data <- newdata[,c(-19,-20)]


str(x_data)

# 범주형 변수 : 프로젝트 분야, 규모/용량, Location, 설계변경공종, 설계변경유형
x_data$프로젝트분야 <- as.factor(x_data$프로젝트분야)
x_data$플랜트종류 <- as.factor(x_data$플랜트종류)
x_data$Location <- as.factor(x_data$Location)
x_data$설계변경공종 <-as.factor(x_data$설계변경공종)
x_data$발주처 <-as.factor(x_data$발주처)

# 수치형 변수: 총 공사비, 설계비용, 설계기간

# Min-Max
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}



for (i in range(6:18)){
  x_data[,i] <- normalize(x_data[,i])
}

str(x_data)





# 모델링 (랜덤포레스트)

# train, test 데이터 분리
train <- sample(nrow(project_df), 0.8*nrow(project_df)) #훈련데이터 예측변수
x.train <- x_data[train,]
x.test <- x_data[-train,]

y.train <- y_data[train]
y.test <- y_data[-train]

y.train <- factor(y.train)

## randomForest 적용

# ntree: 생성하는 나무의 수
# mtry: 각 노드 설정 시 설명변수 후보 개수(후보군)
# replace=TRUE(기본값): 복원추출 가능 여부
# importance : 변수중요도에 다라 모델을 생성. 변수가 많은 데이터는 변수중요도에 따라 error rate가 크게 변하기 때문에 T로 설정
# 다른 옵션도 있긴한데 서치해보니 보통 안 쓰는 것 같아요.


library(randomForest)


## 최적의 파라미터 찾기

# c() 안에 확인해보고 싶은 파라미터 넣으면 됩니다
param.ntree <- c(100,200,300,400,500,600,700,800,900,1000)
param.mtry <- c(10:30)

# 랜덤포레스트의 정보를 담을 벡터 생성
random.ntree <- numeric(length(param.ntree)*length(param.mtry)) # ntree
random.mtry <- numeric(length(param.ntree)*length(param.mtry)) # mtry 
random.predict <- numeric(length(param.ntree)*length(param.mtry)) # 정확도 


a=1 #벡터 인덱스를 위한 변수 a

for (i in param.ntree){
  cat('\n','ntree =',i,": ") #진행률 보고 싶으면 주석 풀기
  for(j in param.mtry){
    cat('**') # 얘도 마찬가지로 진행률
    op.randomforest <- randomForest(y.train ~ .,data=x.train, maxnodes=50, minsplit=50, importance=T, ntree=i, mtry=j, na.action= na.omit)
    
    # 생성된 모델의 정보를 벡터에 저장
    
    predtest <- predict(op.randomforest, x.test, type='class')
    
    random.ntree[a] <- op.randomforest$ntree
    random.mtry[a] <- op.randomforest$mtry
    random.predict[a] <- mean(predtest==y.test)
    
    a= a+1
  }
}



# 정확도가 최대인 랜덤포레스트 모델의 파라미터값 저장
ntree.val = random.ntree[which.max(random.predict)]
mtry.val = random.mtry[which.max(random.predict)]

## 최적화된 파라미터 값을 가진 랜덤포레스트 모델 생성
randomforest <- randomForest(y.train ~ ., ntree = ntree.val, mtry = mtry.val, data=x.train, importance=T)



? randomForest

randomforest <- randomForest(y.train ~ ., ntree = 100,maxnodes=150, minsplit=100, mtry = 11, data=x.train, importance=T)
# 다음에 할 일: maxnodes, minsplit 조정해서 과적합 해결하기

#변수 중요도를 확인할 수 있는 코드인데 나중에 보고서에 해석, 인사이트 넣을 때 도움 될 것 같아요.
importance(randomforest)
varImpPlot(randomforest, type=2,pch=19, col=1, cex=1, main="")

#모델 정보확인
str(randomforest)

#학습데이터 정확도
predtrain <- predict(randomforest, x.train, type='class') 
table(predtrain, y.train, dnn=c("Actual","Predicted")) 
mean(predtrain==y.train)

#검증데이터 정확도
predtest <- predict(randomforest, x.test, type='class')
table(predtest, y.test, dnn=c("Actual","Predicted"))
mean(predtest==y.test)

library(MLmetrics)
F1_Score(predtrain,y.train)
F1_Score(predtest, y.test)
