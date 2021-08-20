setwd("C:/Users/82104/Desktop/상아매니지먼트")


dataset <- read.csv("0819original.csv")
project_df <- as.data.frame(read_excel("codeless.xlsx",skip=5)) #데이터불러오기

newdata <- cbind(project_df[,c(5,8,9,10,14,15,16,17)],dataset[,-1])
newdata

names(newdata)[c(1,2,3,4,5,6,7,8)] = c("프로젝트분야","규모및용량","Location","총공사비","설계비용","설계기간","설계변경공종","설계변경유형")


set.seed(123)
y_data <- newdata[,103]
x_data <- newdata[,c(-101,-102,-103,-104)]


str(x_data)

# 범주형 변수 : 프로젝트 분야, 규모/용량, Location, 설계변경공종, 설계변경유형
x_data$프로젝트분야 <- as.factor(x_data$프로젝트분야)
x_data$규모및용량 <- as.factor(x_data$규모및용량)
x_data$Location <- as.factor(x_data$Location)
x_data$설계변경공종 <- as.factor(x_data$설계변경공종)
x_data$설계변경유형 <-as.factor(x_data$설계변경유형)




for (i in c(9:100)){
  x_data[,i] <- as.factor(x_data[,i])
}

# 수치형 변수: 총 공사비, 설계비용, 설계기간
summary(x_data$총공사비)
boxplot(x_data$총공사비)
hist(x_data$총공사비)

summary(x_data$설계비용)
boxplot(x_data$설계비용)
hist(x_data$설계비용)

summary(x_data$설계기간)
boxplot(x_data$설계기간)
hist(x_data$설계기간)


#정규화는 뭔가 이상해서 포기
#z.총공사비 = scale(x_data$총공사비)
#z.설계비용 = scale(x_data$설계비용)
#z.설계기간 = scale(x_data$설계기간)

#hist(z.총공사비)
#hist(z.설계비용)
#hist(z.설계기간)

# Min-Max
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

x_data$총공사비 <- normalize(x_data$총공사비)
x_data$설계비용 <- normalize(x_data$설계비용)
x_data$설계기간 <- normalize(x_data$설계기간)

hist(x_data$총공사비)
hist(x_data$설계비용)
hist(x_data$설계기간)

str(x_data)

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

# randomForest 적용
library(randomForest)
randomforest <- randomForest(y.train ~ .,data=x.train, importance=T)

#학습데이터 정확도
predtrain <- predict(randomforest, x.train, type='class') 
table(predtrain, y.train, dnn=c("Actual","Predicted")) 
mean(predtrain==y.train)

#검증데이터 정확도
predtest <- predict(randomforest, x.test, type='class')
table(predtest, y.test, dnn=c("Actual","Predicted"))
mean(predtest==y.test)
