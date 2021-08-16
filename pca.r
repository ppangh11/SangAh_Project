# 참고 링크: https://rfriend.tistory.com/61

setwd("C:/Users/82104/Desktop/상아매니지먼트")


dataset <- read.csv("20210811.csv")

library(HSAUR)
head(dataset)
summary(dataset)

y_data <- dataset[,86]
x_data <- dataset[,c(-1,-86)]

var(x_data)

dataset.pca <- prcomp(x_data, scale. =T)
dataset.pca

summary(dataset.pca)
# standard deviation : 표준편차
# proportion of variance : 분산비율, 각 주성분의 차지하는 비율, 클 수록 영향도 높음
# cumulative proportion : 분산 누적 합계

# 주성분 개수 결정 기준
# 1. 총 변이에 대한 공헌도: cumulative proportion이 80%가 넘어가는 시점의 것을 통상적으로 사용
# 2. 개별 고유값의 크기: standard deviation이 1이상인 경우를 사용.(kaiser 기준)
#                        jolloffe는 1대신 0.7을 제안
# 3.스크리 그림(stree plot)을 통한 판단: 주성분(x)과 대응하는 고유값(y)을 나타낸 차트
#                                       그래프가 완만해 지는 부분이전까지만 활용
#screeplot(dataset.pca, type="lines", pch=1, main="scree plot", npcs=40)

# => original.csv : 46

#pca 시각화

#biplot(dataset.pca, main="Biplot")

#install_github("devtools")
#library(devtools)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)

round(predict(dataset.pca), 3)
dataset.pca.df <- predict(dataset.pca)[,1:20]

dataset.pca.df

set.seed(123)



train <- sample(nrow(dataset), 0.8*nrow(dataset)) #훈련데이터 예측변수




y.train <- y_data[train]  #훈련데이터용 결과변수
y.test <- y_data [-train] #검정데이터용 결과변수
table(y.train)
table(y.test)


prop.table(table(y.train))
prop.table(table(y.test))

x.train <- dataset.pca.df[train,]
x.test <- dataset.pca.df[-train,]

library(class)
set.seed(123)
knn <- knn(train = x.train, test = x.test, cl = y.train, k = 3)

accuracy_1 <- sum(knn == y.test) / length(y.test)
accuracy_1

library(caret)
library(e1071)
confusionMatrix(as.factor(knn), as.factor(y.test))

# k 값에 따른 분류 정확도 확인
accuracy_k <- NULL

for (kk in c(1:51)){
  set.seed(123)
  knn_k <- knn(train = x.train, test=x.test, cl=y.train, k = kk)
  accuracy_k <- c(accuracy_k, sum(knn_k == y.test)/length(y.test))
}

vaild_k <- data.frame(k=c(1:51), accuracy= accuracy_k)

plot(formula = accuracy ~ k, data = vaild_k, type="o", pch=20, main= "validation - optimal k")

vaild_k[1:10,]

