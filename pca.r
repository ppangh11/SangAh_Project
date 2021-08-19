# 참고 링크: https://rfriend.tistory.com/61

setwd("C:/Users/82104/Desktop/상아매니지먼트")


dataset <- read.csv("0819original.csv")

library(HSAUR)
head(dataset)
summary(dataset)

# 예측, 종속 변수 할당
y_data <- dataset[,94]
x_data <- dataset[,c(-1,-94,-95,-96,-97)]

var(x_data)

dataset.pca <- prcomp(x_data, scale. =T)
dataset.pca



summary(dataset.pca) # 이거 실행하면 표준편차, 분산비율, 분산 누적 합계 확인 가능

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


#pca 시각화

#biplot(dataset.pca, main="Biplot")

#install_github("devtools")
#library(devtools)
#install_github("ggbiplot", "vqv")
#library(ggbiplot)

round(predict(dataset.pca), 3)
dataset.pca.df <- predict(dataset.pca)[,1:30]   # 위에서 자르라는 기준이 있긴했는데, 잘 모르것어서 그냥 제 멋대로 30에서 잘랐어요

dataset.pca.df

#set.seed(123)



train <- sample(nrow(dataset), 0.8*nrow(dataset)) #훈련데이터 예측변수




y.train <- y_data[train]  #훈련데이터용 결과변수
y.test <- y_data [-train] #검정데이터용 결과변수

y.train.order <- factor(y.train, levels=c("안전","경계","심각"), ordered=TRUE)
y.test.order <- factor(y.test, levels=c("안전","경계","심각"), ordered=TRUE)

table(y.train)
table(y.test)

y.train

prop.table(table(y.train))
prop.table(table(y.test))

x.train <- dataset.pca.df[train,]
x.test <- dataset.pca.df[-train,]


#####<<무시>>##################################################################################################################

# 서수 로지스틱 회귀분석
# 뭔가 좀 많이 이상해서 여긴 빼고 봐주세요
# 참조 링크: https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)
oridinal_logit_m <- polr(y.train.order ~ .,data=x.train,  Hess=TRUE)
summary(oridinal_logit_m)


orid.logit.pred <- predict(oridinal_logit_m, newdata=x.test)

table(y.train, predict(oridinal_logit_m, newdata=x.train), dnn=c("Actual","Predicted"))
table(y.test, orid.logit.pred, dnn=c("Actual","Predicted"))

mean(predict(oridinal_logit_m, newdata=x.train)== y.train) #학습데이터 정확도
mean(orid.logit.pred==y.test) #검증데이터 정확도

head(round(fitted(oridinal_logit_m), 2)) # 각 범주별 확률값

###########################################################################################################################
set.seed(NULL)
# knn
library(class)
library(gmodels)
#set.seed(123)
knn <- knn(train = x.train, test = x.test, cl = y.train, k = 3)
knn

#CrossTable(x=y.test, y = knn, prob.chisq=FALSE)

knn.order <-  factor(knn, )
accuracy_1 <- sum(knn == y.test) / length(y.test)
accuracy_1

library(caret)
library(e1071)
confusionMatrix(as.factor(knn), as.factor(y.test))

# k 값에 따른 분류 정확도 확인
accuracy_k <- NULL

for (kk in c(1:51)){
  #set.seed(123)
  knn_k <- knn(train = x.train, test=x.test, cl=y.train, k = kk)
  accuracy_k <- c(accuracy_k, sum(knn_k == y.test)/length(y.test))
}

vaild_k <- data.frame(k=c(1:51), accuracy= accuracy_k)

plot(formula = accuracy ~ k, data = vaild_k, type="o", pch=20, main= "validation - optimal k")

vaild_k[1:30,]

prop.table(table(y.train))
prop.table(table(y.test))


# csv 파일 만들기
#makecsv <- dataset.pca.df
#makecsv <- cbind(makecsv, dataset[,c(94,95,96,97)])
#makecsv
#write.csv(makecsv,file="C:/Users/82104/Desktop/상아매니지먼트/0819주성분분석30.csv")
