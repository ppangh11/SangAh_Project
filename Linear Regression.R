---
title: "Linear Regression"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

1. traindata, testdata 분할

```{r}
data1 <- read.csv("20210810.csv")[,-1]
data<-read.csv("binary_df.csv")[,44]
data <- cbind(data1,data)

train <- sample(nrow(data), 0.8*nrow(data)) #랜덤으로 숫자배열
train_dat <- data[train,] # traindata
x.train <- train_dat[,c(1:88)] #traindata(독립변수)
num.y.train <- train_dat[,90] #traindata(수치형종속변수)
cat.y.train <- train_dat[,89] #traindata(범주형종속변수)

test_dat <- data[-train,] #testdata
x.test <- test_dat[,c(1:88)] #testdata(종속변수없는거)
```


2. 일정심각도의 선형회귀모델 적합(AIC 기준, 변수선택법(forward, backward, both) 선택해서 direction = 자리에 넣으면 됩니다) 

```{r}
library(olsrr)
df_period <- lm(num.y.train~.,data=x.train)
period_AIC <- stepAIC(df_period,direction = "forward")
AIC_model <- period_AIC$model
df_AIC <- lm(AIC_model[,1]~.,data=AIC_model[,-1])
pred.AIC <- predict(df_AIC,test_dat)
```

3. 예측한 수치를 범주로 나눔

```{r}
i=0
a<- c()
for(i in c(1:212)){
  if(pred.AIC[i] < 0.01 ){
    print("안전")
    a=c(a,"안전")
  }else{
    if(pred.AIC[i] < 0.02 ){
      print("경계")
      a=c(a,"경계")
    }else{
      print("심각")
      a=c(a,"심각")
    }
  }
}  #위 for문에서 변수명 변경되면 바꿔줘야함
``` 

4. 오분류표로 정확도 확인

```{r}
confusionMatrix(as.factor(a),as.factor(test_dat[,89])) #데이터 바뀌면 col 확인해서 col번호 바꿔줘야함
```
