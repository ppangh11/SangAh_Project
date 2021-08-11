data1 <- read.csv("20210811.csv")[,-1]
data<-read.csv("binary_df.csv")[,44]
data <- cbind(data1,data)

train <- sample(nrow(data), 0.8*nrow(data)) #랜덤으로 숫자배열
train_dat <- data[train,] # traindata
x.train <- train_dat[,c(1:84)] #traindata(독립변수)
num.y.train <- train_dat[,86] #traindata(수치형종속변수)
cat.y.train <- train_dat[,85] #traindata(범주형종속변수)

test_dat <- data[-train,] #testdata
x.test <- test_dat[,c(1:84)] #testdata(종속변수없는거)


model.svm <- svm(num.y.train~.,data=x.train) #SVM 모델 생성
svm.p <- predict(model.svm,x.test) #모델로 수치예측

i=0
a<- c()
for(i in c(1:212)){
  if(svm.p[i] < 0.01 ){
    print("안전")
    a=c(a,"안전")
  }else{
    if(svm.p[i] < 0.02 ){
      print("경계")
      a=c(a,"경계")
    }else{
      print("심각")
      a=c(a,"심각")
    }
  }
} #예측한 수치 범주형변수로 바꾸기

confusionMatrix(as.factor(a),as.factor(test_dat[,85])) #오분류표 확인
