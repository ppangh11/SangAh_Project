# 참고한 링크: https://www.youtube.com/watch?v=PvvrexFqFDU&t=292s
# ~34분까지 내용


setwd("C:/Users/82104/Desktop/상아매니지먼트")

#데이터 불러오기
library(readxl)

project_df <- as.data.frame(read_excel("codeless.xlsx",skip=5)) #데이터불러오기
project_xy <- project_df[,c(18,23)]  #일정심각도 
#project_xy <- project_df[,c(18,26)]  #비용심각도


# 일정심각도 비율
# table(project_xy$일정심각도)
# prop.table(table(project_xy$일정심각도))

# 금액심각도 비율
#table(project_xy[,2])
#prop.table(table(project_xy$금액심각도))

names(project_xy)<-c("text","type")

# DataframeSource 함수는 text, id 열이 필요함.
# 그 형태로 만드는 과정
library(dplyr)
library(tibble)

project_xy <- project_xy %>%
  select(text, type) %>%
  add_column(doc_id=1:nrow(.), .before=1)

library(tm)
docs <- VCorpus(DataframeSource(project_xy))
docs   # 1060개의 자료를 가진 corpus 객체

lapply(docs, content)



#meta(docs)

# 모두 소문자로 바꾸는 작업
docs <- tm_map(docs, content_transformer(tolower))

# 특정 단어를 다른 형태로 바꾸는 작업
changeWord <- content_transformer(function(x, pattern, changepattern){return(gsub(pattern, changepattern, x))})

#원본데이터에서 나온 단어 처리
changeword_df <- as.data.frame(read_excel("단어변경.xlsx")) #데이터불러오기

i <- 1

for (i in nrow(changeword_df)){
  docs <- tm_map(docs, changeWord, changeword_df[i,1], changeword_df[i,2])
}

#플랜트 용어 사전 정리한 단어 처리
#change <- read.csv("변경단어.csv")
#as.vector(change)

#i <- 1
#for (i in dim(change)[1]){
#  docs <- tm_map(docs,changeWord,change[i,1],change[i,3])
#}


# 중요한 단어를 잘라내기 위해 양쪽에 띄어쓰기 삽입: ex)"변경" -> " 변경 "
plusSpace <- content_transformer(function(x, pattern){return(gsub(pattern,paste(" ",pattern," ",sep=""),x))})

# 원본데이터에서 나온 단어 처리

spaceword_df <- as.data.frame(read_excel("띄어쓰기.xlsx")) #데이터불러오기

i <- 1
for (i in nrow(changeword_df)){
  docs <- tm_map(docs, plusSpace, spaceword_df[i,1])
}





# reb 어쩌지 => 근데 얘네끼리 별 차이 없어보임 (거의 다 안전으로 분류, 그냥 하나로 합칠까요?)
# 1)
#docs <- tm_map(docs, plusSpace, c("rev0"))
#docs <- tm_map(docs, plusSpace, c("revc"))
#docs <- tm_map(docs, plusSpace, c("rev1"))
#docs <- tm_map(docs, plusSpace, c("revb"))

# 2)
#docs <- tm_map(docs, plusSpace, c("revision"))



lapply(docs, content)


# " " 안에 제거할 문자를 입력하면, " "으로 대체됨.
# aa/aa 인경우 /를 그냥 제거하면 aaaa가 되어서 의미가 변질될 수 있는데,
# " "로 대체함으로써 aa aa로 변형할 수 있음.
toSpace <- content_transformer(function(x, pattern){return(gsub(pattern," ",x))})
#docs <- tm_map(docs, toSpace, c("[[:punct:]]"))
docs <- tm_map(docs, toSpace, c(","))
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
#docs <- tm_map(docs, myRemove, c("\ d"))


# 위에 단어 나누는 거는 이제 왠만한거는 다 잘 잘리는 것 같고, 이제 이 아래 코드에서 단어 이것저것 빼보면서 확인하고, 
# 아니면 주성분분석(PCA) 이용해서 차원 축소하는 방법 적용해 봐야 할 것 같아요
# 예를 들어 "변경"이라는 단어가 다른 단어들이랑 상관관계가 높아서 한 번 빼봤는데 성능이 조금 올라가더라고요.
# 이런애들이 정말 많을 것 같은데,, 일일히 확인하면서 빼는 방법 밖에 없겠죠...?



# 입력한 단어 삭제(띄어쓰기 되어 있는 단어만)
mystopwords <- c(c("b/0","by","for","and","of","to","with")) 
docs <- tm_map(docs, removeWords, mystopwords)

# "변경","발생","증가"


# 필요하면 쓸 애들 
#docs <- tm_map(docs, removePunctuation) # 구두점 제거
#docs <- tm_map(docs, removeNumbers)   # 숫자 제거
docs <- tm_map(docs, stripWhitespace)  # 공백 2개 제거
docs <- tm_map(docs, content_transformer(trimws))  # 요건 뭘까요?
#docs <- tm_map(docs, stemDocument)   # 요것도 뭘까요?

dtm <- DocumentTermMatrix(docs)

dtm
inspect(dtm)

# 단어행렬 생성(wordLengths: 글자 수 범위, global: 빈도수 범위)
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(2,10), bounds=list(global=c(10, 900))))
dtm # 1060개 행에서 378개의 단어


# dtm을 행렬 형태로 변환
termfreq <- colSums(as.matrix(dtm))
termfreq  
termfreq[head(order(termfreq, decreasing=TRUE))]
termfreq[tail(order(termfreq, decreasing=TRUE))]


# 지정된 최소 출현 빈도 이상 등장한 단어를 찾아줌줌
findFreqTerms(dtm, lowfreq=200)

# 상관관계가 있는 단어를 찾아줌.
#findAssocs(dtm, c("12cs1s01","12cg0t01","1fe0w01","31a10","31a50","aa22","ar5w"),c(0.20,0.25))


#워드 클라우드
library(wordcloud)
library(RColorBrewer)
set.seed(123)
#windows(width=7, height=7)
#wordcloud(words=names(termfreq), freq=termfreq, scale=c(4, 0.5) ,min.freq=0, max_words=200, rot.per=0, random.order=FALSE, random.color= FALSE, colors=brewer.pal(6, "Set2"))

# y의 범주별로 워드 클라우드를 만들기
# 행은 레코드, 열은 단어, value는 빈도수인 행렬 형태로 만들어야 함.
hamproject <- as.matrix(dtm)


rownames(hamproject) <- project_xy$type
hamproject <- rowsum(hamproject, group=rownames(hamproject)) #범주별로 단어가 얼마나 등장하는지
hamproject

set.seed(123)

windows(width=7, height=7)
comparison.cloud(t(hamproject), colors=c("cornflowerblue","tomato","black"),title.size=2, title.colors=c("blue", "red","black"), title.bg.colors="wheat",rot.per=0, scale=c(5,0.4),max.words=200)


#나이브베이즈: 범주형 예측변수
inspect(dtm)

#project_xy$type

set.seed(123)
train <- sample(nrow(project_xy), 0.8*nrow(project_xy)) #훈련데이터 예측변수
y.train <- project_xy[train,]$type #훈련데이터용 결과변수
y.test <- project_xy[-train,]$type #검정데이터용 결과변수
table(y.train)
table(y.test)

prop.table(table(y.train))
prop.table(table(y.test))

#예측변수를 범주형 변수로 만드는 함수
toFactor <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, level=c(0,1))
  return(x)
}

project_xy.dtm <- apply(dtm, MARGIN=2, toFactor)
project_xy.dtm

x.train <- project_xy.dtm[train,]
x.test <- project_xy.dtm[-train,]

#x.train <- makecsv[train,]
#x.test <- makecsv[-train,]

library(e1071)
project.nb <- naiveBayes(x=x.train, y=y.train)
project.nb.pred <- predict(project.nb, newdata=x.test)
head(project.nb.pred)
head(y.test)

table(y.train,predict(project.nb, newdata=x.train), dnn=c("Actual","Predicted"))
table(y.test, project.nb.pred, dnn=c("Actual","Predicted"))

mean(predict(project.nb, newdata=x.train)==y.train) #학습데이터 정확도
mean(project.nb.pred==y.test) #검증데이터 정확도



# 잘못 분류된 텍스트 확인하는 코드
#text <- lapply(docs, content)
#text[(project.nb.pred=="안전")&(y.test=="심각")]

#csv 만들기
#makecsv <- project_xy.dtm[,c(-2,-3,-4,-5)]
#makecsv
#binary_df <- cbind(makecsv, project_xy$type)
#write.csv(binary_df,file="C:/Users/82104/Desktop/상아매니지먼트/original.csv")

