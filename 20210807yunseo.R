# 참고한 링크: https://www.youtube.com/watch?v=PvvrexFqFDU&t=292s
# ~34분까지 내용


setwd("C:/Users/82104/Desktop/상아매니지먼트")

#데이터 불러오기
library(readxl)

project_df <- as.data.frame(read_excel("dataset.xlsx",skip=5)) #데이터불러오기
project_xy <- project_df[,c(18,23)]  #일정심각도 
#project_xy <- project_df[,c(18,26)]  #비용심각도


# 일정심각도 비율
# table(project_xy$일정심각도)
# prop.table(table(project_xy$일정심각도))

# 금액심각도 비율
table(project_xy[,2])
prop.table(table(project_xy$금액심각도))

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

# 중요한 단어를 잘라내기 위해 양쪽에 띄어쓰기 삽입: ex)"변경" -> " 변경 "
plusSpace <- content_transformer(function(x, pattern){return(gsub(pattern,paste(" ",pattern," ",sep=""),x))})
docs <- tm_map(docs, plusSpace, c("변경"))
docs <- tm_map(docs, plusSpace, c("발생"))
docs <- tm_map(docs, plusSpace, c("추가"))
docs <- tm_map(docs, plusSpace, c("사업주"))
docs <- tm_map(docs, plusSpace, c("계약"))
docs <- tm_map(docs, plusSpace, c("위치"))
docs <- tm_map(docs, plusSpace, c("변동"))
docs <- tm_map(docs, plusSpace, c("u/g"))
docs <- tm_map(docs, plusSpace, c("p&id"))
docs <- tm_map(docs, plusSpace, c("발주처"))
docs <- tm_map(docs, plusSpace, c("재설계"))
docs <- tm_map(docs, plusSpace, c("부족"))
docs <- tm_map(docs, plusSpace, c("요청"))
docs <- tm_map(docs, plusSpace, c("증가"))
docs <- tm_map(docs, plusSpace, c("설치"))
docs <- tm_map(docs, plusSpace, c("토목"))
docs <- tm_map(docs, plusSpace, c("펌프"))
docs <- tm_map(docs, plusSpace, c("pump"))  # 나중에 하나로 통일




lapply(docs, content)


# " " 안에 제거할 문자를 입력하면, " "으로 대체됨.
# aa/aa 인경우 /를 그냥 제거하면 aaaa가 되어서 의미가 변질될 수 있는데,
# " "로 대체함으로써 aa aa로 변형할 수 있음.
toSpace <- content_transformer(function(x, pattern){return(gsub(pattern," ",x))})
#docs <- tm_map(docs, toSpace, c("[[:punct:]]"))
docs <- tm_map(docs, toSpace, c("의"))
docs <- tm_map(docs, toSpace, c("에는"))
docs <- tm_map(docs, toSpace, c("을"))
docs <- tm_map(docs, toSpace, c("에대한"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("하여"))
docs <- tm_map(docs, toSpace, c("됨"))
docs <- tm_map(docs, toSpace, c("로"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("따라"))
docs <- tm_map(docs, toSpace, c("따른"))

#" " 안에 제거할 문자를 입력하면, 그냥 제거됨.(띄어쓰기 상관없이)
myRemove <- content_transformer(function(x, pattern){return(gsub(pattern, "", x))})

docs <- tm_map(docs, myRemove, c("으로")) # " " 안에 제거할 문자 입력


docs <- tm_map(docs, stripWhitespace) #공백제거
#docs <- tm_map(docs, myRemove, c("\ d"))


# 입력한 단어 삭제(띄어쓰기 되어 있는 단어만)
mystopwords <- c(c("변경", "되는","을", "를","하여","됨","등","에","위해","위한","by","and")) 
docs <- tm_map(docs, removeWords, mystopwords)




# 필요하면 쓸 애들 (근데 영어에만 적용되는거 같기도 함.)
#docs <- tm_map(docs, removePunctuation)
#docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(trimws))
#docs <- tm_map(docs, stemDocument)

dtm <- DocumentTermMatrix(docs) #1060개의 데이터에서 2818개의 단어

dtm
inspect(dtm)

# 단어행렬 생성(wordLengths: 글자 수 범위, global: 빈도수 범위)
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(2,10), bounds=list(global=c(10, 900))))
dtm # 1060개 행에서 378개의 단어

# dtm을 행렬 형태로 변환

termfreq <- colSums(as.matrix(dtm))
termfreq   # 이거 실행시키면 상단에 일반 숫자들이 있는데 이거 처리만 하면 그래도 깔끔할 듯!
termfreq[head(order(termfreq, decreasing=TRUE))]
termfreq[tail(order(termfreq, decreasing=TRUE))]

# 지정된 최소 출현 빈도 이상 등장한 단어를 찾아줌줌
findFreqTerms(dtm, lowfreq=200)

# 상관관계가 있는 단어를 찾아줌.
findAssocs(dtm, c("변경","line","추가","size","locat","dwg"),c(0.20,0.25))


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
  x <- factor(x, level=c(0,1), labels=c("no","yes"))
  return(x)
}

project_xy.dtm <- apply(dtm, MARGIN=2, toFactor)
project_xy.dtm

x.train <- project_xy.dtm[train,]
x.test <- project_xy.dtm[-train,]

library(e1071)
project.nb <- naiveBayes(x=x.train, y=y.train)
project.nb.pred <- predict(project.nb, newdata=x.test)
head(project.nb.pred)
head(y.test)

table(y.train,predict(project.nb, newdata=x.train), dnn=c("Actual","Predicted"))
table(y.test, project.nb.pred, dnn=c("Actual","Predicted"))

mean(predict(project.nb, newdata=x.train)==y.train) #학습데이터 정확도
mean(project.nb.pred==y.test) #검증데이터 정확도

