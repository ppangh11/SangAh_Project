# 참고한 링크: https://www.youtube.com/watch?v=PvvrexFqFDU&t=292s
# ~34분까지 내용


setwd("C:/Users/82104/Desktop/상아매니지먼트")

#데이터 불러오기
library(readxl)
project_df <- as.data.frame(read_excel("dataset.xlsx",skip=5)) #데이터불러오기
project_xy <- project_df[,c(18,26)]  #x,y로 사용될 변수만 추출

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


# " " 안에 제거할 문자를 입력하면, " "으로 대체됨.
# aa/aa 인경우 /를 그냥 제거하면 aaaa가 되어서 의미가 변질될 수 있는데,
# " "로 대체함으로써 aa aa로 변형할 수 있음.
toSpace <- content_transformer(function(x, pattern){return(gsub(pattern," ",x))})
docs <- tm_map(docs, toSpace, c("[[:punct:]]"))
docs <- tm_map(docs, toSpace, c("의"))
docs <- tm_map(docs, toSpace, c("에는"))
docs <- tm_map(docs, toSpace, c("을"))
docs <- tm_map(docs, toSpace, c("에대한"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("하여"))
docs <- tm_map(docs, toSpace, c("됨"))
docs <- tm_map(docs, toSpace, c("로"))

#" " 안에 제거할 문자를 입력하면, 그냥 제거됨.(띄어쓰기 상관없이)
myRemove <- content_transformer(function(x, pattern){return(gsub(pattern, "", x))})

docs <- tm_map(docs, myRemove, c("으로")) # " " 안에 제거할 문자 입력
docs <- tm_map(docs, myRemove, c("에")) 


# 입력한 단어 삭제(띄어쓰기 되어 있는 단어만)
mystopwords <- c(c("을", "를", "가", "으로","하여","됨","등","에","않아","위해","위한","by","and","\ d")) 
docs <- tm_map(docs, removeWords, mystopwords)




# 필요하면 쓸 애들 (근데 영어에만 적용되는거 같기도 함.)
docs <- tm_map(docs, removePunctuation)
#docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, content_transformer(trimws))
docs <- tm_map(docs, stemDocument)

dtm <- DocumentTermMatrix(docs) #1060개의 데이터에서 2818개의 단어
dtm
inspect(dtm)

# 단어행렬 생성(wordLengths: 글자 수 범위, global: 빈도수 범위)
dtm <- DocumentTermMatrix(docs, control=list(wordLengths=c(2,10), bounds=list(global=c(5, 900))))
dtm # 1060개 행에서 378개의 단어

# dtm을 행렬 형태로 변환
as.matrix(dtm)

termfreq <- colSums(as.matrix(dtm))
termfreq   # 이거 실행시키면 상단에 일반 숫자들이 있는데 이거 처리만 하면 그래도 깔끔할 듯!
termfreq[head(order(termfreq, decreasing=TRUE))]
termfreq[tail(order(termfreq, decreasing=TRUE))]

