library(tidyverse)
library(stringr)
library(stringi)
library(magrittr)
library(readxl)

project_df <- as.data.frame(read_excel("C:/SANGAH/dataset.xlsx",skip=5)) 
project_xy <- project_df[,c(18,23,26)]  


data1<-project_xy
data1

glimpse(data1)

data1 <- as.data.frame(data1, stringsAsFactors = F)
data1%>%nrow

data1 <- sapply(data1, str_remove_all, '\\s+')
data1 <- as.data.frame(data1, stringsAsFactors = FALSE)
colnames(data1) <- c('content')

data_range <- data1$content %>% nchar %>% range
print(data_range)

data1$content[nchar(x = data1$content) > 1]

#id 추가
generateIDs <- function(obj, index = 'id'){
  if(obj %>% class() == 'data.frame'){
    n <- nrow(x = obj)
  }else{
    n <- length(x = obj)
  }
  
  id <- str_c(
    index,
    str_pad(
      string = 1:n,
      width = ceiling(x = log10(x = n)),
      side = 'left',
      pad = '0'))
  
  return(id)
}
data1$id <- generateIDs(obj = data1, index = 'doc')

names(data1)<-c("content","tar1","tar2", "id")

data1$tar1 <- ifelse(data1$tar1=="안전",1, ifelse(data1$tar1=="경계",2,3))
data1$tar2 <- ifelse(data1$tar2=="안전",1, ifelse(data1$tar2=="경계",2,3))

#nlp
library(NLP4kec)
#형태소 분석하여 객체 생성, 띄어쓰기 구분됨
parsed <- r_parser_r(contentVector = data1$content, language = 'ko', useEn = T)
#길이 확인
length(x = parsed)

parsed[1:10]
#글자수 확인
parsed %>% nchar() %>% table()
#중복건수 확인
duplicated(x = parsed) %>% sum()

#corpus 생성
library(tm)

corpus <- parsed %>% VectorSource() %>% VCorpus()
print(corpus)

str(object = corpus[[1]])

#N-gram
install.packages("RWeka")
library(RWeka)
##bigram 생성
bigram <- function(x){
  NGramTokenizer(x = x, control = Weka_control(min = 2, max = 2))
}
#TDM
bigramList <- corpus %>%
  TermDocumentMatrix(control = list(tokenize = bigram)) %>%
  apply(MARGIN = 1, FUN = sum) %>%
  sort(decreasing = TRUE)

#bigram길이 확인
length(bigramList)

bigramList <- bigramList[bigramList >= (nrow(x = data1) * 0.01)]
length(x = bigramList)

bigramNames <- names(bigramList)

top <- if (length(x = bigramNames) >= 100) bigramNames[1:100] else bigramNames
print(top)
##spacing.txt파일로 저장
write.table(
  x = top,
  quote = FALSE,
  file = './spacing.txt',
  fileEncoding="UTF-8",
  row.names = FALSE,
  col.names = FALSE
)
#열기 전에 spacing.txt 열어서 필요없는 단어 삭제
spacing <- read.table(file = './spacing.txt', sep = '\t')
colnames(x = spacing) <- 'before'
#####options(encoding="UTF-8")
#중복 제거
spacing <- unique(x = spacing)
#띄어쓰기 없앤 문자벡터를 after 칼럼으로 추가
spacing$after <- spacing$before %>% str_remove_all(pattern=' ')
#dictionary.txt로 저장
write.table(
  x = spacing$after,
  quote = FALSE,
  file = './dictionary.txt',
  fileEncoding="UTF-8",
  row.names = FALSE,
  col.names = FALSE
)

#형태소 분석 재실행, 사전 추가
install.packages('rJava')
library('NLP4kec')
parsed <- r_parser_r(
  contentVector = data$content,
  language = 'ko', useEn = T,
  korDicPath = './dictionary.txt'
)
length(x = parsed) #길이 확인

parsed[1:10]

#텍스트>벡터 소스로 변경
#벡터소스는 벡터의 개별 원소를 각각의 문서로 인식
corpus <- parsed %>% VectorSource() %>% VCorpus()

#사전으로 처리되지 않는 단어 띄어쓰기 강제로 변환하는 함수 생성
changeTerms <- function(corpus, before, after){
  #corpus 길이 확인
  n <- length(x = corpus)
  #반복문 실행
  for (i in 1:n){
    corpus[[i]]$content <- corpus[[i]]$content %>%
      str_replace_all(pattern = before, replacement = after)
  }
  #결과를 반환
  return(corpus)
}

#띄어쓰기 적용되지 않은 단어들 강제 적용
for (i in 1:nrow(x = spacing)){
  corpus <- changeTerms(
    corpus = corpus,
    before = spacing$before[i],
    after = spacing$after[i]
  )
}

#의심되는 단어 여전히 바뀌지 않은 채 포함되어 있는 지 확인
checkTerms <- function(corpus, term){
  corpus %>%
    sapply(FUN = '[[', 'content') %>%
    str_detect(pattern = term) %>%
    sum() %>%
    print()
}

#의심되는 단어 별로 corpus에 포함된 개수 확인
checkTerms(corpus = corpus, term ='line추가')
