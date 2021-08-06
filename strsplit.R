setwd("C:/Users/82104/Desktop/상아매니지먼트")


#####<데이터불러오기>###########################################################

library(readxl)

project_df <- as.data.frame(read_excel("dataset.xlsx",skip=5)) #데이터불러오기
project_xy <- project_df[,c(1,17,18,29,23,25,26,28)]  #x,y로 사용될 변수만 추출

################################################################################

#####<텍스트 나누기>############################################################

# 1. 텍스트 나누기

# 1-1. strsplit

str = strsplit(project_xy[,3],split="[[:blank:]]|[[:punct:]]|\n|\r|으로|이|이|께서|이다|다|을|를|의|에|에서|로
|와|하여|하고|됨|과|보다|아|야|와|과|하고|랑|이랑|이|이며|나|은|는|만|밖에|뿐|도|조차|마저|까지|나|이나|든지|이든지
|나마|이나마|라도|이라도|및|된|될|등|함|또는")

# 1-2. gsub

str=gsub("[[:punct:]]|x|으로|이|이|께서|이다|다|을|를|의|에|에서|로
|와|하여|하고|됨|과|보다|아|야|와|과|하고|랑|이랑|이|이며|나|은|는|만|밖에|뿐|도|조차|마저|까지|나|이나|든지|이든지
|나마|이나마|라도|이라도|및|된|될|등|함|또는|\\b\\d+\\b", "", project_xy[,3])

str = strsplit(str, split="[[:blank:]]")


# 2. "" 제거
for (i in 1:length(str)){
  for (j in length(str[[i]]):1){
    if (str[[i]][j]==""){
      str[[i]] = str[[i]][-j]
    }
  }
}

# 3. 문자별 등장 빈도수 확인
library("dplyr")
wordcount <- table(unlist(str))                           #list해체
df_word <- as.data.frame(wordcount,stringsAsFactors = F)    #df 형태로 변환, 빈도수 확인 가능
df_word <- rename(df_word,word=Var1,freq=Freq)              #column 명 변경
df_word <- filter(df_word ,nchar(word)>=2)                  #조건으로 filter, 글자수 조건
top <- df_word %>% arrange(desc(freq)) %>% head(100)     #상위 n개 결과 확인
top <- filter(top,top$freq>=10)

################################################################################

#####<이진행렬 만들기>##########################################################

binary_df <- data.frame(matrix(nrow=length(project_df),ncol=length(top$word)))
names(binary_df) <- top$word


for (i in 1:nrow(project_df)){
  for (j in top$word){
    if (j %in% str[[i]]){
      binary_df[i,j]=1
    }else {binary_df[i,j]=0}
  }
}

y_df <- project_df[,c(23,25,26,28)]

binary_df <- cbind(binary_df, y_df)

write.csv(binary_df, file="C:/Users/82104/Desktop/상아매니지먼트/binary_strsplit10.csv")

