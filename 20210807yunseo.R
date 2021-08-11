# 참고한 링크: https://www.youtube.com/watch?v=PvvrexFqFDU&t=292s
# ~34분까지 내용


setwd("C:/Users/82104/Desktop/상아매니지먼트")

#데이터 불러오기
library(readxl)

project_df <- as.data.frame(read_excel("dataset.xlsx",skip=5)) #데이터불러오기
#project_xy <- project_df[,c(18,23)]  #일정심각도 
project_xy <- project_df[,c(18,26)]  #비용심각도


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


docs <- tm_map(docs,changeWord,"spectacle"," spec ")



docs <- tm_map(docs,changeWord,"이동","변경")
docs <- tm_map(docs,changeWord,"change","변경")
docs <- tm_map(docs,changeWord,"conn.","conn")
docs <- tm_map(docs,changeWord,"lines","line")
docs <- tm_map(docs,changeWord,"location","위치")
docs <- tm_map(docs,changeWord,"plot plan","PlotPlan")  ##
docs <- tm_map(docs,changeWord,"sheets","sheet")
docs <- tm_map(docs,changeWord,"u/g","ug")
docs <- tm_map(docs,changeWord,"vendor print","v/p")
docs <- tm_map(docs,changeWord,"vp","v/p")
docs <- tm_map(docs,changeWord,"라인","line")
docs <- tm_map(docs,changeWord,"요구","proposal")
docs <- tm_map(docs,changeWord,"requirement","proposal")

docs <- tm_map(docs,changeWord,"요청","proposal")
docs <- tm_map(docs,changeWord,"펌프","pump")

docs <- tm_map(docs,changeWord,"dewatering","dwg")
docs <- tm_map(docs,changeWord,"ewatering","dewatering")

docs <- tm_map(docs,changeWord,"mh","m/h")
docs <- tm_map(docs,changeWord,"equipment","equip")
docs <- tm_map(docs,changeWord,"stedccture","structure")
docs <- tm_map(docs,changeWord,"specification"," spec ")


docs <- tm_map(docs,changeWord,"design","디자인")
docs <- tm_map(docs,changeWord,"pipe rack","PipeRack")
docs <- tm_map(docs,changeWord,"connect","CONNECT")
docs <- tm_map(docs,changeWord,"connecting","CONNECT")
docs <- tm_map(docs,changeWord,"connection","CONNECT")
docs <- tm_map(docs,changeWord,"conn","CONNECT")
docs <- tm_map(docs,changeWord,"funner","funnel")



# 화살표 처리
#docs <- tm_map(docs,changeWord,"--->","증가")

# rev 처리 2)
docs <- tm_map(docs,changeWord,"review","RV")
docs <- tm_map(docs,changeWord,"rev"," revision ")
docs <- tm_map(docs,changeWord,"rev0"," revision ")
docs <- tm_map(docs,changeWord,"rev1"," revision ")
docs <- tm_map(docs,changeWord,"revc"," revision ")
docs <- tm_map(docs,changeWord,"revb"," revision ")

#spec, special 처리 : spec 종류별로 분리
#docs <- tm_map(docs,changeWord,"spectacle","spec")
#docs <- tm_map(docs,changeWord,"spec","spec")


# 중요한 단어를 잘라내기 위해 양쪽에 띄어쓰기 삽입: ex)"변경" -> " 변경 "
plusSpace <- content_transformer(function(x, pattern){return(gsub(pattern,paste(" ",pattern," ",sep=""),x))})
#docs <- tm_map(docs, plusSpace, c("ballast"))
docs <- tm_map(docs, plusSpace, c("3d"))
docs <- tm_map(docs, plusSpace, c("acceleratino"))
docs <- tm_map(docs, plusSpace, c("air"))
docs <- tm_map(docs, plusSpace, c("analzer"))
docs <- tm_map(docs, plusSpace, c("barite"))
docs <- tm_map(docs, plusSpace, c("bentonite"))
docs <- tm_map(docs, plusSpace, c("bop"))
docs <- tm_map(docs, plusSpace, c("bracing"))
docs <- tm_map(docs, plusSpace, c("branch"))
docs <- tm_map(docs, plusSpace, c("boiler"))

docs <- tm_map(docs, plusSpace, c("burner"))
docs <- tm_map(docs, plusSpace, c("c/v"))
docs <- tm_map(docs, plusSpace, c("cable"))
docs <- tm_map(docs, plusSpace, c("cement"))
docs <- tm_map(docs, plusSpace, c("chang"))
docs <- tm_map(docs, plusSpace, c("check"))
docs <- tm_map(docs, plusSpace, c("class"))
docs <- tm_map(docs, plusSpace, c("conn"))
docs <- tm_map(docs, plusSpace, c("CONNECT"))
docs <- tm_map(docs, plusSpace, c("cost"))
docs <- tm_map(docs, plusSpace, c("compressor"))

docs <- tm_map(docs, plusSpace, c("deck"))
docs <- tm_map(docs, plusSpace, c("design"))
docs <- tm_map(docs, plusSpace, c("dewatering"))
docs <- tm_map(docs, plusSpace, c("diverter"))
docs <- tm_map(docs, plusSpace, c("drain"))
docs <- tm_map(docs, plusSpace, c("dwg"))
docs <- tm_map(docs, plusSpace, c("edg"))
docs <- tm_map(docs, plusSpace, c("elev"))
docs <- tm_map(docs, plusSpace, c("epdm"))
docs <- tm_map(docs, plusSpace, c("equip"))
docs <- tm_map(docs, plusSpace, c("feed"))
docs <- tm_map(docs, plusSpace, c("filter"))
docs <- tm_map(docs, plusSpace, c("flare"))
docs <- tm_map(docs, plusSpace, c("flare"))
docs <- tm_map(docs, plusSpace, c("funnel"))
docs <- tm_map(docs, plusSpace, c("gate"))
docs <- tm_map(docs, plusSpace, c("globe"))
docs <- tm_map(docs, plusSpace, c("gre"))
docs <- tm_map(docs, plusSpace, c("handling"))
docs <- tm_map(docs, plusSpace, c("hb"))
docs <- tm_map(docs, plusSpace, c("ibc"))
docs <- tm_map(docs, plusSpace, c("itb"))

docs <- tm_map(docs, plusSpace, c("ifc"))
docs <- tm_map(docs, plusSpace, c("line"))
docs <- tm_map(docs, plusSpace, c("liquids"))
docs <- tm_map(docs, plusSpace, c("loop"))
docs <- tm_map(docs, plusSpace, c("m/h"))
docs <- tm_map(docs, plusSpace, c("manifold"))
docs <- tm_map(docs, plusSpace, c("model"))
docs <- tm_map(docs, plusSpace, c("mud"))
docs <- tm_map(docs, plusSpace, c("nozzle"))
docs <- tm_map(docs, plusSpace, c("p&id"))
docs <- tm_map(docs, plusSpace, c("p/f"))
docs <- tm_map(docs, plusSpace, c("pass"))
docs <- tm_map(docs, plusSpace, c("pbr"))
docs <- tm_map(docs, plusSpace, c("pc"))
docs <- tm_map(docs, plusSpace, c("PipeRack"))
docs <- tm_map(docs, plusSpace, c("PlotPlan"))
docs <- tm_map(docs, plusSpace, c("point"))
docs <- tm_map(docs, plusSpace, c("ppb"))
docs <- tm_map(docs, plusSpace, c("project"))
docs <- tm_map(docs, plusSpace, c("proposal"))
docs <- tm_map(docs, plusSpace, c("psv"))
docs <- tm_map(docs, plusSpace, c("p-tank"))
docs <- tm_map(docs, plusSpace, c("pump"))
docs <- tm_map(docs, plusSpace, c("quantity"))
docs <- tm_map(docs, plusSpace, c("revamping"))
docs <- tm_map(docs, plusSpace, c("review"))
docs <- tm_map(docs, plusSpace, c("route"))
docs <- tm_map(docs, plusSpace, c("rtrp"))
docs <- tm_map(docs, plusSpace, c("schedule"))
docs <- tm_map(docs, plusSpace, c("sheet"))
docs <- tm_map(docs, plusSpace, c("size"))
docs <- tm_map(docs, plusSpace, c("special"))
docs <- tm_map(docs, plusSpace, c("stair"))
docs <- tm_map(docs, plusSpace, c("stbd"))
docs <- tm_map(docs, plusSpace, c("steel"))
docs <- tm_map(docs, plusSpace, c("strainer"))
docs <- tm_map(docs, plusSpace, c("structure"))
docs <- tm_map(docs, plusSpace, c("study"))
docs <- tm_map(docs, plusSpace, c("support"))
docs <- tm_map(docs, plusSpace, c("tie-in"))
docs <- tm_map(docs, plusSpace, c("t-supprot"))
docs <- tm_map(docs, plusSpace, c("type"))
docs <- tm_map(docs, plusSpace, c("ug"))
docs <- tm_map(docs, plusSpace, c("unit"))
docs <- tm_map(docs, plusSpace, c("v/p"))
docs <- tm_map(docs, plusSpace, c("valve"))
docs <- tm_map(docs, plusSpace, c("logic"))
docs <- tm_map(docs, plusSpace, c("api"))
docs <- tm_map(docs, plusSpace, c("module"))
docs <- tm_map(docs, plusSpace, c("revision"))

docs <- tm_map(docs, plusSpace, c("vendor"))   ##
docs <- tm_map(docs, plusSpace, c("가능"))
docs <- tm_map(docs, plusSpace, c("간섭"))
docs <- tm_map(docs, plusSpace, c("검토"))
docs <- tm_map(docs, plusSpace, c("격벽"))
docs <- tm_map(docs, plusSpace, c("결여"))
docs <- tm_map(docs, plusSpace, c("계산"))
docs <- tm_map(docs, plusSpace, c("계약"))
docs <- tm_map(docs, plusSpace, c("고려"))
docs <- tm_map(docs, plusSpace, c("곤란"))
docs <- tm_map(docs, plusSpace, c("공기"))  #공기 압축기라는 단어 존재
docs <- tm_map(docs, plusSpace, c("구매"))
docs <- tm_map(docs, plusSpace, c("근거"))
docs <- tm_map(docs, plusSpace, c("급수"))
docs <- tm_map(docs, plusSpace, c("도면"))
docs <- tm_map(docs, plusSpace, c("디자인"))
docs <- tm_map(docs, plusSpace, c("반영"))
docs <- tm_map(docs, plusSpace, c("발생"))
docs <- tm_map(docs, plusSpace, c("발주처"))
docs <- tm_map(docs, plusSpace, c("방법"))
docs <- tm_map(docs, plusSpace, c("배관"))
docs <- tm_map(docs, plusSpace, c("배출"))
docs <- tm_map(docs, plusSpace, c("배치"))
docs <- tm_map(docs, plusSpace, c("변경"))
docs <- tm_map(docs, plusSpace, c("변동"))
docs <- tm_map(docs, plusSpace, c("변형"))
docs <- tm_map(docs, plusSpace, c("별도"))
docs <- tm_map(docs, plusSpace, c("보강"))
docs <- tm_map(docs, plusSpace, c("보완"))
docs <- tm_map(docs, plusSpace, c("부재"))
docs <- tm_map(docs, plusSpace, c("부족"))
docs <- tm_map(docs, plusSpace, c("불명확"))
docs <- tm_map(docs, plusSpace, c("사업주"))
docs <- tm_map(docs, plusSpace, c("사항"))
docs <- tm_map(docs, plusSpace, c("상이"))
docs <- tm_map(docs, plusSpace, c("설계"))
docs <- tm_map(docs, plusSpace, c("설치"))
docs <- tm_map(docs, plusSpace, c("수량"))
docs <- tm_map(docs, plusSpace, c("수정"))
docs <- tm_map(docs, plusSpace, c("업무"))
docs <- tm_map(docs, plusSpace, c("오동작"))
docs <- tm_map(docs, plusSpace, c("우려"))
docs <- tm_map(docs, plusSpace, c("위치"))
docs <- tm_map(docs, plusSpace, c("위험"))
docs <- tm_map(docs, plusSpace, c("작업자"))
docs <- tm_map(docs, plusSpace, c("잠재"))
docs <- tm_map(docs, plusSpace, c("재설계"))
docs <- tm_map(docs, plusSpace, c("적용"))
docs <- tm_map(docs, plusSpace, c("제작장"))
docs <- tm_map(docs, plusSpace, c("증가"))
docs <- tm_map(docs, plusSpace, c("지진"))
docs <- tm_map(docs, plusSpace, c("처리"))
docs <- tm_map(docs, plusSpace, c("철거"))
docs <- tm_map(docs, plusSpace, c("추가"))
docs <- tm_map(docs, plusSpace, c("추가"))
docs <- tm_map(docs, plusSpace, c("토목"))
docs <- tm_map(docs, plusSpace, c("펌프"))
docs <- tm_map(docs, plusSpace, c("필요"))
docs <- tm_map(docs, plusSpace, c("하중"))
docs <- tm_map(docs, plusSpace, c("확보"))
docs <- tm_map(docs, plusSpace, c("회피"))






# reb 어쩌지 => 근데 얘네끼리 별 차이 없어보임 (거의 다 안전으로 분류, 그냥 하나로 합칠까요?)
# 1)
#docs <- tm_map(docs, plusSpace, c("rev0"))
#docs <- tm_map(docs, plusSpace, c("revc"))
#docs <- tm_map(docs, plusSpace, c("rev1"))
#docs <- tm_map(docs, plusSpace, c("revb"))

# 2)
docs <- tm_map(docs, plusSpace, c("REV"))



lapply(docs, content)


# " " 안에 제거할 문자를 입력하면, " "으로 대체됨.
# aa/aa 인경우 /를 그냥 제거하면 aaaa가 되어서 의미가 변질될 수 있는데,
# " "로 대체함으로써 aa aa로 변형할 수 있음.
toSpace <- content_transformer(function(x, pattern){return(gsub(pattern," ",x))})
#docs <- tm_map(docs, toSpace, c("[[:punct:]]"))
docs <- tm_map(docs, toSpace, c(","))
docs <- tm_map(docs, toSpace, c("의"))
docs <- tm_map(docs, toSpace, c("에는"))
docs <- tm_map(docs, toSpace, c("을"))
docs <- tm_map(docs, toSpace, c("를"))
docs <- tm_map(docs, toSpace, c("에대한"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("하여"))
docs <- tm_map(docs, toSpace, c("됨"))
docs <- tm_map(docs, toSpace, c("로"))
docs <- tm_map(docs, toSpace, c("에"))
docs <- tm_map(docs, toSpace, c("으로"))
docs <- tm_map(docs, toSpace, c("따라"))
docs <- tm_map(docs, toSpace, c("따른"))
docs <- tm_map(docs, toSpace, c("에서"))
docs <- tm_map(docs, toSpace, c("대한"))
docs <- tm_map(docs, toSpace, c("인한"))
docs <- tm_map(docs, toSpace, c("되는"))
docs <- tm_map(docs, toSpace, c("위해"))
docs <- tm_map(docs, toSpace, c("위한"))
docs <- tm_map(docs, toSpace, c("하고"))
docs <- tm_map(docs, toSpace, c("하는"))
docs <- tm_map(docs, toSpace, c("하지"))
docs <- tm_map(docs, toSpace, c("한다."))


#" " 안에 제거할 문자를 입력하면, 그냥 제거됨.(띄어쓰기 상관없이)
myRemove <- content_transformer(function(x, pattern){return(gsub(pattern, "", x))})

#docs <- tm_map(docs, myRemove, c()) # " " 안에 제거할 문자 입력


docs <- tm_map(docs, stripWhitespace) #공백제거
#docs <- tm_map(docs, myRemove, c("\ d"))


# 입력한 단어 삭제(띄어쓰기 되어 있는 단어만)
mystopwords <- c(c("by","for","and","of","to","with")) 
docs <- tm_map(docs, removeWords, mystopwords)


# 임의로 삭제
#  "--->", "/0)", "1)", "13", "16", "2)", "20--->21", "30%",


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
findAssocs(dtm, c("line"),c(0.20,0.25))


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

#csv 만들기
#makecsv <- project_xy.dtm[,c(-1,-2,-3,-4,-5,-6,-7,-12,-13)]
#makecsv
#binary_df <- cbind(makecsv, project_xy$type)
#write.csv(binary_df,file="C:/Users/82104/Desktop/상아매니지먼트/20210811.csv")
