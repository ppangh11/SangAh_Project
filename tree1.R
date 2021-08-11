#Decision Tree

#1. 데이터 불러오기
df <- read.csv('./20210810.csv',
               header = T, stringsAsFactors = F)
str(df)
head(df)
df <- df[,-c(1)]
#df <- df[, -c(1, 39, 38, 37)]

#1-1. factor화
for (i in (1:ncol(df))){
  df[,i]<-factor(df[,i])
}

#2. train-test split
library(caret)

set.seed(12345)
intrain <- createDataPartition(y = df$X.1, p = 0.8, list = FALSE)
train <- df[intrain, ]
test <- df[-intrain, ]

#3. decision tree

#3-1. tree package
#install.packages("tree")
library(tree)
treemod <- tree(X.1~., data = train)
par("mar")
par(mar = c(1, 1, 1, 1))
plot(treemod)
text(treemod)

treepred <- predict(treemod, test, type='class')
confusionMatrix(treepred, test$X.1)

##pruning_가지치기인데.. 안 해도 될 듯
cv.trees <- cv.tree(treemod, FUN = prune.misclass)
plot(cv.trees)
prune.trees <- prune.misclass(treemod, best=6)  # for regression decision tree, use prune.tree function
plot(prune.trees)
text(prune.trees, pretty=0)

#3-2. rpart 이용
library(rpart)
rpartmod <- rpart(X.1~., data = train, method = "class")
plot(rpartmod)
text(rpartmod)

printcp(rpartmod)
plotcp(rpartmod)
#가지치기_뭐가 달라지는 지 모르겠음
ptree <- prune(rpartmod, cp = rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]), "CP"])
plot(ptree)
text(ptree)

rpartpred <- predict(rpartmod, test, type='class')
confusionMatrix(rpartpred, test$X.1)

#3-3. party 이용
#install.packages("party")
library(party)

partymod <- ctree(X.1~., data = train)
plot(partymod)

partypred <- predict(partymod, test)

confusionMatrix(partypred, test$X.1)
