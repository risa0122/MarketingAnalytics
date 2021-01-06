setwd('/Users/jongeun/Documents/marketing_analysis')
mower.df <- read.csv("RidingMowers.csv") #데이터불러오기

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(111)
train.index <- sample(row.names(mower.df), 0.6*dim(mower.df)[1])
valid.index <- setdiff(row.names(mower.df), train.index)
train.df <- mower.df[train.index, ]
valid.df <- mower.df[valid.index, ]

## 새로운 레코드(new household)
new.df <- data.frame(Income = 60, Lot_Size = 20)

## 산점도(scatter plot)

plot(Lot_Size ~ Income, data=train.df, pch=ifelse(train.df$Ownership=="Owner", 1, 3))
text(train.df$Income, train.df$Lot_Size, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topright", c("owner", "non-owner", "newhousehold"), pch = c(1, 3, 4))

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df
mower.norm.df <- mower.df

# caret패키지의 preProcess() 함수 사용
install.packages("caret")
library(caret)
norm.values <- preProcess(train.df[, 1:2], method=c("center", "scale"))
train.norm.df[, 1:2] <- predict(norm.values, train.df[, 1:2]) #학습데이터 정규화
valid.norm.df[, 1:2] <- predict(norm.values, valid.df[, 1:2]) #검증데이터 정규화
mower.norm.df[, 1:2] <- predict(norm.values, mower.df[, 1:2]) #전체데이터 정규화
new.norm.df <- predict(norm.values, new.df) #새 레코드 정규화

# FNN패키지의 knn() 함수 활용
install.packages("FNN")
library(FNN)
nn <- knn(train = train.norm.df[, 1:2], test = new.norm.df, cl = train.norm.df[, 3], k = 3)

# 근접 이웃 번호 
row.names(train.df)[attr(nn, "nn.index")]

# 분류 결과 확인 
nn

#caret패키지의 accuracy함수 사용
library(caret)
# accuracy.df 데이터 초기화
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# k값을 1에서 14까지 변화시키면서 분류 정확도 값 저장
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, 1:2], valid.norm.df[, 1:2],
                  cl = train.norm.df[, 3], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 3])$overall[1]
}

accuracy.df

