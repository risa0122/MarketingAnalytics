setwd('/Users/jongeun/Documents/marketing_analysis')

housing.df <- read.csv("BostonHousing.csv") #데이터불러오기

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(111)
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])
valid.index <- setdiff(row.names(housing.df), train.index)
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df

# caret패키지의 preProcess() 함수 사용
library(caret)
norm.values <- preProcess(train.df[, 1:12], method=c("center", "scale"))
train.norm.df[, 1:12] <- predict(norm.values, train.df[, 1:12]) #학습데이터 정규화

valid.norm.df[, 1:12] <- predict(norm.values, valid.df[, 1:12]) #검증데이터 정규화

#caret패키지의 accuracy함수 사용
library(caret)

# accuracy.df 데이터 초기화
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# k값을 1에서 14까지 변화시키면서 분류 정확도 값 저장
for(i in 1:14) {
  
  knn.pred <- knn(train.norm.df[, 1:12], valid.norm.df[, 1:12],
                  cl = train.norm.df[, 14], k = i)
  accuracy.df[i, 2] <- confusionMatrix(factor(knn.pred), factor(valid.norm.df[, 14]))$overall[1]
}


accuracy.df
