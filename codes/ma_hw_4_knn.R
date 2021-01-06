setwd('/Users/jongeun/Documents/marketing_analysis')
ub.df <- read.csv("UniversalBank.csv") #데이터불러오기

#데이터 분할 (학습데이터 60%, 검증데이터 40%)
set.seed(111)
train.index <- sample(row.names(ub.df), 0.6*dim(ub.df)[1])
valid.index <- setdiff(row.names(ub.df), train.index)
train.df <- ub.df[train.index, ]
valid.df <- ub.df[valid.index, ]

## 새로운 레코드(new ub)
new.cust <- data.frame(Age = 40,Experience = 10,     
                Income = 84,Family = 2, CCAvg = 2, Education = 2,Mortgage = 0,
                Securities.Account = 0, CD.Account = 0, 
                Online = 1,CreditCard = 1)

# 정규화(normalize)를 위한 초기화
train.norm.df <- train.df
valid.norm.df <- valid.df
bank.norm.df <- ub.df

# caret패키지의 preProcess() 함수 사용
library(caret)
norm.values <- preProcess(train.df[, -c(1,5,10)], method=c("center", "scale"))
train.norm.df[, -c(1,5,10)] <- predict(norm.values, train.df[, -c(1,5,10)]) #학습데이터 정규화
valid.norm.df[, -c(1,5,10)] <- predict(norm.values, valid.df[, -c(1,5,10)]) #검증데이터 정규화
bank.norm.df[, -c(1,5,10)] <- predict(norm.values, ub.df[, -c(1,5,10)]) #전체데이터 정규화
new.norm.df <- predict(norm.values, new.cust) #새 레코드 정규화


# newcust 예측
pred<- knn(train = train.norm.df[, -c(1,5,10)], test = new.norm.df, 
          cl = train.norm.df[,10], k = 1)
pred

#caret패키지의 accuracy함수 사용
library(caret)
# accuracy.df 데이터 초기화
accuracy.df <- data.frame(k = seq(1, 14, 1), accuracy = rep(0, 14))

# k값을 1에서 14까지 변화시키면서 분류 정확도 값 저장
for(i in 1:14) {
  knn.pred <- knn(train.norm.df[, -c(1,5,10)], valid.norm.df[, -c(1,5,10)],
                  cl = train.norm.df[, 10], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, factor(valid.norm.df[,10]))$overall[1]
}

accuracy.df
# FNN패키지의 knn() 함수 활용

library(FNN)
nn <- knn(train = train.norm.df[, -c(1,5,10)], test = valid.norm.df[, -c(1,5,10)], 
          cl = train.norm.df[,10], k = 1)

# 근접 이웃 번호 
row.names(train.df)[attr(nn, "nn.index")]
# 분류 결과 확인 
nn
#knn predict
knnPredict <- predict(nn,newdata = valid.norm.df[, -c(1,5,10)])
# confusion matrix 
cf <- confusionMatrix(factor(nn),factor(valid.norm.df[,10]))
cf
