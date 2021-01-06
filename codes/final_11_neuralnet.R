setwd('/Users/jongeun/Documents/marketing_analysis')
1/{1+exp(-(-0.3+(0.2)*(0.05)+(0.9)*(0.01)))}


library(neuralnet)
library(caret)

# 분류(시험X)
###치즈 tinydata 인공신경망 example 
df <- read.csv("tinydata.csv")

nn <-neuralnet(Acceptance ~ Salt+Fat, data=df, linear.output=F, hidden=3)

nn$weights
prediction(nn)
plot(nn,rep="best")

predict <- compute(nn,data.frame(df$Salt, df$Fat))
predicted.class = apply(predict$net.result, 1, which.max)-1
confusionMatrix(factor(ifelse(predicted.class==1, 'like','dislike')),
                factor(df$Acceptance))



# 예측
library(caret) #preProcess함수
library(neuralnet) #neuralnet함수
library(forecast) #accuracy함수
## 데이터 불러오기
toyota.df <- read.csv("ToyotaCorolla.csv")
# 데이터 전처리: 범주형 변수에 대한 가변수 생성
toyota.df$Fuel_Type_CNG <- 1*(toyota.df$Fuel_Type == "CNG")
toyota.df$Fuel_Type_Diesel <- 1*(toyota.df$Fuel_Type == "Diesel")
# 데이터 분할 : 학습데이터 60% 검증데이터 40%
set.seed(1)
train.index <- sample(row.names(toyota.df), 0.6*dim(toyota.df)[1])
valid.index <- setdiff(row.names(toyota.df), train.index)
train.df <- toyota.df[train.index, ]
valid.df <- toyota.df[valid.index, ]
# 정규화(normalize) : 0에서 1사이로 변환
norm.values <- preProcess(train.df, method="range")
train.norm.df <- predict(norm.values, train.df)
valid.norm.df <- predict(norm.values, valid.df)
#R: Neural Nets – 예측 script
# 학습데이터에서 인공신경망 적합
nn <- neuralnet( Price ~ Age_08_04+
                  KM+
                  Fuel_Type_CNG+
                  Fuel_Type_Diesel+
                  HP+
                  Automatic+
                  Doors,
                 data=train.norm.df,
                 linear.output = T,
                 hidden = 3)

plot(nn,rep='best')
# 검증데이터에서 예측 정확도 계산
valid.pred = compute(nn, valid.norm.df[,c("Age_08_04",
                                         "KM",
                                         "Fuel_Type_CNG",
                                         "Fuel_Type_Diesel",
                                         "HP",
                                         "Automatic",
                                         "Doors")])

pred <- valid.pred$net.result
valid.df1 <- data.frame(pred)
accuracy(valid.df1$pred, valid.norm.df$Price)

