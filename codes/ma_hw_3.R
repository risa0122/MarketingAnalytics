#install.packages('forecast')
library(forecast)

setwd('/Users/jongeun/Documents/marketing_analysis')
housing.df <- read.csv('WestRoxbury.csv')

set.seed(10)
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.6)
valid.rows <- setdiff(rownames(housing.df),train.rows)

train.data <- housing.df[train.rows,]
valid.data <- housing.df[valid.rows,]


lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT,data = housing.df)

#학습데이터로 회귀분석 진행 - 모형구축
# 검증데이터에서 예측값 추정 - ^y

reg1 <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT,data = train.data)
pred1 <- predict(reg,newdata = valid.data)
accuracy(pred1,valid.data$TOTAL.VALUE)


reg2 <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT+FLOORS+ROOMS+BEDROOMS,data=train.data)
pred2 <- predict(reg2,newdata = valid.data)
accuracy(pred2,valid.data$TOTAL.VALUE)

reg3 <- lm(TOTAL.VALUE~LOT.SQFT+YR.BUILT+GROSS.AREA+LIVING.AREA,data=train.data)
pred3 <- predict(reg3, newdata = valid.data)
accuracy(pred3,valid.data$TOTAL.VALUE)


