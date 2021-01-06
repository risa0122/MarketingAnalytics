library(forecast)

setwd('/Users/jongeun/Documents/marketing_analysis')

car.df <- read.csv('ToyotaCorolla.csv')
car.df <- car.df[1:1000,]

select.var <- c(3,4,7,8,9,10,12,13,14,17,18)

# 설명모델
car.lm.0 <- lm(Price~., data = car.df[,select.var])
summary(car.lm.0)

car.lm0.step <- step(car.lm.0,direction = 'backward')
summary(car.lm0.step)

# 예측모델
set.seed(1)
train.index <- sample(c(1:1000),600)
train.df <- car.df[train.index,select.var]
valid.df <- car.df[-train.index, select.var]

# 모형 적합
car.lm <- lm(Price~., data=train.df)
options(scipen = 999)
summary(car.lm)

# 예측 정확도
car.lm.pred <- predict(car.lm, valid.df) 
accuracy(car.lm.pred,valid.df$Price)

all.residual <- valid.df$Price - car.lm.pred
data.frame('predicted'=car.lm.pred, 'actual'=valid.df$Price,
           'residual'=all.residual)

# 전역탐색
library(leaps)

Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))

train.df1 <- cbindh
head(train.df1)
search <- regsubsets(Price ~ ., data = train.df1, nbest = 1, nvmax = dim(train.df1)[2],
                     method = "exhaustive")
sum <- summary(search)

sum$which
sum$rsq
sum$adjr2

# 전방선책 방법
car.lm.null <- lm(Price~1, data=train.df)
car.lm.step <- step(car.lm.null,
                    scope = list(lower=car.lm.null,
                                 upper=car.lm),
                    direction = 'forward')
summary(car.lm.step)

# 후방소거법
car.lm.step <- step(car.lm, direction = 'backward')
summary(car.lm.step)

# 단계 선택법
car.lm.step <- step(car.lm,direction = 'both')
summary(car.lm.step)








