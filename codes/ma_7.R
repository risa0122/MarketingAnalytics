library(forecast)

setwd('/Users/jongeun/Documents/marketing_analysis')

toyota.corolla.df <- read.csv('ToyotaCorolla.csv')

set.seed(1)
training <- sample(toyota.corolla.df$Id,600)
validation <- sample(setdiff(toyota.corolla.df$Id,training), 400)

reg <- lm(Price~., data=toyota.corolla.df[,-c(1,2,8,11)], subset = training, na.action=na.exclude)
pred_t <- predict(reg, na.action = na.pass)
pred_v <- predict(reg, newdata =toyota.corolla.df[validation,-c(1,2,8,11)], na.action = na.pass)

accuracy(pred_t, toyota.corolla.df[training,]$Price)
accuracy(pred_v, toyota.corolla.df[validation,]$Price)

# x 변수 지정

reg <- lm(Price~Age_08_04+Mfg_Year+KM+HP,ata=toyota.corolla.df[,-c(1,2,8,11)], subset = training, na.action=na.exclude)
pred_t <- predict(reg, na.action = na.pass)
pred_v <- predict(reg, newdata =toyota.corolla.df[validation,-c(1,2,8,11)], na.action = na.pass)

accuracy(pred_t, toyota.corolla.df[training,]$Price)
accuracy(pred_v, toyota.corolla.df[validation,]$Price)
