setwd('/Users/jongeun/Documents/marketing_analysis')

bank.df <- read.csv('UniversalBank.csv')
bank.df <- bank.df[,-c(1,5)]

bank.df$Education <- factor(bank.df$Education, levels=c(1,2,3), labels = c('학사','석사','박사'))
summary(bank.df)

set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]),dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index,]
valid.df <- bank.df[-train.index,]

#로지스틱 회귀분석 적합
logit.reg <- glm(Personal.Loan~., data=train.df, family = 'binomial')
options(scipen = 999)
summary(logit.reg)

# 성능평가
pred <- predict(logit.reg, valid.df, type='response')
library(caret)
accuracy(pred,valid.df$Personal.Loan)
confusionMatrix(factor(ifelse(pred>0.5,1,0)),factor(valid.df$Personal.Loan))
