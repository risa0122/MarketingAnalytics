setwd('/Users/jongeun/Documents/marketing_analysis')

#데이터 불러오기
bank.df <- read.csv("UniversalBank.csv")
bank.df <- bank.df[ , -c(1, 5)] # 분석에 사용되지 않는 열 삭제(ID, zipcode)

# 학력(Education)변수를 범주형 변수로 변환 : factor()함수 이용
bank.df$Education <- factor(bank.df$Education, levels = c(1, 2, 3),labels = c("학부", "석사","박사"))

#데이터 분할 :  학습데이터 대 검증데이터 6:4 비율
set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(Personal.Loan ~ ., data = train.df, family = "binomial")
logit.reg <- glm(Personal.Loan ~., data=train.df)
options(scipen=999) # 소숫점 모두 표시
summary(logit.reg)

# 예측변수 1개, odd관점 해석
# family 한 단뒤 증가 시, 오즈가 1.729137배 대출 증가한다.
exp(0.5476224)    #[1] 1.729137
# 온라인 뱅킹 사용하는 한 사람(Online) 증가 시, 개인대출을 수락할 오즈가 0.5710662배 감소한다.
exp(-0.5602502)   #[1] 0.5710662

#분류 성능 평가
pred <- predict(logit.reg,valid.df,type="response")
predict(logit.reg,valid.df)

library(caret)
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), factor(valid.df$Personal.Loan))


confusionMatrix(pred, valid.df$Personal.Loan)
