setwd('/Users/jongeun/Documents/marketing_analysis')

#데이터 불러오기
df <- read.csv("BostonHousing.csv")

plot(df$RM, df$MEDV)
m1 <- lm(MEDV~RM, data=df)
summary(m1)


plot(df$RM,df$CAT..MEDV)
m2 <- glm(CAT..MEDV~RM, data=df,family = 'binomial')
summary(m2)
exp(4.6640)

#데이터 분할 :  학습데이터 대 검증데이터 6:4 비율
set.seed(2)
train.index <- sample(c(1:dim(df)[1]), dim(df)[1]*0.6)
train.df <- df[train.index, ]
valid.df <- df[-train.index, ]

# 로지스틱 회귀분석 적합 : glm()함수 family = "binomial" 이용  
logit.reg <- glm(CAT..MEDV ~ ., data = train.df, family = "binomial")
options(scipen=999) # 소숫점 모두 표시
summary(logit.reg)

# 예측변수 1개, odd관점 해석
# CHAS 한 단뒤 증가 시, 오즈가 3012660589426195배 집값 증가한다.
exp(35.6416)    #[1] 3012660589426195
 
# DIS 한 단뒤 증가 시, 오즈가 39653905배 집값 증가한다.
exp(17.4957)   #[1] 0.5710662

# RAD 한 단뒤 증가 시, 오즈가 38063.08배 집값 증가한다.
exp(10.5470)

#분류 성능 평가
pred <- predict(logit.reg,valid.df,type="response")
predict(logit.reg,valid.df)

library(caret)
confusionMatrix(factor(ifelse(pred >  0.5, 1, 0)), factor(valid.df$CAT..MEDV))

