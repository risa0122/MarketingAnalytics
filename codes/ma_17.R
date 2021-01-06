install.packages("neuralnet")
library(neuralnet)
library(caret)
setwd('/Users/jongeun/Documents/marketing_analysis')
df <- read.csv('tinydata.csv')

nn <- neuralnet(Acceptance~Fat+Salt, data=df, 
                linear.output=F, hidden=4)

nn$weights
plot(nn,rep='best')

prediction(nn)
options(scipen=999)   #소수점길이조절
predict <- compute(nn,data.frame(df$Salt,df$Fat))

predicted.class = apply(predict$net.result,1,which.max)-1
confusionMatrix(factor(ifelse(predicted.class=="1","like","dislike")),
                factor(df$Acceptance))

