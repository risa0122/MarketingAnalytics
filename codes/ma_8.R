library(caret)
library(pROC)
setwd('/Users/jongeun/Documents/marketing_analysis')
owner.df <- read.csv('ownerExample.csv')

confusionMatrix(factor(ifelse(owner.df$Probability>0.5,"owner","nonowner")),owner.df$Class)
confusionMatrix(factor(ifelse(owner.df$Probability>0.25,"owner","nonowner")),owner.df$Class)
confusionMatrix(factor(ifelse(owner.df$Probability>0.75,"owner","nonowner")),owner.df$Class)

# ROC 곡선 그리기
r <- roc(owner.df$Class,owner.df$Probability)
plot.roc(r)

auc(r)
