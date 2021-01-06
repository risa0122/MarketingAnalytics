setwd('/Users/jongeun/Documents/marketing_analysis')

housing.df <- read.csv("BostonHousing.csv")
dim(housing.df)

# select variables for regression
selected.var <- c(1,4,6,13)

# partition data (trn:cal=6:4)
set.seed(1) # set seed for reproducing the partition
train.index <- sample(row.names(housing.df), 0.6*dim(housing.df)[1])
valid.index <- setdiff(row.names(housing.df), train.index)
train.df <- housing.df[train.index,selected.var]
valid.df <- housing.df[valid.index,selected.var]

# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
housing.lm <- lm(MEDV ~ ., data = train.df)

# use options() to ensure numbers are not displayed in scientific notation.
options(scipen = 999)
summary(housing.lm)

library(forecast)
# use predict() to make predictions on a new set.
new.d <- data.frame(CRIM=0.1, CHAS=0,RM=6)
housing.lm.pred <- predict(housing.lm, newdata=new.d)
housing.lm.pred

### residuals 전체
housing.lm.pred <- predict(housing.lm, valid.df)
all.residuals <- valid.df$MEDV - housing.lm.pred
data.frame("Predicted" = housing.lm.pred, "Actual" = valid.df$MEDV, "Residual" = all.residuals)
options(scipen=999, digits = 3)
accuracy(housing.lm.pred, valid.df$MEDV)
length(all.residuals[which(all.residuals > -1406 & all.residuals < 1406)])/400
hist(all.residuals, breaks = 25, xlab = "Residuals", main = "")



'''
### residuals 작게
some.residuals <- valid.df$CAT..MEDV[1:20] - housing.lm.pred[1:20]
data.frame("Predicted" = housing.lm.pred[1:20], "Actual" = valid.df$CAT..MEDV[1:20], "Residual" = some.residuals)
options(scipen=999, digits = 3)
# use accuracy() to compute common accuracy measures.
accuracy(housing.lm.pred, valid.df$CAT..MEDV)
'''
