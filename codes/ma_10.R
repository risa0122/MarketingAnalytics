setwd('/Users/jongeun/Documents/marketing_analysis')

car.lm7 <- lm(Price~Age_08_04+KM+HP+Quarterly_Tax+Weight+Fuel_TypeDiesel+Fuel_TypePetrol,data=train.df1)
summary(car.lm7)

train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]


# use regsubsets() in package leaps to run an exhaustive search.
# unlike with lm, categorical predictors must be turned into dummies manually.

library(leaps)

# create dummies for fuel type
Fuel_Type <- as.data.frame(model.matrix(~ 0 + Fuel_Type, data=train.df))

# replace Fuel_Type column with 2 dummies
train.df1 <- cbind(train.df[,-4], Fuel_Type[,-1])
head(train.df1)

search <- regsubsets(Price ~ ., data = train.df1, nbest = 1, nvmax = dim(train.df1)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which
# show metrics
sum$rsq
sum$adjr2

car.lm7 <- lm(Price~Age_08_04+KM+HP+Quart)
              

# Which variables were added?
car.df <- read.csv("ToyotaCorolla.csv")
# use first 1000 rows of data
car.df <- car.df[1:1000, ]
# select variables for regression
selected.var <- c(3, 4, 7, 8, 9, 10, 12, 13, 14, 17, 18)
# partition data
set.seed(1) # set seed for reproducing the partition
train.index <- sample(c(1:1000), 600)
train.df <- car.df[train.index, selected.var]
valid.df <- car.df[-train.index, selected.var]
# use lm() to run a linear regression of Price on all 11 predictors in the
# training set.
# use . after ~ to include all the remaining columns in train.df as predictors.
car.lm <- lm(Price ~ ., data = train.df)
summary(car.lm)

# create model with no predictors for bottom of search range
car.lm.null <- lm(Price~1, data = train.df)
summary(car.lm.null)

# use step() to run forward selection
car.lm.step <- step(car.lm.null,   
                    scope=list(lower=car.lm.null, upper=car.lm), direction = "forward")

summary(car.lm.step) 
# Which variables were added?
car.lm.step <- step(car.lm, direction = "backward")
summary(car.lm.step) 
