setwd('/Users/jongeun/Documents/marketing_analysis')
housing.df <- read.csv('WestRoxbury.csv',header = TRUE)

rows.to.missing <- sample(row.names(housing.df),10)
housing.df[rows.to.missing,]$BEDROOMS <- NA
summary(housing.df$BEDROOMS)

housing.df[rows.to.missing,]$BEDROOMS <- median(housing.df$BEDROOMS,na.rm = TRUE)
summary(housing.df$BEDROOMS)

housing.df[rows.to.missing,]$BEDROOMS <- mean(housing.df$BEDROOMS,na.rm = TRUE)
summary(housing.df$BEDROOMS)

# 데이터 분할
set.seed(1)
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.6)
train.data <- housing.df[train.rows,]

valid.rows <- setdiff(rownames(housing.df),train.rows)
valid.data <- housing.df[valid.rows,]


set.seed(1)
train.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.5)
train.data <- housing.df[train.rows,]

valid.rows <- sample(rownames(housing.df),dim(housing.df)[1]*0.3)
valid.data <- housing.df[valid.rows,]

test.rows <- setdiff(rownames(housing.df),union(train.rows,valid.rows))
test.data <- housing.df[test.rows,]



