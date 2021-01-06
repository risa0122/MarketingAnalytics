setwd('/Users/jongeun/Documents/marketing_analysis')
telecom.df <- read.csv('Telecom.csv')

set.seed(1)
test.rows <- sample(rownames(telecom.df),dim(telecom.df)*0.5)
test.data <- telecom.df[test.rows,]

valid.rows <- sample(setdiff(rownames(telecom.df),train.rows),dim(telecom.df)*0.25)
valid.data <- telecom.df[valid.rows,]

test.rows <- setdiff(rownames(telecom.df),union(test.rows,valid.rows))
test.data <- telecom.df[test.rows,]
