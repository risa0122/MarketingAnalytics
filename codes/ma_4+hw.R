setwd('/Users/jongeun/Documents/marketing_analysis')
housing.df <- read_csv('BostonHousing.csv')

data.for.plot <- aggregate(housing.df$MEDV, by=list(housing.df$CHAS),FUN=mean)
names(data.for.plot) <- c('CHAS','MeanMEDV')          

barplot(data.for.plot$MeanMEDV,names.arg =data.for.plot$CHAS,
        xlab = 'CHAS',ylab = 'Avg.MEDV',col=c("darkgoldenrod3","cyan3"))

# 실습

d2 <- aggregate(housing.df$LSTAT, by=list(housing.df$CHAS), FUN=mean)
names(d2) <- c('CHAS','Mean_Low_Income')
barplot(d2$Mean_Low_Income, names.arg = d2$CHAS,
        xlab='CHAS',ylab = 'Avg.Low_Income',
        col = c('lightpink','thistle3'))

plot(housing.df$MEDV ~housing.df$LSTAT, xlab='LSTAT',ylab = 'MEDV',
     col = 'darkorchid4')

plot(housing.df$LSTAT ~housing.df$MEDV, xlab='MEDV',ylab = 'LSTAT',
     col = 'green')
plot(housing.df$MEDV ~housing.df$RAD,col='cyan4')

hist(housing.df$MEDV,xlab ='MEDV',col='steelblue3' )
hist(housing.df$LSTAT,xlab ='LSTAT',col='steelblue3' )
hist(housing.df$RAD,xlab ='RAD',col='steelblue3' )

boxplot(housing.df$MEDV~housing.df$CHAS,
        xlab='CHAS',ylab = 'MEDV',col=c("darkgoldenrod3","cyan3"))

# 실습
boxplot(housing.df$NOX ~housing.df$`CAT. MEDV`,
        xlab='Abovethree_MEDV',ylab = 'NOX',
        col = c('lightpink','thistle3'))
boxplot(housing.df$LSTAT ~housing.df$`CAT. MEDV`,
        xlab='Abovethree_MEDV',ylab = 'LSTAT',
        col = c("darkgoldenrod3","cyan3"))
boxplot(housing.df$INDUS ~housing.df$`CAT. MEDV`,
        xlab='Abovethree_MEDV',ylab = 'INDUS',
        col = c('red','steelblue3'))

