setwd('/Users/jongeun/Documents/marketing_analysis')
library(ggplot2)
library(reshape)
library(gplots)
library(GGally)

housing.df <- read.csv('BostonHousing.csv')

cor(housing.df)

heatmap(cor(housing.df),Rowv = NA, Colv = NA)

heatmap.2(cor(housing.df), Rowv=FALSE, Colv=FALSE,
          dendrogram='none', cellnote=round(cor(housing.df),2),
          notecol='black', key=FALSE, trace = 'none', margin=c(9,9))

cor.mat <- round(cor(housing.df),2)
melted.cor.mat <- melt(cor.mat)

ggplot(melted.cor.mat,aes(x=X1, y=X2, fill=value))+
  geom_tile()+geom_text(aes(x=X1, y=X2, label=value))

ggplot(housing.df, aes(y=DIS, x=NOX,colour=CAT..MEDV))+
  geom_point(alpha=0.5)

plot(housing.df[,c('CRIM','INDUS','NOX','LSTAT','MEDV')])
# 같은 말: plot(housing.df[,c(1,3,5,12,13)])
ggpairs(housing.df[,c('CRIM','INDUS','NOX','LSTAT','MEDV')])

plot(housing.df$MEDV ~ housing.df$CRIM)
plot(housing.df$MEDV ~ housing.df$CRIM, log='x')
plot(housing.df$MEDV ~ housing.df$CRIM, log='xy')

# 시계열 데이터





