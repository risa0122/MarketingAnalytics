library(ggplot2)
library(ggrepel)
library(extrafont) 
font_import()
fonts()
#theme_set(theme_grey(base_family="NanumBarunGothic"))
#par(family="AppleMyungjo")
#theme_set(theme_gray(base_family="AppleMyungjo"))

setwd('/Users/jongeun/Documents/marketing_analysis')
dist <- read.csv('seoulmap.csv', header = T, fileEncoding = "euc-kr")
View(dist)

dist[is.na(dist)] <- 0
row.names(dist) <- dist[,1]
dist <- dist[,-1]   # 9*9 matrix

fit <- cmdscale(dist)  # x,y 좌표로 전환

x <- -fit[,1]
y <- -fit[,2]

plot(x,y,pch=19)
text(x,y,rownames(fit), cex=0.8)

mds <- data.frame(name=rownames(fit), x=-fit[,1], y=-fit[,2])

theme.ti <- element_text(family="Arial", face="bold", size=12) #그래프 제목 스타일 변경

theme.ax <- element_text(family="Arial", face="bold", size=10, angle=00, hjust=0.54, vjust=0.5) #그래프 축 이름 스타일 변경



ggplot(mds, aes(x,y,label=name))+geom_point()+geom_text_repel()+
  theme(axis.title = theme.ax, plot.title = theme.ti)



