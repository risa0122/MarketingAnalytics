setwd('/Users/jongeun/Documents/marketing_analysis')

housing.df <- read.csv('WestRoxbury.csv',header = TRUE)

dim(housing.df)
head(housing.df)

xtotal <- model.matrix(~0+BEDROOMS+REMODEL,data = housing.df)
xtotal <- as.data.frame(xtotal) # matrix를 dataframe 으로 바뚬

#names(xtotal)      # 변수명 확인
#t(names(xtotal))   # transpose 행,열 바꾸기 
t(t(names(xtotal))) # 1열로 정리해서 변수명 확인
head(xtotal)

# Recent   기준으로 가변수 정리
xtotal <- xtotal[,-4]
head(xtotal)

