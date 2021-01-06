setwd('/Users/jongeun/Documents/marketing_analysis')
telecom.df <- read.csv('Telecom.csv')
dim(telecom.df)

ytotal <- model.matrix(~0+Contract,data=telecom.df)
ytotal <- as.data.frame(ytotal)
t(t(names(ytotal)))

ytotal <- ytotal[,-3]
t(t(names(ytotal)))

# +a) 원본 데이터에 가변수 통합
telecom.df <- telecom.df[,-8]
telecom.df <- cbind(telecom.df,ytotal)
