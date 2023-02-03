library(ggplot2)
library(reshape2)
file <- read.csv("boston.csv")
file
df <- data.frame(file)
df

#par(mfrow = c(2,2))
##VARIATION OF PRICE OF HOUSE WITH DIFFERENT FACTORS
ggplot(df,aes(LSTAT,MEDV))+geom_point()+ggtitle("MEDV vs LSTAT comparision")
##as the number of lower status population(LSTAT) increases the price of house decreases(negative correlation)
##we can also calculate the statistical correlation and see the effect : -0.7376627
cor(df$LSTAT,df$MEDV)


ggplot(df,aes(MEDV,CRIM))+geom_point()+ggtitle("MEDV vs CRIM comparision")
##as the crime rate(CRIM) increases the house prices(MEDV) are decreasing(negative correlation)
##similarly the correlation is :-0.3883046
cor(df$MEDV,df$CRIM)

ggplot(df,aes(NOX,MEDV))+geom_point()+ggtitle("MEDV vs NOX comparision")
##as the nitric oxide concentration(NOX) increases the price of house decreases(MEDV)(negative correlation)
##price of the houses decrease as the pollution increases
##correlation : -0.4273208
cor(df$NOX,df$MEDV)

ggplot(df,aes(RM,MEDV))+geom_point()+ggtitle(("RM vs MEDV comparision"))
##as averarge average number of rooms increases the price of house increases(positive correlation)
##calculating correlation gives : 0.6953599
cor(df$RM,df$MEDV)

##We can also add regression lines to our plots and try to visulaize them:
##regression line using in our data:(drawing all the scatter plots again using regression lines)
#plot(df$LSTA,df$MEDV)
#abline(lm(LSTAT~MEDV,data = df),col = "blue")

#plot(df$CRIM,df$MEDV)
#abline(lm(MEDV~CRIM,data = df),col = "blue")

#plot(df$NOX,df$MEDV)
#abline(lm(NOX~MEDV,data = df),col = "blue")

#plot(df$RM,df$MEDV)
#abline(lm(RM~MEDV,data = df),col = "blue"
##Here we try to plot a heatmap of the correlation matrix of the data
cormat <- round(cor(df),2)
melted_cormat <- melt(cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                   midpoint = 0, limit = c(-1,1), space = "Lab", 
                                   name="Pearson\nCorrelation")