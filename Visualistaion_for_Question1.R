ibrary(stringi)
library(stringr)
hist(df$MaximumRating,col = "blue", main = "Histogram of the contest ratings",xlab = "ratings")
##we can clearly see that most of the people around more than a quarter are having less than 500 rating and there are very few people above 1700
ggplot(df,aes(MaximumRating,Problems.Solved.All.Time))+geom_point()+ labs(x = "Maximum Ratings",y = "Problems solved all time")+ ggtitle("Scatterplot of alltime best rating vs all time problems solved")
##we can see that as the number of problems solved by an individual increases ,his/her rating gradually improves
ggplot(df,aes(Total.no.of.contests,Current.Rating))+geom_point()+labs(x = "Total number of contests",y = "Current Rating")+ggtitle("Scatter Plot of Current Rating vs Total Number of Contests")
##We can see that there is a direct correlation between the total number of contests given by a individual and the current rating he/she has
##Clearly, the more contests you give hisgher is your rating, hard work pays off!!

##Impact of the rating of a coder on his popularity
ggplot(df,aes(Friends.of.users,Current.Rating))+geom_point()+labs(x = "Users with friends",y = "Rating")+ggtitle("Friends of users vs rating")
##we find that if anyone's rating is very high then he is definitely an inspiration for others and he has a lot of friends