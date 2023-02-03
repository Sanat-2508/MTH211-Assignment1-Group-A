############## Assignment - 1 MTH211A #########################



##################### Problem - 1 ############################

# As we decided to scrape data from codeforces IDs of student of IIT Kanpur
# and apply the statistics applications on that data.

library(rvest)
library(tidyverse)
dat = read.csv("input.csv")


mat =  matrix(0, nrow = 234, ncol = 12)
df = as.data.frame(mat)

colnames(df) <- c("Current.Rating", "MaximumRating", "Class", "RegisteredWhen", 
                  "Problems.Solved.All.Time","Problems.Solved.last.year", "Problems.Solved.last.month",
                  "max.days.in.row.All.Time", "max.days.in.row.last.year", "max.days.in.row.last.month", 
                  "Friends.of.users", "Total.no.of.contests")
colnames(dat) <- c("Roll.Number", "codeforces")
# 1) present rating
# 2) maximum rating
# 3) what is his class (like "Expert")
# 4) Registered when ?

# 5) No. of Problems Solved all time
# 6) No. of Problems Solved last year
# 7) No. of Problems Solved last month
# 8) No. of day in row(max) all time
# 9) No. of day in row(max) last year
# 10) No. of day in row(max) last month

# 11) friend of users (aka Popularity)
# 12) no. of contest given

base = "https://codeforces.com/profile/"
base2 = "https://codeforces.com/contests/with/"

for(i in 1:234){
  CF_id = dat$codeforces[i]
  full_id = paste(sep = "", base, CF_id)
  id <- read_html(full_id)
  
  items_1_to_4 <- html_elements(id,"li span")%>% html_text()
  
  Friend_of_users <- html_elements(id,".info li") %>% html_text()
  Friend_of_users <- Friend_of_users[3]
  Friend_of_users <- as.integer(strsplit(Friend_of_users, " +")[[1]][6])
  
  if(items_1_to_4[3] != "0")
  {
    df[i, 1] = as.integer(items_1_to_4[3])
    df[i, 2] = as.integer(items_1_to_4[6])
    df[i, 3] = substr(items_1_to_4[5], 1, nchar(items_1_to_4[5])-2)
    df[i, 4] = items_1_to_4[9]
    
    items_5_to_10 <- html_elements(id,"._UserActivityFrame_counterValue") %>% html_text()
    
    for(j in 5:7){
      k =substring(items_5_to_10[j-4],1,nchar(items_5_to_10[j-4])-9)
      dt <- as.integer(k)
      df[i,j] = dt
    } 
    for(j in 8:10){
      k = substring(items_5_to_10[j-4],1,nchar(items_5_to_10[j-4])-5)
      dt = as.integer(k)
      df[i,j] = dt
    }
    df[i, 11] = Friend_of_users
    
    
    con_id = paste(sep="", base2, CF_id)
    id2 = read_html(con_id)
    tab <- html_table(id2)
    tab <- tab[6]
    tab <- as.data.frame(tab)
    Number_of_contests <- tab[1,1]
    df[i , 12] = Number_of_contests
  }
}


