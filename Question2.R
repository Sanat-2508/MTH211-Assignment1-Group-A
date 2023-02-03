# We want to find the suicides are related to some attributes of the native country. Of course people have some prosonal
# reasons also, but environment also contributes to it. So, we investigate and try to extract ansewers about some of the 
# following questions:

#1. Did richer countries save their people from doing suicides and inverse statement.
#2. how does the situation changes during the course of time(we would be carefull as population also increases generally).
#3. overall which age-group is more effected and did this varies country to country?

library(ggplot2)
library(dplyr)
library(plotly)
library(hrbrthemes)

dataset <- read.csv("Suicide.csv")
dataset <- data.frame(dataset)
summary(dataset)

######******let us try to Reorder levels of age to be in increasing age gap order. 
dataset$age <- factor(dataset$age, levels = c("5-14 years", "15-24 years", 
                                              "25-34 years", "35-54 years", "55-74 years", "75+ years"))


####################################################################################
Total_suicdies_in_years <- matrix(0,nrow=32,ncol=8)
for(i in 1985:2016)
{
  datind = (dataset$year == i)
  data <- dataset[datind, ]
  Total_suicdies_in_years[i-1984,1] = i
  Total_suicdies_in_years[i-1984,2] = sum(data$suicides_no)
  ages = levels(unique(dataset$age))
  for(j in 1:6)
  {
    datind1<- (data$age == ages[j] )
    data1 <- data[datind1, ]
    Total_suicdies_in_years[i-1984,j+2] = sum(data1$suicides_no)
    
  }
}

Total_suicdies_in_years <- as.data.frame(Total_suicdies_in_years)
colnames(Total_suicdies_in_years) <- c("Year","Total_Suicides",levels(unique(dataset$age)))
boxplot(Total_suicdies_in_years$Total_Suicides,Total_suicdies_in_years$`5-14 years`,
        Total_suicdies_in_years$`15-24 years`,Total_suicdies_in_years$`25-34 years`,
        Total_suicdies_in_years$`35-54 years`,Total_suicdies_in_years$`55-74 years`,Total_suicdies_in_years$`75+ years`)
################# let us define the types of different Variables #######################

#country <- Nominal variable
#year <- ordinal variable
#sex <- Binary variable
#age <- ordinal variable
#suicides_no <- Discrete variable
#population <- Discrete variable
#suicides.100k.pop <- Continuous variable
#gdp_per_capita... <- Discrete Variable


######################################################################################


######################### Summarizing the Data ##########################################



data_maleind = (dataset$sex == "male") 
data_male <- dataset[data_maleind, ]
data_femaleind = (dataset$sex == "female") 
data_female <- dataset[data_femaleind, ]

get_year_data <- function(year)
{ 
  dat_yearind = (dataset$year == year)
  data_year = dataset[dat_yearind, ]
  return(data_year)
} 
get_year_data(1999)



plot_country_suicides <- function(Country)
{
  countryind = (dataset$country == Country)
  dat = dataset[countryind, ]
  
  ggplot(dat,aes(x=year,y=suicides_no,fill = age)) +geom_bar(position="dodge", stat="identity")
  
}

plot_country_suicide_100k <- function(Country)
{
  countryind = (dataset$country == Country)
  dat = dataset[countryind, ]
  
  ggplot(dat,aes(x=year,y=suicides.100k.pop,fill = age)) +geom_bar(position="dodge", stat="identity")
  
}

plot_country_gender <- function(Country)
{
  countryind = (dataset$country == Country)
  dat = dataset[countryind, ]
  
  ggplot(dat,aes(x=year,y=suicides.100k.pop,fill = sex)) +geom_bar(position="dodge", stat="identity")
  
}


plot_country_gender <- function(Country)
{
  countryind = (dataset$country == Country)
  dat = dataset[countryind, ]
  
  ggplot(dat,aes(x=year,y=suicides.100k.pop,fill = sex)) +geom_bar(position="dodge", stat="identity")
  
}

par(mfrow = c(1,2))
ggplot(dataset, aes(x=year, y=suicides_no,fill = age)) +
  geom_bar(position="dodge", stat="identity") +
  ylim(0,max(dataset$suicides_no))+theme_ipsum()

ggplot(dataset, aes(x=year, y=suicides.100k.pop  ,fill = age)) +
  geom_bar(position="dodge", stat="identity") +
  ylim(0,max(dataset$suicides_no))+theme_ipsum()