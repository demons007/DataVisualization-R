# data analysis on cars data set.
##############QUESTIONS##################
#1. Graph to show how the type of fuel affects the mileage and Maximum Power of the Cars.
#2. Box plot denoting the variation in selling price according to the fuel type of the cars.
#3. Distribution of mileage with fuel type.
#4: Box plot showing distribution of Cars along with selling price considering owner as a factor.
#5: Histogram Depicting the Distribution of Cars over age of it.
#################################################################################################
library(dplyr)
library(tidyverse)
library(ggplot2)
###library

#dataframe = read.csv("car_dataset.csv", sep = "," ,na.strings=c("","NA"), head = TRUE)
data = read.csv("car_dataset.csv", sep = "," , head = TRUE)
View(summary(data))
str(data)

####### data cleaning
#find location missing values
is.na(data)
complete.cases(data)
#count NA
sum(is.na(data))
# omit NA values
df = na.exclude(data)
#check the NA
sum(is.na(df))

## visual
View(df)

### Q1
df1%>%
  ggplot()+geom_point(aes(x=mileage,y=engine,color = fuel)) +
  labs(title = 'Comparison of Mileage and Engine Power ',
       subtitle = 'wrt Fuel Type',
       x= 'Mileage in kmpl',
       y='Maximum Power of Engine in CC',
       caption = 'Question 1') + theme(axis.text.x=element_text(size=5,angle=90))

### Q2
df1%>%ggplot(aes(x=fuel,y=selling_price,fill=fuel))+
  geom_boxplot(show.legend = FALSE)+
  stat_summary(fun="mean",show.legend = FALSE)+
  labs(title = 'Distribution of Selling Price',
       subtitle = 'wrt Fuel Type',
       x= 'Fuel Type',
       y='Selling Price',
       caption = 'Question2')

## Q3
df1 %>%filter(as.integer(mileage)<30)%>%
  ggplot(aes(x=mileage,fill=fuel))+
  geom_density(alpha = 0.7)+
  labs(title = 'Distribution of Mileage',
       subtitle = 'wrt Fuel Type',
       x= 'Mileage',
       y='',
       caption = 'Question3')

## Q4


## Q5
df1%>%
  group_by(year)%>%
  count()%>%
  ggplot()+geom_col(aes(y=n,x=year))+
  labs(title = 'Age Distribution of Cars at the time of Selling',
       subtitle= 'Calculated in 2021',
       x= 'Age of Car',
       y='#Cars',
       caption = 'Question5')
