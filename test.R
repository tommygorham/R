library(dplyr) 
library(lubridate)
library(ggplot2)
library(tidyverse)

#Get Covid Cumulative Cases Data
DATA_cumulative_cases <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases.csv")
#convert to data frame
TB_jhucases <- data.frame(DATA_cumulative_cases)

#select country
mycountries = c("United.States" ) #add more countries later to compare cumulative covid cases
#clean df (cols: date and countries )
TB1 <- TB_jhucases %>%
  select(c('date', mycountries))


#select a timeframe
#TB1 %>% filter(date > ymd("2020-1-22"), date < ymd("2020-2-22"))
#select a timeframe method 2 
start = ymd("2021-11-22")
end = start + ddays(7)
#TB1 %>% filter(date > start, date < end)
#plot this timeframe 
TB_sub <- TB1 %>% dplyr::filter( date >= start, date < end) %>% dplyr::select( c(1:2), date)
head(TB_sub)
tail(TB_sub)

ggplot(TB_sub, aes( x=date, y=TB_sub[,2])) + geom_point()

#ggplot(TB1, aes(x=date, y=TB1[,2]))  + geom_point() 






TB_jhucases[,1] #gets all dates (aka first col)

TB_jhucases[(TB_jhucases[,1])]


names(TB_jhucases[1,1])

