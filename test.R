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
start = Sys.Date()-7
ymd(start)
end = start + ddays(7)

#plot this timeframe 
TB_sub <- TB1 %>% dplyr::filter( date >= start, date < end) %>% dplyr::select( c(1:2), date)
head(TB_sub)
tail(TB_sub)

ggplot(TB_sub, aes( x=date, y=TB_sub[,2])) + geom_point()

#TO: Daily Cases, overlay vaccine





