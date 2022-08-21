library(dplyr) 
library(lubridate)
library(ggplot2)
library(tidyverse)

# Getting Data
DATA_cumulative_cases <- read_csv("<link>")

# converting to a data frame
df <- data.frame(DATA_cumulative_cases)

#selecting a timeframe
start = Sys.Date()-7
ymd(start)
end = start + ddays(7)

#plotting this timeframe 
TB_sub <- TB1 %>% dplyr::filter( date >= start, date < end) %>% dplyr::select( c(1:2), date)
head(TB_sub)
tail(TB_sub)
ggplot(TB_sub, aes( x=date, y=TB_sub[,2])) + geom_point()
