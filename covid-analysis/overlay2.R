# program description: this program investigates the COVID daily cases and mobility in two countries. 
# Using the JHU Covid Cases Data and the average Google Global Mobility data. 
rm( list=ls()) 

#load my libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)

# Global Cases
tb = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
##### Helpful Dataframe Commands #####
# head(tb) #view structure
#tb[,2] #select all countries
#tb[1,5]  #index row 1, column 5 which iswhere the data starts at 
# we want the countries and the cases, so we could select column 2 and column 5-all like this 
# tb[, c(2,5:length(tb[1,]))]

#try this first
tb1 <- tb %>% dplyr::select(5:length(tb[1,])) %>% as.matrix() #here i select column 2 and all dates (5->length)
tb2= as.data.frame(t(tb1))
names(tb2) = tb$Country/Region

#otherwise 
names(tb[2]) <- 'Country'
names(tb[1:5])
names(tb)[names(tb) == "Country/Region"] <- "Country"

#then this
tb1 <- tb %>% dplyr::select(5:length(tb[1,])) %>% as.matrix() #here i select column 2 and all dates (5->length)
tb2= as.data.frame(t(tb1))
names(tb2) = tb$Country

current_dates= names(tb)[5:length(tb[1,])]
tb2$dates = mdy(current_dates)
days_window = 582
endday = today (tzone='EST')
mycountries = c("Italy")

tb_sub <- tb2 %>% 
  dplyr::select( all_of(mycountries), 'dates') %>%
  dplyr::filter( between(dates, endday-ddays(days_window), endday))

local_dailycases = c(0, diff( tb_sub[,1] )) 
tb_daily_sub = data.frame( local_dailycases)
head(tb_daily_sub)

tb_daily_sub$YMD = mdy(row.names(tb_sub))
head(tb_daily_sub)
row.names(tb_daily_sub) = tb_daily_sub$YMD
tail(tb_daily_sub)
names(tb_daily_sub)[1] = mycounties
tail(tb_daily_sub)

myplot <- ggplot(tb_daily_sub, aes(x=YMD, y=tb_daily_sub[,1]))
myplot + geom_point() + stat_smooth(span=0.3) + ggtitle("Daily cases") + 
  ylab("Daily Cases") + xlab("Time")

#Google Mobility Section 
  tbGMIT20 = read_csv("2020_IT_Region_Mobility_Report.csv")
  tail(tbGMIT20)

  tbGMIT21 = read_csv("2021_IT_Region_Mobility_Report.csv")
  tail(tbGMIT20)

# stack the two google mobility data frames 
  tbGM = rbind( tbGMIT20,  tbGMIT21) #rbind combines by rows
  
# METHOD 1: filter by place id
  myCountry = 'Italy'
  myPlaceID = 'ChIJA9KNRIL-1BIRb15jJFz1LOI'
  tbGMmycountry <- 
    tbGM %>% filter( country_region == myCountry, place_id== myPlaceID)
  tail(tbGMmycountry)
  
  tb_daily_mycountry = tb_daily_sub
  names(tb_daily_mycountry) = c("DailyCases", "date")
  tail(tb_daily_mycountry)
  
  tb_GMCovidmycountry = merge(x=tb_daily_mycountry, tbGMmycountry, by = 'date', all.y=TRUE)
  row.names( tb_GMCovidmycountry) = tb_GMCovidmycountry$date
  names(tb_GMCovidmycountry) 
  
  #overlay
  tb_GMCovidmycountry2 <- tb_GMCovidmycountry %>% dplyr :: select(2, 11:16) # select values columns 
  tail(tb_GMCovidmycountry2)
  
  tb_GMCovidmycountry_scaled <- data.frame( scale(tb_GMCovidmycountry2) )
  head(tb_GMCovidmycountry_scaled)
  
  tb_GMCovidmycountry_scaled$date = ymd(row.names( tb_GMCovidmycountry_scaled))
  tail(  tb_GMCovidmycountry_scaled)
  
  #step 3 overlay
  names(tb_GMCovidmycountry_scaled)
  selected_columns = c('DailyCases', "residential_percent_change_from_baseline", 'workplaces_percent_change_from_baseline')
  df_melt <- melt( tb_GMCovidmycountry_scaled, measure.vars=selected_columns, value.names="Values", variable.name="variable")
  plot <- ggplot(df_melt, aes(x=date, y=value, color=variable)) + stat_smooth(span=0.15)
  plot + ggtitle( paste(myCountry, myPlaceID))
