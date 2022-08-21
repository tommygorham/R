#Removes all of the objects that are present in the workspace
rm( list=ls()) 

#load my libraries
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggplot2)
library(dplyr)

# PART ONE: Global Daily Cases

tb = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

## head(tb) #view structure
## tb[,2] #select all countries
## tb[1,5]  #index row 1, column 5 which is where the data starts at 

## we want the countries and the cases, so we could select column 2 and column 5-all like this #  tb[, c(2,5:length(tb[1,]))]

#rename country columnif there is an issue with indexing 'Country/Region'
names(tb[2]) <- 'Country'
names(tb[1:5])
names(tb)[names(tb) == "Country/Region"] <- "Country"

#then do dplyr::select
tb1 <- tb %>% dplyr::select(5:length(tb[1,])) %>% as.matrix() #here i select column 2 and all dates (5->length)
tb2= as.data.frame(t(tb1))
names(tb2) = tb$Country

#select time window as we did in class
current_dates= names(tb)[5:length(tb[1,])]
tb2$dates = mdy(current_dates)
days_window = 582
endday = today (tzone='EST')
mycountries = c("Italy", "Afghanistan") #vector to store the 2 countries we will compare 

#sub-table of cumulative daily cases for both countries
tb_sub <- tb2 %>% 
  dplyr::select( all_of(mycountries), 'dates') %>%
  dplyr::filter( between(dates, endday-ddays(days_window), endday))

#changed df to represent italy daily cases 
italy_dailycases = c(0, diff( tb_sub[,1] )) 
tb_IT_daily_sub = data.frame( italy_dailycases)
head(tb_IT_daily_sub)

#make rows the date in the italy daily cases df
tb_IT_daily_sub$YMD = mdy(row.names(tb_sub))
head(tb_IT_daily_sub)
row.names(tb_IT_daily_sub) = tb_IT_daily_sub$YMD
tail(tb_IT_daily_sub)


#similarly, this df represents fghanistan daily cases
afghanistan_dailycases = c(0, diff( tb_sub[,2] )) #from col 2 in tb_sub 
tb_AF_daily_sub = data.frame( afghanistan_dailycases)
tail(tb_AF_daily_sub) #checking 

#make rows the date or italy daily cases df
tb_AF_daily_sub$YMD = mdy(row.names(tb_sub))
head(tb_AF_daily_sub)
row.names(tb_AF_daily_sub) = tb_AF_daily_sub$YMD
tail(tb_AF_daily_sub) #checking

#set column name to country name for italy
names(tb_IT_daily_sub)[1] = mycountries
tail(tb_IT_daily_sub)

#set column name to country name for afghanistan
names(tb_AF_daily_sub)[1] = mycountries[2] # via index 2 in the vector 
tail(tb_AF_daily_sub)

#plotting italy daily cases
myITplot <- ggplot(tb_IT_daily_sub, aes(x=YMD, y=tb_IT_daily_sub[,1]))
myITplot + geom_point() + stat_smooth(span=0.3) + ggtitle(paste("Daily cases in", mycountries[1])) + 
  ylab("Daily Cases") + xlab("Time")

#plotting afghanistan daily cases
myAFplot <- ggplot(tb_AF_daily_sub, aes(x=YMD, y=tb_AF_daily_sub[,1]))
myAFplot + geom_point() + stat_smooth(span=0.3) + ggtitle(paste("Daily cases in", mycountries[2])) + 
  ylab("Daily Cases") + xlab("Time")


# part 2 mobility 

#for country 1: Italy
  #read in mobility data
  tbGM20 = read_csv("2020_IT_Region_Mobility_Report.csv")
  tbGM21 = read_csv("2021_IT_Region_Mobility_Report.csv")

  #stack the two dfs
  tbGM = rbind(tbGM20, tbGM21)
  
  # METHOD 1: filter by place id
  myCountry = 'Italy'
  myPlaceID = 'ChIJA9KNRIL-1BIRb15jJFz1LOI' #place id for italy
  
  # get the data for the country of italy
  tbGMIT <- tbGM %>%  filter(country_region == myCountry, place_id == myPlaceID) 
  tail(tbGMIT) 
  #setting col names
  tb_daily_IT = tb_IT_daily_sub
  names(tb_daily_IT) = c("DailyCases", "date")
  head(tb_daily_IT)
  #merge italy daily cases with italy google mobility
  tb_GMCovidIT = merge(x=tb_daily_IT, y = tbGMIT, by = 'date', all.y=TRUE)
  row.names(tb_GMCovidIT) = tb_GMCovidIT$date
  tail(tb_GMCovidIT)
  
  # select the relevant information we need from the data
  tb_GMCovidIT2 <- tb_GMCovidIT %>% dplyr::select(2, 11:16)
  tail(tb_GMCovidIT2)
  
  #scale the data for overlay 
  tb_GMCovidIT_scaled <- data.frame(scale(tb_GMCovidIT2))
  head(tb_GMCovidIT_scaled)
  
  #add the date column back 
  tb_GMCovidIT_scaled$date = ymd(row.names(tb_GMCovidIT_scaled))
  
#for country 2: Afghanistan
  #read in mobility data
  tbGM2_20 = read_csv("2020_AF_Region_Mobility_Report.csv")
  tbGM2_21 = read_csv("2021_AF_Region_Mobility_Report.csv")
  
  #stack the two dfs
  tbGM2 = rbind(tbGM2_20, tbGM2_21)
  
  myCountry2 = 'Afghanistan'
  myPlaceID2= 'ChIJbQL_-LZu0TgReNqWvg1GtfM' #place id for Afghanistan

  # get the data for the country of Afghanistan
  tbGMAF <- tbGM2 %>%  filter(country_region == myCountry2, place_id == myPlaceID2) 
  tail(tbGMAF)
  #set col names
  tb_daily_AF = tb_AF_daily_sub
  names(tb_daily_AF) = c("DailyCases", "date")
  head(tb_daily_AF)

  #merge Afghanistan daily cases with Afghanistan google mobility
  tb_GMCovidAF = merge(x=tb_daily_AF, y = tbGMAF, by = 'date', all.y=TRUE)
  row.names(tb_GMCovidAF) = tb_GMCovidAF$date
  tail(tb_GMCovidAF)

# select the relevant information we need from the afghanistan data 
  tb_GMCovidAF2 <- tb_GMCovidAF %>% dplyr::select(2, 11:16)
  tail(tb_GMCovidAF2)

  #scale the data for overlay
  tb_GMCovidAF_scaled <- data.frame(scale(tb_GMCovidAF2))
  head(tb_GMCovidAF_scaled)

  #add the date column back 
  tb_GMCovidAF_scaled$date = ymd(row.names(tb_GMCovidAF_scaled))


#for both countries, compare the same data by selecting the same cols
selected_cols = c('DailyCases', 'residential_percent_change_from_baseline', 'workplaces_percent_change_from_baseline')


#plot both countries, starting with italy
df_meltIT <- melt(tb_GMCovidIT_scaled, measure.vars=selected_cols, value.names="Values", variable.name = "variable")

#do the plot to create the graphic
plot <- ggplot(df_meltIT, aes(x=date, y = value, color=variable)) + 
  stat_smooth(span=0.15)

plot + ggtitle(paste(myCountry, myPlaceID))


#lastly, plot Afghanistan
df_meltAF <- melt(tb_GMCovidAF_scaled, measure.vars=selected_cols, value.names="Values", variable.name = "variable")

#do the plot to create the graphic
plot <- ggplot(df_meltAF, aes(x=date, y = value, color=variable)) + 
  stat_smooth(span=0.15)

plot + ggtitle(paste(myCountry2, myPlaceID2))
