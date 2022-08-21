#program description: Compare COVID CASES of two counties in the USA 
   rm( list=ls()) # clean workspace
  # for dataframe manipulation
   library(tidyverse)
   # for time manipulation
   library(lubridate)
   # for rearranging the dataframe for plotting 
   library(reshape2)
   # for creating graphics
   library(ggplot2)
   #also for data manipulation
   library(dplyr)
   
   # about the data: Data Repository of COVID-19 Cases, provided by Johns 
   # Hopkins University by the Center for Systems Science and Engineering 

  # load the data 
   tb = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
   
   #verify tb is a dataframe
   str(tb[ , 1:10]) 

## 1.2 indexing 
   #100-105th row, and 99-100th column 
   tb[100:105, 99:100]
   
   tb[12:15]
   #100th and 105th only, along with 5th and 9th only
   tb[c(100, 105), c(5,9)]
   
   #look through columns 5-9, but only give show the columns: fips, admin2, and province_state columns on the 4th of july
   tb[c(5,9), c('FIPS','Admin2', "Province_State", "7/4/21")]
   
   #length of the first row = number of columns
   numcols <- length(tb[1,])
   print(paste( "there are ", numcols, "columns" ))
   
   # truncate and transpose 
   tb1 <- tb %>% dplyr::select(12:length(tb[1,])) %>% as.matrix()
   tb1= as.data.frame(t(tb1))
   tail(tb1)
  
   # add meaningful column names 
   names(tb1) = tb$Combined_Key
   # select a time window 
   # convert dates from text format to actual date type using lubridate library
   # format month/day/year
   library(lubridate)
   mdy("10/1/2020")
   current_dates= names(tb)[12:length(tb[1,])]
   class(current_dates)
   current_dates
   mdy(current_dates[1:10])
   #using mdy object, assign current_dates to be mdy format back into the column
   tb1$dates = mdy(current_dates)
   tail(tb1)
   
   #selecting the time window using filter (select is for column, filter is for row)
   tb1 %>% filter( dates >= mdy("3/15/2020"), dates <= mdy("3/20/20")) %>% head()
   #30-day sliding window (quick time window analysis)
   start = mdy("12/15/20")
   end = start + ddays(30)
   tb1 %>% dplyr::filter( dates >= start, dates <= end)  %>%
   dplyr::select(1:2)
   #plot time window
   tb_sub <- tb1 %>% dplyr::filter(  dates >= start, dates < start + ddays(30)) %>% 
     dplyr::select( c(1:5), dates)
   head(tb_sub)
   ggplot(tb_sub, aes( x = dates, y=tb_sub[,3])) + geom_point() + geom_smooth()
   
# 1.6 view my county
   mycounty = c( 'Hamilton, Tennessee, US')
   tail( tb1[ , mycounty])
   mycounty2 = c( 'Williamson, Tennessee, US')
   tail( tb1[ , mycounty2])
   
   ### END OF CHAPTER
   ### TO MAKE THIS A .HTML IN RSTUDIO:
   ### CTRL + SHIFT + K 
   ### COMPILE 
