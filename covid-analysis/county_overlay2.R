# program description: converting cumulative into daily cases 
# Learning Objectives: 
# how to wrangle a dataframe and modify data 
# how to do simple plots
# how to recognize simple bias in data sets
# how to verify results using independent approaches

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
  # load the data 
  tb = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

  # reusing some code from ch1
  tb1 <- tb %>% dplyr::select(12:length(tb[1,])) %>% as.matrix()
  tb1= as.data.frame(t(tb1))
  names(tb1) = tb$Combined_Key
  current_dates= names(tb)[12:length(tb[1,])]
  tb1$dates = mdy(current_dates)

  # new code (compared to county_overlay1.R) begins here  
  days_window = 582
  endday = today (tzone='EST')
  mycounties = c( 'Hamilton, Tennessee, US')
  tb_sub <- tb1 %>% 
    dplyr::select( all_of(mycounties), 'dates') %>%
    dplyr::filter( between(dates, endday-ddays(days_window), endday))
  # currently, this is a cumulative count that we have 

  # converting cumulative into daily cases (aka the difference in every row)
  # use case of diff function 
  tb_sub[,1][90:100] #cumulative 
  diff(  tb_sub[,1][90:100]) #difference in every row
  
  #use diff() to convert cumulative cases to difference in every row to make it daily cases of covid 19 
  #when calculating the difference between a row, we are short on one, so we need to put a zero back
  #otherwise, the dimension won't match
  local_dailycases = c(0, diff( tb_sub[,1] )) 
  #putting local_dailycases back into the dataframe since local_dailycases is currently a vector
  tb_daily_sub = data.frame( local_dailycases)
  ### now that we have daily cases, formatting output
  # putting the date back 
  tb_daily_sub$YMD = mdy(row.names(tb_sub))
  head(tb_daily_sub)
  #add row/col names
  row.names(tb_daily_sub) = tb_daily_sub$YMD
  names(tb_daily_sub)[1] = mycounties
  
  # GGPLOT to plot the daily cases for this county
  myplot <- ggplot(tb_daily_sub, aes(x=YMD, y=tb_daily_sub[,1]))
  myplot + geom_point() + stat_smooth(span=0.3) + ggtitle("Daily cases") + 
                                        ylab("Daily Cases") + xlab("Time")

  #find unique provice states in us
  tb$Province_State %>% unique()
  
  # find how may counties are in Tennessee 
 TNCounties = tb %>% filter( Province_State == "Tennessee") %>% select(Admin2)
 print(paste("There are ", length(rownames(TNCounties)), " counties in Tennessee"))
  
  # US Mobility Data
  tbGMUS20 = read_csv("googlemobility-data/2020_US_Region_Mobility_Report.csv")
  tbGMUS21 = read_csv("googlemobility-data/2021_US_Region_Mobility_Report.csv")
  #3.2 stack the two google mobility data frames 
  tbGMUS = rbind( tbGMUS20,  tbGMUS21) #rbind combines by rows
  #3.3 Pick Google Mobility for the county 
  myState = 'Tennessee'
  myCounty = 'Hamilton County'
   tbGMmycounty <- 
    tbGMUS %>% filter( sub_region_1 == myState, sub_region_2 == myCounty)
  tail(tbGMmycounty)
 
  # Merge mobility and covid19 data four our location/county   
  # get most recent date of the mobility report 
  max(tbGMmycounty$date)
  tb_daily_mycounty = tb_daily_sub
  names(tb_daily_mycounty) = c("DailyCases", "date")
  #MERGE by date meaning the days that are missing from google mobility will be removed 
  tb_GMCovidmycounty = merge(x=tb_daily_mycounty, tbGMmycounty, by = 'date', all.y=TRUE)
  row.names( tb_GMCovidmycounty) = tb_GMCovidmycounty$date
  names(tb_GMCovidmycounty)
  
  print( paste( "The most recent date is ", max(tb_GMCovidmycounty$date), " ."))
  print( paste( "There are ", length(tb_GMCovidmycounty[1,]), "columns"))
  
  # Overlay 
  # in order to overlay, first we must normalize the data
  tb_GMCovidmycounty2 <- tb_GMCovidmycounty %>% dplyr :: select(2, 11:16) # select values columns 
  tb_GMCovidmycounty_scaled <- data.frame( scale(tb_GMCovidmycounty2) )
  # add date column back
  tb_GMCovidmycounty_scaled$date = ymd(row.names( tb_GMCovidmycounty_scaled))
  
  names(tb_GMCovidmycounty_scaled)
  selected_columns = c('DailyCases', "residential_percent_change_from_baseline", 'workplaces_percent_change_from_baseline')
  
  df_melt <- melt( tb_GMCovidmycounty_scaled, measure.vars=selected_columns, value.names="Values", variable.name="variable")
  plot <- ggplot(df_melt, aes(x=date, y=value, color=variable)) + stat_smooth(span=0.15)
  
  plot + ggtitle( paste(myCounty, myState))
