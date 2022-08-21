 # program description: Similar to overlay2.R, however, investigating different data and using some alternative methods 
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
  
  # 1) load the global daily cases data 
  tb = read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  
  #2) remove / from country/region
  names(tb[2]) <- 'Country'
  names(tb[1:5])
  names(tb)[names(tb) == "Country/Region"] <- "Country"
  # tb[,2] #select all countries
  # tb[1,5]  #index row 1, column 5 which iswhere the data starts at 
  # we want the countries and the cases, so we could select column 2 and column 5-all like this 
  # tb[, c(2,5:length(tb[1,]))]
  
  tb1 <- tb %>% dplyr::select(5:length(tb[1,])) %>% as.matrix() #here i select column 2 and all dates (5->length)
   
  # 4) now transponse data 
  tb2= as.data.frame(t(tb1))
  
  names(tb2) = tb$Country
  
  # SOLUTION: since we dont have a combined key, copy first row to header
  #names(tb2) <- tb2[1,]
  #names(tb2) <- tb$Country/Region

  # OTHER SOLUTION: delete first row
  #tb2 <- tb2[-1, ]
  # all daily cases in first columnn 
  #tb2[1:617, 1]
  
  # 6) MAKE SURE IT WORKED
  tail(tb2[,1:10])
  
  #can do this too 
  # tb2["Afghanistan"]
  # tail(tb2[ , "Afghanistan"])
  #or change above two lines to make it a var 
  # country1 = ('Afghanistan')   
  # tail(tb2[ , country1])
  
  current_dates= names(tb)[5:length(tb[1,])]
  tb2$dates = mdy(current_dates)
 
  days_window = 582
  endday = today (tzone='EST')
  
  mycounties = c("Italy")
  
  tb_sub <- tb2 %>% 
    dplyr::select( all_of(mycounties), 'dates') %>%
    dplyr::filter( between(dates, endday-ddays(days_window), endday))
  # 8)  VERIFY 
  ##tb_sub[,1][90:100]
  ##[1] 11180 11917 12465 13102 13745 14529 15180 15836 16578 17353 17977
  ##  >   diff(  tb_sub[,1][90:100])
  ##[1] 737 548 637 643 784 651 656 742 775 624
  
  #9) 
  local_dailycases = c(0, diff( tb_sub[,1] )) 
  #putting local_dailycases back into the dataframe since local_dailycases is currently a vector
  tb_daily_sub = data.frame( local_dailycases)
  head(tb_daily_sub)
  
  tb_daily_sub$YMD = mdy(row.names(tb_sub))
  head(tb_daily_sub)
  
  row.names(tb_daily_sub) = tb_daily_sub$YMD
  tail(tb_daily_sub)
  
  # 10) verify 
  #11) add col names
  names(tb_daily_sub)[1] = mycounties
  tail(tb_daily_sub)
  
  #12) GGPLOT to plot the daily cases for this county
  myplot <- ggplot(tb_daily_sub, aes(x=YMD, y=tb_daily_sub[,1]))
  myplot + geom_point() + stat_smooth(span=0.3) + ggtitle("Daily cases") + 
    ylab("Daily Cases") + xlab("Time")
  
##################################################################################
  
# PART TWO: Analyze google mobility for country 1 
  #read in files
  tbGMIT20 = read_csv("2020_IT_Region_Mobility_Report.csv")
  tail(tbGMIT20)
  
  tbGMIT21 = read_csv("2021_IT_Region_Mobility_Report.csv")
  tail(tbGMIT20)
  
  # stack the two google mobility data frames 
  tbGMIT = rbind( tbGMIT20,  tbGMIT21) #rbind combines by rows
  
  head(tbGMIT[1,])
  
  myCountry = 'Italy'
  
  tbGMIT2 <- tbGMIT %>% dplyr::select( c('date', 'retail_and_recreation_percent_change_from_baseline', 'grocery_and_pharmacy_percent_change_from_baseline', 'parks_percent_change_from_baseline',
                                         'transit_stations_percent_change_from_baseline', 'workplaces_percent_change_from_baseline', 'residential_percent_change_from_baseline'))
  
  #create subset based on 1 date
  #tbGMIT[tbGMIT$date=='2020-02-15', ]
  
  tbGMIT2$YMD = as.Date(tbGMIT2$date)
  
  #groupby and summarize if 
  tbGMIT3 <- tbGMIT2 %>% group_by( YMD) %>% summarise_if(is.numeric, mean, na.rm=TRUE)
  tail(tbGMIT3)
  
  tb_daily_mycountry = tb_daily_sub
  names(tb_daily_mycountry) = c("DailyCases", "date")
  tail(tb_daily_mycountry)
  
  tb_daily_mycountry$YMD = ymd( row.names (tb_daily_mycountry))

 #merge
  tb_GMCovidmycountry = merge(x=tb_daily_mycountry, y=tbGMIT3, by = 'YMD', all.y=FALSE)
  row.names( tb_GMCovidmycountry) = tb_GMCovidmycountry$YMD
  tail(tb_GMCovidmycountry)
  cbind(names (tb_GMCovidmycountry), seq(1:length(tb_GMCovidmycountry[1,])))
  names(tb_GMCovidmycountry) = c("YMD", "DailyCases", "Date", "Retail", "Grocery", "Parks", "Transit", "Workplace", "Residential")
  tail(tb_GMCovidmycountry)
   
  #can also just remove cols, however, the line selects columns we want
  tb_GMCovidmycountry2 <- tb_GMCovidmycountry %>% dplyr::select(2,4:9)
  tail(tb_GMCovidmycountry2)
  
  tb_GMCovidmycountry2$date <- ymd(row.names(tb_GMCovidmycountry2))
  head(tb_GMCovidmycountry2)
  
  tb_GMCovidmycountry2 <- tb_GMCovidmycountry %>% dplyr::select(2,4:9)
  tail(tb_GMCovidmycountry2)

  tb_GMCovidmycountry_scaled <- data.frame( scale(tb_GMCovidmycountry2))
  head(tb_GMCovidmycountry_scaled)
 
  tb_GMCovidmycountry_scaled$date = ymd(row.names(tb_GMCovidmycountry_scaled))
  tail(tb_GMCovidmycountry_scaled)
  
  names(tb_GMCovidmycountry_scaled)
  #what do we want to overlay? 
  selected_columns = c('DailyCases', 'Retail', 'Grocery', 'Parks', 'Transit', 'Workplace', 'Residential')
  #names(tb_GMCovidmycountry_scaled)
  df_melt <- melt(tb_GMCovidmycountry_scaled, measure.vars=selected_columns, value.names="Values", variable.name="variable")
  plot <- ggplot(df_melt, aes(x=date, y=value, color=variable)) + stat_smooth(span=0.15)
  plot + ggtitle( paste(myCountry))
