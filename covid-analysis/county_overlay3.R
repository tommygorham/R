# google mobility and covid19 county data plot 3 
# google mobility dataset is trying to use anonymized cell phone movement signal 
# to compare how people travel before the pandemic
# this will enable us to estimate social distancing practices

# 3.1 load google mobility
  rm( list=ls()) # clean workspace
  # for dataframe manipulation
  library(tidyverse)

  #load the static file
  mobility20 = read_csv("googlemobility-data/2020_US_Region_Mobility_Report.csv")
  mobility21 = read_csv("googlemobility-data/2021_US_Region_Mobility_Report.csv")
  tbGMUS = rbind(mobility20, mobility21) #rbind combines by rows
  
  myState = 'Tennessee'
  myCounty = 'Hamilton County'
  tbMyCounty <- 
    tbGMUS %>% filter( sub_region_1 == myState, sub_region_2 == myCounty)
    tail(tbMyCounty)
 
    #get most recent date of the mobility report 
    max(tbMyCounty$date)
    tb_daily_mycounty = tb_daily_sub
    names(tb_daily_mycounty) = c("DailyCases", "date")
    tb_GMCovidmycounty = merge(x=tb_daily_mycounty, tbGMmycounty, by = 'date', all.y=TRUE)
    row.names( tb_GMCovidmycounty) = tb_GMCovidmycounty$date
    names(tb_GMCovidmycounty)
    
    print( paste( "The most recent date is ", max(tb_GMCovidmycounty$date), " ."))
    
    print( paste( "There are ", length(tb_GMCovidmycounty[1,]), "columns"))
    
    tb_GMCovidmycounty2 <- tb_GMCovidmycounty %>% dplyr :: select(2, 11:16) # select values columns 
    tb_GMCovidmycounty_scaled <- data.frame( scale(tb_GMCovidmycounty2) )
    # add date column back
    tb_GMCovidmycounty_scaled$date = ymd(row.names( tb_GMCovidmycounty_scaled))
    #step 3 overlay
    names(tb_GMCovidmycounty_scaled)
    selected_columns = c('DailyCases', "residential_percent_change_from_baseline", 'workplaces_percent_change_from_baseline')
    df_melt <- melt( tb_GMCovidmycounty_scaled, measure.vars=selected_columns, value.names="Values", variable.name="variable")
    plot <- ggplot(df_melt, aes(x=date, y=value, color=variable)) + stat_smooth(span=0.15)
    
    plot + ggtitle( paste(myCounty, myState))
