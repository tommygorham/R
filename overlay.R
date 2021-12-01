library(dplyr) 
library(lubridate) #ymd
library(ggplot2) #for plotting and creating graphics
library(tidyverse) #read_csv()
library(reshape2)#melt()


#Get Data
DATA_cumulative_cases <- read_csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/total_cases.csv")
DATA_vaccine <- read_csv('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/vaccinations/vaccinations.csv')

#Make Dataframes
TB_jhucases <- data.frame(DATA_cumulative_cases)
TB_vd <- data.frame(DATA_vaccine)

#select country and timeframe
mycountries = c("United.States") #add more countries later to compare covid cases
days_window = 334 #how many days to compare (334 is how many days since end of the year 2020)
endday = today(tzone='EST') 

#get cumulative covid cases
TB_jhucases <- TB_jhucases %>%
  dplyr::select('date',all_of(mycountries)) %>%
  dplyr::filter(between(date, endday-ddays(days_window), endday))

#changing col name for readability
colnames(TB_jhucases)[colnames(TB_jhucases) == "United.States"] <- "USA_Cumulative"
names(TB_jhucases)

#add daily covid cases using diff()
dailycases = c(0, diff(TB_jhucases[,2]))
TB_jhucases$DailyCases <- dailycases

###Covid Vaccine Data Manipulation below
TB_fullyvaccinated <- TB_vd %>% dplyr::select(2,3,6) %>% as.data.frame()
TB_fullyvaccinated <- filter(TB_fullyvaccinated, iso_code=="USA")
TB_fullyvaccinated <- TB_fullyvaccinated %>%
  dplyr::filter(between(date, endday-ddays(days_window), endday))

#merge jhu covid cases with fully vaccinated
TB1 = merge(x=TB_jhucases, y=TB_fullyvaccinated, by= 'date', all.y=TRUE) #TRUE will keep everything in vaccinated df
#make date = row name
row.names(TB1) = TB1$date
#remove first row to get rid of 0 
TB1 = TB1[-1,]
TB1



##to remove NA
#TB1CLEANED <- na.omit(TB1)



#normalize / statistics
#remove first row

TB2 <- TB1 %>% dplyr::select(2,6)
TBSCALED <- data.frame( scale(TB2))
#add data column back
TBSCALED$date = ymd(row.names(TB1$date))

#####
#plots

#not the best method so far for voerlay
# ggplot()+
  #geom_point(data = TB1,mapping =  aes( x=date, y=TB1[,2]), color="blue")+
  #geom_line(data = TB1,mapping =  aes( x=date, y=TB1[,6]), color="green")

#melt method from class -- change to z score before this plot
names(TB1)
selected_cols = c('United.States', "DailyCases", "people_fully_vaccinated")
df_melt <- melt (TB1, measure.vars=selected_cols, value.names="Values", variable.name="variable")
plot <- ggplot(df_melt, aes(x=date, y=value, color=variable)) +stat_smooth(span=0.15)
#plot + ggtitle( )

#melt method from class -- USING SCALED
names(TBMELT)
selected_cols = c('USA_Cumulative', "people_fully_vaccinated")
df_melt <- melt (TB1, measure.vars=selected_cols, value.names="Values", variable.name="variable")
plot <- ggplot(df_melt, aes(x=date, y=value, color=variable)) +stat_smooth(span=0.15)




PLOT_dailycases+ geom_point() +stat_smooth(span=0.3) + ggtitle("USA")+
  ylab("Daily Cases") + xlab("Date")









