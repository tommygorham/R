# program description: Analyzes the COVID19 variant data downloaded from GISAID and generate two figures
# based on the GISAID SARS-COV-2 clade data

rm( list=ls()) 

library(tidyverse)
library(lubridate)
library(reshape2) 
library(ggplot2)
library(dplyr)

tb <- read.delim("variant_surveillance.tsv")
        
# fig 1: Global view of the monthly percentage of SARS-COV-2 clades reported to GISAID.  
# The Y-axis represents the fractions of each clade in the GISAID submission in that month-year.
# we want the clade, submission dates, and fraction of each clade
tb$'Collection date' = ymd(tb$'Collection.date')

tb1 <- tb %>% filter(tb$'Collection date' > ymd('2020-01-01') 
                     & tb$'Collection date' < ymd('2021-08-01'))

tb1$year = year( ymd(tb1$'Collection date'))

tb1$month = month( ymd(tb1$'Collection date'))

tb1$year_month = ym( paste (tb1$year, tb1$month))

tbClade <- tb1 %>% count (year_month, Clade) %>% group_by(year_month)

tbClade <- tbClade %>% filter (n>100)

myplot = ggplot(tbClade, aes(fill=Clade, y=n, x=year_month)) +
  geom_bar(position="fill", stat="identity")
myplot + theme(axis.text.x = element_text(angle = 45))

# fig 2: Distribution of SARS-COV-2 clade by continents. This is a pie-chart presentation of the fractions 
# of each clade by continent, or, a ' Continental distribution of clade. ' 
mainCladeTableLocCont <- table(tb$Clade, tb$Location)
mainPropTableLocCont <- as.data.frame(prop.table(mainCladeTableLocCont,2)) %>% 
  mutate("Clade"=Var1,
         "Location"=Var2,
         "N"=Freq*100) %>%
  select(Clade,Location,N)
#mainPropTableLocCont

originalCont <- ggplot(mainPropTableLocCont, aes(x="",y=Frequency,fill=Clade,)) + 
  geom_bar(stat="identity",width=1) + 
  coord_polar("y", start=0) + 
  theme_void() + 
  scale_fill_brewer(palette="Paired") + 
  facet_wrap(mainPropTableLocCont$Location) 
#originalCont

group <- tb %>% group_by(Location) %>% summarize(N=n()) %>% filter(N>=50)
#sum of africa
sum(group[1:116, 2])
sum(group[1:2, 2])
mainCladeTableMonth <- table(tb$Clade, tb$month)
mainPropTableMonth <- as.data.frame(prop.table(tbClade,2)) %>% 
  mutate("Clade"=Var1,
         "Month"=Var2,
         "Frequency"=Freq*100) %>%
  select(Clade,Month,Frequency)

#Original Data Set Proportion Table by Continent
mainCladeTableLocCont <- table(group$Location, group$N)
mainPropTableLocCont <- as.data.frame(prop.table(mainCladeTableLocCont,2)) %>% 
  mutate("Clade"=Var1,
         "Continent"=Var2,
         "Frequency"=Freq*100) %>%
  select(Clade,Continent,Frequency)
set.seed(2)
undersample20 <- cleaned %>% group_by(country) %>% sample_n(20)
under20Bar <- ggplot(undersample20, aes(x=GISAID_clade)) + 
  geom_bar()
under20Bar

#Filtering out countries that have fewer than 50 samples
group <- tb %>% group_by(Location) %>% summarize(N=n()) %>% filter(N>=50)
#sum(group$N)
cleaned <- tb %>% filter(country %in% group$country)
mainCladeTableLocCont <- table(tb$GISAID_clade, tb$region)
mainPropTableLocCont <- as.data.frame(prop.table(mainCladeTableLocCont,2)) %>% 
  mutate("Clade"=Var1,
         "Continent"=Var2,
         "Frequency"=Freq*100) %>%
  select(Clade,Continent,Frequency)
#mainPropTableLocCont
