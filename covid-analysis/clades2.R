# program description: Similar to clades1.R, however generates an HTML and a pie chart
```{r}
rm( list=ls()) 
library(tidyverse)
library(lubridate)
library(reshape2) 
library(ggplot2)
library(dplyr)
```


```{r}
tsv <- read_tsv("variant_surveillance.tsv")
tb = data.frame(tsv)
head(tb)
```
```{r}
#group into year and month 
tb$'Collection date' = ymd(tb$'Collection.date')
tb1 <- tb %>% filter(tb$'Collection date' > ymd('2020-01-01') 
                     & tb$'Collection date' < ymd('2021-08-01'))
tb1$year = year( ymd(tb1$'Collection date'))
tb1$month = month( ymd(tb1$'Collection date'))
tb1$y_m = ym( paste (tb1$year, tb1$month))
```

```{r} 
# count clades by row 
tbClade <- tb1 %>% count (y_m, Clade) %>% group_by(y_m)
#filter what is significant 
tbClade <- tbClade %>% filter (n>100)
#plot from lecture 
myplot = ggplot(tbClade, aes(fill=Clade, y=n, x=y_m)) +
  geom_bar(position="fill", stat="identity")
myplot + theme(axis.text.x = element_text(angle = 45))
```

```{r}
#new dataframe with relevant data 
tb2 <- tb %>% dplyr::select(3,9,11) %>% as.data.frame()
  
#convert date
tb2$'year_month' <- format(tb2$'Submission date', format = "%Y-%m")
  
#remove unknown daa 
tb2_cleaned <- tb2[!(is.na(tb2$Clade)),]
head(tb2_cleaned)
```
```{r}
#add continent col
  tb2Continent <- tb2_cleaned%>% 
    mutate(Continent = case_when(
      startsWith(Location, "Oceania") ~ "Oceania", 
      startsWith(Location,"Asia") ~ "Asia", 
      startsWith(Location, "North") ~ "North America", 
      startsWith(Location, "Europe") ~ "Europe", 
      startsWith(Location, "South") ~ "South America", 
      startsWith(Location, "Africa") ~ "Africa", 
      startsWith(Location, "USA") ~ "North America", 
      startsWith(Location, "Central Asia") ~ "Asia", 
    ))
head(tb2Continent)
```

#pie plot
```{r}
ggplot(tb2Continent, aes(x=1, fill=Clade)) + 
    geom_bar(position="fill")+
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank())+
    coord_polar(theta="y")+
    facet_wrap(vars(Continent))
  
         
``` 
