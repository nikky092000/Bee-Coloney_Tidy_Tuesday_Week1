freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

library(dplyr)
library(tidyverse)
library(ggalt)
 df<-freedom%>% select(country, year,CL, PR)%>%pivot_wider(names_from = year, values_from =c(CL, PR))%>%
   select(country, CL_1995, CL_2000, PR_1995, PR_2000)%>%
   filter((CL_1995-CL_2000>0 & PR_1995-PR_2000>0) | (CL_1995-CL_2000<0 & PR_1995-PR_2000<0))
###
 P<-ggplot(df, aes(country))+
   geom_segment(aes(x=CL_1995, xend=CL_2000, y=country, yend=country, colour='CL'),  size=3.5)+
   geom_segment(aes(x=PR_1995,xend=PR_2000, y=country, yend=country, colour='PR'), size=2)+
   labs(x = " ", y = " ")
  
 P <-P+labs(color=' ') +theme(legend.position="bottom")
 p<-P+labs(title = " Changes in political rights and civil liberties", 
           #subtitle = "DONE BY ATLANTA UNIVERSITY", 
           caption= 'Freedom House')
 P
 
