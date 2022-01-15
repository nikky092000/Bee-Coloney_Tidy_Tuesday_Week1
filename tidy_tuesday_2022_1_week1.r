tuesdata <- tidytuesdayR::tt_load('2022-01-11')
colony <- tuesdata$colony
stressor <-tuesdata$stressor
library(dplyr)
library(tidyverse)
library(ggplot2)
library(scales)
library(ggiraph)
library(tidyr)
library(gganimate)
library(gifski)
library(av)
library(cowplot)
# Based on the Bee's Colony Dataset, we will use interactive graph to show:
#1 Average lost in Bee's Coloney from 2015 to 2021 by each states
#2 Average losst in Bee's coloney as a results of various stressor
#3 Average lost in Bee's Colony in different quarters
#quick glimpse of the top 5 of the observation 
head(colony)
head(stressor)
#merge_colume <- c('year','months', 'state')
# merging of colony data set with stressor data set and creating three different for dataset for easy understand
colony_detail <- colony%>%left_join(stressor, by=c('year'='year','months'='months','state'='state'))%>%
filter(!is.na(year),
!is.na(months),
!is.na(state),
!is.na(colony_n),
!is.na(colony_max), 
!is.na(colony_lost),
!is.na(colony_lost_pct),
!is.na(colony_added),
!is.na(colony_reno),
!is.na(colony_reno_pct),
!is.na(stressor),
!is.na(stress_pct))%>% group_by ( state, stressor)%>%
summarise(colony_max_total=sum(colony_max),
total_col_add=sum(colony_added),
colony_n_total= sum(colony_n),
colony_lost_total=sum(colony_lost),
colony_reno_total=sum(colony_reno)
) 
#sum the total lost across the state 
colony_data_year_sum <- colony%>%left_join(stressor, by=c('year'='year','months'='months','state'='state'))%>%
filter(!is.na(year),
!is.na(months),
!is.na(state),
!is.na(colony_n),
!is.na(colony_max), 
!is.na(colony_lost),
!is.na(colony_lost_pct),
!is.na(colony_added),
!is.na(colony_reno),
!is.na(colony_reno_pct),
!is.na(stressor),
!is.na(stress_pct))%>% group_by ( state)%>%
summarise(colony_max_total=sum(colony_max),
total_col_add=sum(colony_added),
colony_n_total= sum(colony_n),
colony_lost_total=sum(colony_lost),
colony_reno_total=sum(colony_reno)
) 
# 
colony_quarter <- colony%>%left_join(stressor, by=c('year'='year','months'='months','state'='state'))%>%
filter(!is.na(year),
!is.na(months),
!is.na(state),
!is.na(colony_n),
!is.na(colony_max), 
!is.na(colony_lost),
!is.na(colony_lost_pct),
!is.na(colony_added),
!is.na(colony_reno),
!is.na(colony_reno_pct),
!is.na(stressor),
#!is.na(quarter),
!is.na(stress_pct))%>% group_by ( state, year, months)%>%
summarise(colony_max_total=sum(colony_max),
total_col_add=sum(colony_added),
colony_n_total= sum(colony_n),
colony_lost_total=sum(colony_lost),
colony_reno_total=sum(colony_reno)
) 
# creating quarter column from months columns 
colony_quarter<-colony_quarter%>% mutate(quarter = case_when(months=="January-March"   ~ 1,
 months=="April-June"  ~ 2, months== "July-September" ~ 3, months== "October-December" ~ 4 ))%>% dplyr::select(state, year, quarter,colony_lost_total)

# checking the structure of data
attach(colony_data)
hist(colony_max_total)

#checking for Null values
sum(is.na(colony_detail))


# ploting and subsetting the dataset to answer question 1
colony_detail_filter<-colony_detail%>%mutate(tooltip_text = paste(toupper(state), "\n", colony_lost_total))%>%
dplyr::select (state,colony_lost_total,stressor, tooltip_text)
lost<-ggplot(colony_detail_filter, aes(state,colony_lost_total, fill= stressor, data_id = state))+
 geom_bar_interactive(aes(y=colony_lost_total, fill=stressor, tooltip = tooltip_text), stat='identity')+ theme_minimal() +coord_flip() 
 point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
lost<-lost+ scale_y_continuous(labels = point, name='Colony Lost') + 
labs(title = "Mean average impact of Bee's colony lost by  stressor",caption =paste("source: Data from the USDA"))
girafe(ggobj = lost, width_svg = 9, height_svg = 10)
# ploting and subsetting the dataset to answer question 2
colony_data_year_sum_filter<-colony_data_year_sum%>%mutate(tooltip_text = paste(toupper(state), "\n", colony_lost_total))%>%
dplyr::select (state,colony_lost_total, tooltip_text)
lost_state<-ggplot(colony_data_year_sum_filter, aes(state,colony_lost_total, fill= state, data_id = state))+
 geom_bar_interactive(aes(y=colony_lost_total, fill=state, tooltip = tooltip_text), stat='identity')+
 theme_minimal() +
 coord_flip() 
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

lost_state <- lost_state+ scale_y_continuous(labels = point, name='Colony Lost') +
labs(title = "Mean average of  Bee's colony lost by state",caption =paste("source: Data from the USDA"))+
theme(legend.position = "none", axis.text=element_text(size=5),plot.title = element_text(size=9))


# ploting and subsetting the dataset to answer question 3
colony_quarter_filter<-colony_quarter%>%mutate(tooltip_text = paste(toupper(state), "\n", colony_lost_total))%>%
dplyr::select (state,colony_lost_total,quarter, tooltip_text)
lost_quarter<-ggplot(colony_quarter_filter, aes(state,colony_lost_total, fill= quarter, data_id = state))+
 geom_bar_interactive(aes(y=colony_lost_total, fill=quarter, tooltip = tooltip_text), stat='identity')+
theme_minimal() +
coord_flip() 
point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
lost_quarter<-lost_quarter+ scale_y_continuous(labels = point, name='Colony Lost') + 
labs(title = "Mean average of Bee's colony lost by quarter",caption =paste("source: Data from the USDA"))+
theme(legend.position = "none", axis.text=element_text(size=5), plot.title = element_text(size=9))

# Creating an interactive section between graph 2 and graph 3
ggiraph_plots=cowplot::plot_grid(lost_state , lost_quarter)
ggiraph::girafe(code=print(ggiraph_plots))