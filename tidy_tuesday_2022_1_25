library(tidytuesdayR)
library(dplyr)
library(tidyverse)
tuesdata <- tidytuesdayR::tt_load('2022-01-25')

ratings<- tuesdata$ratings
details<- tuesdata$details

#Research Question 
# Graphically if they is a relationship between the  max average play time to board game category

# select to details to obtain category of named board 
filter_data<-left_join(x=ratings, y=details, by =c('name'='primary'))%>%
select(year,boardgamecategory,bayes_average,maxplaytime,maxplayers)


# filtering, cleaning and selecting the needed columns for analysis 
d1<-filter_data%>%filter(!is.na(bayes_average),
                         !is.na(maxplaytime),
                         !is.na(maxplayers),
                         !is.na(boardgamecategory))%>%
  group_by(boardgamecategory)%>% mutate(Category_count=n())%>%
  summarise(bayes_average_cat=round(mean(bayes_average),2),
            maxplaytime_ave_cat=round(mean(maxplaytime),2),
            maxplayers_ave_cat= round(mean(maxplayers),0), Category_count
          )%>% mutate(boardgamecategory=gsub("[^a-zA-Z]", '',boardgamecategory))%>%
  mutate(boardgamecategory=gsub('([[:upper:]])', ' \\1',boardgamecategory))%>%
  select(boardgamecategory,
         Category_count,maxplayers_ave_cat,maxplaytime_ave_cat,bayes_average_cat)%>%
  unique()

#selecting the top 20 with most category for analyis 
df3<-head(arrange(d1,desc(Category_count)), n = 20)

#mutating and creating a negative for max play time 
df3<- mutate(df3, maxplaytime_ave_cat = -maxplaytime_ave_cat)

#plotting the graph of average max play time and category counts
graph<- ggplot(df3, aes(x = reorder(boardgamecategory, Category_count))) +
  geom_col(aes(y = Category_count), fill = "#5d8402",  size=10) +
 
  geom_col(aes(y = maxplaytime_ave_cat), fill = "#817d79")+

coord_flip()+
  scale_y_continuous(breaks = seq(-1500, 1500, 200), 
                     labels = abs(seq(-1500, 1500, 200)))+
  labs(x = "  ", y = " ", title = "Average Play time & Category counts", caption = "Data Source: Kaggle ")+
  theme(
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill =  "grey90"))+
  geom_text(aes(x=5, y=-505, label='Play Time'), color="#817d79", angle='90')+
  geom_text(aes(x=7, y=505, label='Number of Category'), color="#5d8402", angle='90')
ggsave("week.png", graph, path = "C:/Users/nikky/Desktop/R_Tuesday/Tidy  Tuesday")
