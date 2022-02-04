library(dplyr)
library(tidyverse)
library(ggalluvial)
library(purrr)
library(ggpubr)
##loading the data set
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv')
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv')

## removing the wide spaces--
breed_rank_all$breed <- trimws(gsub(pattern = "\\s",   
                                    replacement = "",
                                    x = breed_rank_all$Breed), which = c("both"))
breed_traits$breed <- trimws(gsub(pattern = "\\s",   
                                  replacement = "",
                                  x = breed_rank_all$Breed), which = c("both"))
# join the breed trait and breed_rank

breed_trait_rank<-breed_traits%>%inner_join(breed_rank_all, by=c('breed'='breed'))
#pivoting year and rank -pivot long
pvoted_breed_year<-breed_trait_rank%>% pivot_longer(cols=starts_with('20'), 
                                 names_to='year',
                                 values_to ='Rank', 
                                 values_drop_na=TRUE)
#pivoting the for features ---pivot long 
pvoted_breed_year_new<-pvoted_breed_year%>% 
pivot_longer(cols=!c("Breed.x" ,"breed","Breed.y", "links", "Image" , "year", "Rank", "Coat Type" ,
                                          "Coat Length"  ), 
                                  names_to='Breed_feature',
                                  values_to ='Rating', 
                                  values_drop_na=TRUE)%>% 
                            mutate(Year=  parse_number(year), Breed=Breed.x)%>%
                            select(Breed, Breed_feature, Year,Rating,Rank,Image)%>%
                            unique
###plotting the top 10 features
Circle2<-pvoted_breed_year_new%>%filter(Rank <=10 & Year==2013)%>%select(Breed,Year, Rank)%>%
  group_by(Year)%>%
  unique()%>%
  ggplot(aes(x=Breed, y=factor(-Rank), fill=factor(Breed)))+
  geom_col(width = 1, color = "white")+coord_polar()+
  labs(
    x = "",
    y = "",
    title = " "
   # subtitle = "Top 10 Dog in 2013"
   
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12)
  )

###Selecting the top 5 and top 3 rated features

df<-pvoted_breed_year_new%>%filter(Rank <=5 & Rating<=3 &Year==2013)%>%select(Breed,Breed_feature, Rank, Rating)%>%
  group_by(Breed_feature)%>%
  unique()

#plotting the top 3 rated features
circle1<-df%>%ggplot(
  aes(axis1 = reorder(Breed, Rank), axis2 = Breed_feature, y = Rating/100)) +
  scale_x_discrete(limits = c("Breed", "Breed features"),  expand = c(.05, .05)) +
  geom_alluvium(aes(fill = factor(Rating)), show.legend = TRUE) +
  geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
   geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
  
 # theme_void() + 
  ggtitle(" ") +
theme(
  legend.position = "none",
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_text(face = "bold"),
  plot.title = element_text(size = 24, face = "bold"),
  plot.subtitle = element_text(size = 12)
)+
  guides(fill=guide_legend(title="Features"))

# combine the two graph 

figure<-ggarrange(circle1, Circle2,
          common.legend = TRUE, legend = "bottom")
 ggraph<- annotate_figure(figure,
                  top = text_grob("Visualizing top 10 Dogs with top rated 3 features", color = "red", face = "bold", size = 14),
                  bottom = text_grob("Data source: \n American Kennel Club courtesy of KKakey", color = "blue",
                                     hjust = 1, x = 1, face = "italic", size = 10)
                  
  )
 ##saving the chart
  ggsave("week5.png", ggraph, path = "C:/Users/nikky/Desktop/R_Tuesday/Tidy  Tuesday")


