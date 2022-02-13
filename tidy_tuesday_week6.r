airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')


library(geofacet)
library(ggplot2)
### exploratory analysis based on expected outcome
airmen['state'][airmen['state']=='CN']<-'CT'
airmen['state'][airmen['state']=='In']<-'IN'
airmen['state'][airmen['state']=='VI']<-'VA'
airmen['state'][airmen['state']=='KN']<-'KY'
airmen['rank_at_graduation'][airmen['rank_at_graduation']=='Capt']<-'Captain' # captain % capt is the same 

##filtering and plotting the graph
#airmen_filter<-
  
  airmen%>%select(state,rank_at_graduation)%>%
    filter(!(state %in% c('Haiti', 'HT', 'TD', 'Unk')))%>%
    
    filter(!(rank_at_graduation %in% c('NA','Unk')))%>% 
    
    filter(!is.na(rank_at_graduation),!is.na(state) )%>%
    
    group_by(state, rank_at_graduation)%>%
    
    count(rank_at_graduation)%>%rename(total=n)%>%ungroup()%>%
    
    ggplot(aes(x=rank_at_graduation,y=total, fill = rank_at_graduation)) + 

    geom_col() +
    coord_flip() +
    theme_bw()+
    theme(#axis.text.x=element_blank(), #remove x axis labels
          #axis.ticks.x=element_blank(), #remove x axis ticks
          #axis.text.y=element_blank(),  #remove y axis labels
          #axis.ticks.y=element_blank() , #remove y axis ticks
          axis.title.y=element_blank(), #remove y axis title
          axis.title.x=element_blank() #remove x axis title
    )+
    guides(fill=guide_legend(title="Graduation Rank"))+
    labs(title = "Tuskegee Airmen in USA",
         caption = "Tuskegee Airman Challenge")+
    
    facet_geo(~ state)
  
   