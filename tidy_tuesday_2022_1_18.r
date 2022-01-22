tuesdata <- tidytuesdayR::tt_load('2022-01-18')
chocolate <- tuesdata$chocolate
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
library(viridis)
library(ggpubr)
library(ggpubr)
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(wordcloud)
library(stylo)
#extracting first number of ingredient

chocolate$ing<-str_extract(chocolate$ingredients,"^\\d{1}")
chocolate1<-chocolate

#number of columns after splitting by space
ncols <- max(stringr::str_count(chocolate1$most_memorable_characteristics, " ")) + 1

#generate necessary column names
colmn <- paste("col", 1:ncols)

df<-tidyr::separate(
  data = chocolate1,
  col = most_memorable_characteristics,
  sep = ",",
  into = colmn, fill = "right",convert = TRUE,
  remove = FALSE
)
#sub-setting for Ingredient one

cho_with_one <- df%>%filter(ing==1)%>% select("col 1" , "col 2","col 3", "col 4", "col 5","col 6")

## unlist the memorable characteristics
cho_with_one <- unlist(cho_with_one)

#Creating a frequency  using table function
cho_with_one<- table(cho_with_one)

#converting the table to dataframe
cho_with_one<-as.data.frame(cho_with_one)

#Reordering dataset 
cho_with_one<-cho_with_one%>%arrange(desc(Freq))

#removing the whitespace
cho_with_one$word<- str_trim(cho_with_one$cho_with_one)

#### Remove word pronouns and nouns
cho_with_one<-cho_with_one %>% anti_join(stop_words,by= c("word"="word"))

#extracting sentiments words
bing <- get_sentiments("bing")

#inner join with our data

cho_with_one<- cho_with_one %>%
inner_join(bing, by=c("word"="word"))

#plot bar chart for ingredient one sentiment
bar_with_one<-cho_with_one %>%
  mutate(n = ifelse(sentiment == "negative", -Freq, Freq)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(width = 0.5) + 
  theme(axis.text.y = element_text(size = 8), axis.text.x  = element_text(size = 8)) +
  coord_flip() +
  labs(y = " ", x=" ")+annotate("text", x = 1, y = -2.5,  col = "red", size = 3,angle='90', label = " Ingredients one")

#sub-setting for Ingredient two

cho_with_two <- df%>%filter(ing==2)%>% select("col 1" , "col 2","col 3", "col 4", "col 5","col 6")

## unlist the memorable characteristics
cho_with_two <- unlist(cho_with_two)

#Creating a frequency  using table function
cho_with_two<- table(cho_with_two)

#converting the table to dataframe
cho_with_two<-as.data.frame(cho_with_two)

#Reordering dataset 
cho_with_two<-cho_with_two%>%arrange(desc(Freq))

#removing the whitespace
cho_with_two$word<- str_trim(cho_with_two$cho_with_two)

#### Remove word pronouns and nouns
cho_with_two<-cho_with_two %>% anti_join(stop_words,by= c("word"="word"))

#extracting sentiments words
bing <- get_sentiments("bing")

#inner join with our data

cho_with_two<- cho_with_two %>%
  inner_join(bing, by=c("word"="word"))

#plot bar chart for ingredient one sentiment
bar_with_two<-cho_with_two %>%filter(Freq>5)%>%
  mutate(n = ifelse(sentiment == "negative", -Freq, Freq)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(width = 0.5) +
  theme(axis.text.y = element_text(size = 8), axis.text.x  = element_text(size = 8)) +
  coord_flip() +
  labs(y = " ", x=" ")+annotate("text", x = 10, y = 25, angle='90',  col = "red", size = 3, label = " Ingredients two")

#sub-setting for Ingredient three

cho_with_three <- df%>%filter(ing==3)%>% select("col 1" , "col 2","col 3", "col 4", "col 5","col 6")

## unlist the memorable characteristics
cho_with_three <- unlist(cho_with_three)

#Creating a frequency  using table function
cho_with_three<- table(cho_with_three)

#converting the table to dataframe
cho_with_three<-as.data.frame(cho_with_three)

#Reordering dataset 
cho_with_three<-cho_with_three%>%arrange(desc(Freq))

#removing the whitespace
cho_with_three$word<- str_trim(cho_with_three$cho_with_three)

#### Remove word pronouns and nouns
cho_with_three<-cho_with_three %>% anti_join(stop_words,by= c("word"="word"))

#extracting sentiments words
bing <- get_sentiments("bing")

#inner join with our data

cho_with_three<- cho_with_three %>%
  inner_join(bing, by=c("word"="word"))

#plot bar chart for ingredient three sentiment
bar_with_three<-cho_with_three %>%filter(Freq>6)%>%
  mutate(n = ifelse(sentiment == "negative", -Freq, Freq)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(width = 0.5)+ 
  theme(axis.text.y = element_text(size = 8),axis.text.x  = element_text(size = 8)) +
  coord_flip() +
  labs(y = " ", x=" ")+annotate("text", x = 5, y = 30, angle='90',  col = "red", size = 3, label = " Ingredients three")

#sub-setting for Ingredient four

cho_with_four <- df%>%filter(ing==4)%>% select("col 1" , "col 2","col 3", "col 4", "col 5","col 6")

## unlist the memorable characteristics
cho_with_four <- unlist(cho_with_four)

#Creating a frequency  using table function
cho_with_four<- table(cho_with_four)

#converting the table to dataframe
cho_with_four<-as.data.frame(cho_with_four)

#Reordering dataset 
cho_with_four<-cho_with_four%>%arrange(desc(Freq))

#removing the whitespace
cho_with_four$word<- str_trim(cho_with_four$cho_with_four)

#### Remove word pronouns and nouns
cho_with_four<-cho_with_four %>% anti_join(stop_words,by= c("word"="word"))

#extracting sentiments words
bing <- get_sentiments("bing")

#inner join with our data

cho_with_four<- cho_with_four %>%
  inner_join(bing, by=c("word"="word"))

#plot bar chart for ingredient one sentiment
bar_with_four<-cho_with_four %>%filter(Freq>3)%>%
  mutate(n = ifelse(sentiment == "negative", -Freq, Freq)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(width = 0.5)+ 
  theme(axis.text.y = element_text(size = 8),axis.text.x  = element_text(size = 8)) +
  coord_flip() +
  labs(y = " ", x=" ")+annotate("text", x = 5, y = 30, angle='90',  col = "red", size = 3, label = " Ingredients four")


#sub-setting for Ingredient five

cho_with_five <- df%>%filter(ing==5)%>% select("col 1" , "col 2","col 3", "col 4", "col 5","col 6")

## unlist the memorable characteristics
cho_with_five <- unlist(cho_with_five)

#Creating a frequency  using table function
cho_with_five<- table(cho_with_five)

#converting the table to dataframe
cho_with_five<-as.data.frame(cho_with_five)

#Reordering dataset 
cho_with_five<-cho_with_five%>%arrange(desc(Freq))

#removing the whitespace
cho_with_five$word<- str_trim(cho_with_five$cho_with_five)

#### Remove word pronouns and nouns
cho_with_five<-cho_with_five %>% anti_join(stop_words,by= c("word"="word"))

#extracting sentiments words
bing <- get_sentiments("bing")

#inner join with our data

cho_with_five<- cho_with_five %>%
  inner_join(bing, by=c("word"="word"))

#plot bar chart for ingredient one sentiment
bar_with_five<-cho_with_five %>%filter(Freq>2)%>%
  mutate(n = ifelse(sentiment == "negative", -Freq, Freq)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(width = 0.5) +
  theme(axis.text.y = element_text(size = 8),axis.text.x  = element_text(size = 8)) +
  coord_flip() +
  labs(y = " ", x= " ")+annotate("text", x = 5, y = 20, angle='90',  col = "red", size = 3, label = " Ingredients five")

#sub-setting for Ingredient six

cho_with_six <- df%>%filter(ing==6)%>% select("col 1" , "col 2","col 3", "col 4", "col 5","col 6")

## unlist the memorable characteristics
cho_with_six <- unlist(cho_with_six)

#Creating a frequency  using table function
cho_with_six<- table(cho_with_six)

#converting the table to dataframe
cho_with_six<-as.data.frame(cho_with_six)

#Reordering dataset 
cho_with_six<-cho_with_six%>%arrange(desc(Freq))

#removing the whitespace
cho_with_six$word<- str_trim(cho_with_six$cho_with_six)

#### Remove word pronouns and nouns
cho_with_six<-cho_with_six %>% anti_join(stop_words,by= c("word"="word"))

#extracting sentiments words
bing <- get_sentiments("bing")

#inner join with our data

cho_with_six<- cho_with_six %>%
  inner_join(bing, by=c("word"="word"))

#plot bar chart for ingredient one sentiment
bar_with_six<-cho_with_six %>%
  mutate(n = ifelse(sentiment == "negative", -Freq, Freq)) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(width = 0.5) + 
  theme(axis.text.y = element_text(size = 5), axis.text.x  = element_text(size = 8)) +
  coord_flip() +
  labs(y = " ", x=" ") +annotate("text", x = 0.5, y =4 , angle='90',  col = "red", size = 3, label = " Ingredients six")
#+ theme(plot.margin = unit(c(2,2,2,2), "cm"))

####word 
#set.seed(1234)

#df<-chocalate_word 
#wordcloud(words = df$df, freq = df$Freq, min.freq = 10,
         # max.words=1000, random.order=FALSE, rot.per=0.35, scale=c(2,.4),
          #colors=brewer.pal(8, "Dark2"))
######Country Rating 


average_choco<-chocolate1%>%filter(!is.na(rating),!is.na(ing))%>%
  dplyr::select(ing, rating)%>%
  group_by(ing)%>%summarize(avg_rating=round(mean(rating),2))%>%
  arrange(desc(avg_rating)) %>%
  mutate(Ingredient=factor(ing,ing),Avg_rating=factor(avg_rating,avg_rating))%>%
  dplyr::select(Ingredient, Avg_rating)

colar_av<- average_choco%>%
  ggplot( aes(x=Ingredient,y=Avg_rating, fill=Avg_rating ))+
  geom_bar( width = 1,stat='identity', color='white')+
  theme_light()+
  
  theme(axis.title.y=element_text(angle=0, vjust = 0.5))+ 
  scale_fill_brewer(palette="Set1", name = "Average rating\n Ingredients level")+ theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))+ coord_polar()+ aes(x=reorder((Ingredient),(Avg_rating)))+ 
  theme(axis.text.x=element_text(angle=-20))+ xlab("Ingredient Levels") + ylab(" ")


## combing the graphs 

text <- paste("Six ingredient has one word only named 'sweet'",
              "so i decided not illustrate it but the code is included",
              "in my github. one interesting thing about this",
              " sentimental analysis is that as more ingredients is added",
              "the negative words decreases, so one may think.",
              " that the average rating will respond accordingly,",
              "sure it does respond postively but upto ingredient three.",
              " The code for this illustration can found in my github.",
              " the data source for the analyis is from,",
              "  Flavors of Cacao by way of Georgios and Kelsey",sep = " ")
text.p <- ggparagraph(text = text, face = "italic", size = 8, color = "black")
ggarrange(bar_with_two,bar_with_one,
           bar_with_three,bar_with_four,bar_with_five,text.p,
          common.legend = TRUE, legend = "bottom")




