data <- readr::read_csv('https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/challenge/2022/challenge10/data.csv')


## exploratory data analysis 

data['group'] <- 'Percent_Enrolled'  ## creating a number column

## inserting creating a new columns and inserting a value based on the benchmark
data<-data%>% mutate(end=case_when(Year==1876 ~162.4, Year==1886~114.95, Year==1896~96.86)) 
data<-data%>% mutate(start=case_when(Year==1876 ~200, Year==1886~200, Year==1896~200))

### Adding a new columns for non enrolled negro population
data<-data%>%add_row(Year=1876,`Percent Enrolled`= 62.4, group='Not_Enrolled _percent', end=100, start=162.4)
data<-data%>%add_row(Year=1886,`Percent Enrolled`= 43.3, group='Not_Enrolled _percent', end=50, start=114.95)
data<-data%>%add_row(Year=1896,`Percent Enrolled`= 42.7, group='Not_Enrolled _percent', end=20,start=96.86)

## renaming enrolled percent columns and creating a sequence id for each group
data<-data%>%rename(total='Percent Enrolled')%>%
mutate(id=case_when(Year==1876 ~1, Year==1886~2, Year==1896~3))%>%
group_by(Year, id)%>%arrange((Year))

## creating upper bound limit columns and assigne the value to 200
data['bound']<-200

## selecting only the needed columns

data<-data%>% select (Year,group,id,start, total, end, bound)

###plotting the needed graph 
P<-ggplot(data, aes(x = as.factor(Year), y=bound)) + 
  geom_rect(aes(x = as.factor(Year),
                xmin = id - 0.25, # control bar gap width
                xmax = id + 0.25, 
                ymin = end,
                ymax =start,
                fill=as.factor(group)
                )
  )+scale_fill_manual(values=c("#000a0a",
                              "#C70039"
                          ))+
  geom_text(aes(label = factor(Year)), position = position_dodge(width = 0.9), vjust =-0.5)+
  annotate("text", x = 1, y = 175, label = '37.59%')+
  annotate("text", x = 2, y = 150, label = '56.66%')+
  annotate("text", x = 3, y = 160, label = '57.29%')+
  annotate("rect", xmin =0.7, xmax = 1, ymin = 40, ymax = 50,
         fill = "#C70039")+
  annotate("rect", xmin =0.7, xmax = 1, ymin = 35, ymax = 25,
           fill = "#000a0a")
P<-P+annotate("text", x = 1.2, y = 49, label = 'of Children enrolled', size=3)+
  annotate("text", x = 1.2, y = 44, label = "d'enfants inscrits", size=3) +
  annotate("text", x = 0.55, y = 49, label = "Proportion", size=3)+
  annotate("text", x = 0.55, y = 44, label = "Proportion", size=3)

P<-P+annotate("text", x = 1.2, y = 35, label = 'of Children not enrolled', size=3)+
  annotate("text", x = 1.2, y = 30, label = "d'enfants Non inscrits", size=3) +
  annotate("text", x = 0.55, y = 35, label = "Proportion", size=3)+
  annotate("text", x = 0.55, y = 30, label = "Proportion", size=3)

p<-P+ theme(axis.text.x=element_blank(), #remove x axis labels
         axis.ticks.x=element_blank(), #remove x axis ticks
         axis.text.y=element_blank(),  #remove y axis labels
         axis.ticks.y=element_blank() , #remove y axis ticks
         axis.title.y=element_blank(), #remove y axis title
         axis.title.x=element_blank(), #remove x axis title)
         legend.position="none")
p<-p+labs(title = "PROPORTION OF TOTAL NEGRO CHILDREN OF SCHOOL OF AGE WHO ENROLLED IN THE PUBLIC SCHOOL\n
          PROPORTION DES ENFANTS NEGRE EN AGE D ECOLE ENEISTRES DANS LES ECOLES PUBLIQUES\n", subtitle = "DONE BY ATLANTA UNIVERSITY", caption= 'Challenge 10')
p+ theme(plot.subtitle  = element_text(hjust = 0.5),plot.title  = element_text(hjust = 0.5) )     