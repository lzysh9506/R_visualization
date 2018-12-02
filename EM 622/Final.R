library(dplyr)
library(tidyr)
library(ggplot2)
library(tmaptools)
library(readr)
#Population of World and United States from 2000 to 2015
Population_2015 <- read.csv("E:/SIT/EM622/Final/Population2015.csv",stringsAsFactors = F)
pop <- ggplot(data = Population_2015) + geom_line(aes(x = ï..Year, y = Unitied_States_Population.million.), size = 1.3, colour = "mediumorchid3") +
  geom_point(aes(x = ï..Year, y = Unitied_States_Population.million.), size = 2.5,colour = "mediumorchid3") +
  geom_line(aes(x = ï..Year, y = World_Population.million./210), size = 1.3, colour = "darkorange2") +
  geom_point(aes(x = ï..Year, y = World_Population.million./210), size = 2.5, colour = "darkorange2")
pop <- pop + scale_y_continuous('Unitied States Population(million)', sec.axis = sec_axis(~.*200+4000, name = "World Population(million)"))

pop <- pop + theme_bw() + xlab('Year') + ggtitle('Population of World and United States')+ 
  theme(plot.title = element_text(size=18, face="bold"),
        axis.text=element_text(size=12, color = 'black'), 
        axis.title=element_text(size=14,face="bold"), legend.position="none")
pop <- pop + annotate("text", label = "United States", x = 2010, y = 312.1, size = 6, angle = 18.8, colour = "mediumorchid3") +
  annotate("text", label = "World", x = 2010, y = 334.4, size = 6, angle = 25, colour = "darkorange2")
pop

#Numeric Change in Population and Components of Population Change
theme_clean <- function(base_size = 12) {
  require(grid) # Needed for unit() function
  theme_grey(base_size) %+replace%theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.length = unit(0, "cm"),
    axis.text.x = element_text(margin=unit(0, "cm")),
    axis.text.y = element_text(margin=unit(0, "cm")),
    panel.margin = unit(0, "lines"),
    plot.margin = unit(c(0, 0, 0, 0), "lines"),
    complete = TRUE
  )}

USbirthdeath2015 <- read_csv("E:/SIT/EM622/Final/USbirthdeath2015.csv")
levels(USbirthdeath2015$X1) <- c('Total population change', 'Birth', 'Death', 'Migration')
bdm <- ggplot(data = USbirthdeath2015,aes(x = X1, y = Population.million.)) + geom_bar(stat = "identity", width = 0.5, fill = 'grey50') +
  geom_text(aes(label=paste(USbirthdeath2015$Population.million. ,'million')), vjust = -0.4, size = 6) 
bdm <- bdm + annotate("text", label = "Birth", x = 1, y = 1.35, size = 9, colour = "white") +
  annotate("text", label = "Death", x = 2, y = 1.35, size = 9, colour = "white") +
  annotate("text", label = "Migration", x = 3, y = 0.6, size = 7, colour = "white") +
  annotate("text", label = "Population\nChange", x = 4, y = 1.35, size = 6, colour = "white")+
  annotate("text", label = "-", x = 1.5, y = 0.85, size = 22, colour = "grey30") +
  annotate("text", label = "+", x = 2.5, y = 0.85, size = 20, colour = "grey30") +
  annotate("text", label = "=", x = 3.5, y = 0.85, size = 20, colour = "grey30")
bdm <- bdm + theme_clean() + ggtitle('          Numeric Change in Population and Components of Population Change')
bdm 

#Death2015.txt data clean
data <- read.delim("Death2015.txt",fill = TRUE , header = TRUE)
summary(data)

data_clean <- select(data, c(2,4,6,8,10))

levels(data_clean$Ten.Year.Age.Groups) <- c("","< 1 year","1-4 years","5-14 years","15-24 years","25-34 years","35-44 years","45-54 years",
                                       "55-64 years","65-74 years","75-84 years","85+ years","Not Stated")

#Place of Death Frequency
Place <- data_clean%>%
  group_by(Place.of.Death) %>%
  summarise(Deaths = sum(Deaths))%>%
  na.omit()

a <- ggplot(Place, aes(x = reorder(Place.of.Death, Deaths), y = Deaths)) + 
  geom_col(fill = 'grey50') +coord_flip()+ylab("Death Frequency") + xlab("Place of Death") + scale_y_continuous(labels = c('0', '200 thousand', '400 thousand', '600 thousand', '800 thousand')) 
a <- a + theme_bw()+ ggtitle('Place of Death Frequency') + theme(plot.title = element_text(size=18, face="bold"),
                                                                                     axis.text=element_text(size=12, color = 'black'), 
                                                                                     axis.title=element_text(size=14,face="bold"))
a


ICD_TOP <- sort(table(data_clean$ICD.Chapter), decreasing = T)

ICD <- data_clean %>%
  filter(ICD.Chapter == 'Certain infectious and parasitic diseases', Deaths > 5)

#People die at home by ICD Chapters
library(plotly)
col <- get_brewer_pal("Set1", n = 19)
p <- plot_ly(Home_ICD_Chapter, labels = ~ICD.Chapter, values = ~Deaths, type = 'pie',
             textposition = 'inside',
             textinfo = 'label+percent',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'text',
             text = ~paste( Deaths, ' people'),
             marker = list(colors = col,
                           line = list(color = '#FFFFFF', width = 1)),
             showlegend = FALSE) %>%
  layout(title = 'People die at home by categories',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

#People die in different places by diseases
library(vcd)
place_ICD <- data_clean %>%
  select(Place.of.Death, ICD.Chapter, Deaths) %>%
  filter(ICD.Chapter %in% Home_ICD_Chapter$ICD.Chapter[1:10]) %>%
  group_by(Place.of.Death, ICD.Chapter) %>%
  summarise(Deaths = sum(Deaths)) 

place_icd_table <- matrix(place_ICD$Deaths, ncol=10,byrow=TRUE)
colnames(place_icd_table) <- place_ICD$ICD.Chapter[1:10]
rownames(place_icd_table) <- attributes(place_ICD$Place.of.Death)[[1]][2:9]
place_icd_table <- as.table(place_icd_table[1:7,])

names(attributes(place_icd_table)$dimnames) <- c("Place","ICD")

q <- mosaic(Place~ICD, data=place_icd_table,rot_labels = c(20, 0), main = 'People die in different places by diseases')

q

#Abnormal observation of Age
Home_all <- data_clean %>%
  filter(Place.of.Death == "Decedent's home") %>%
  select(c(3,4,5)) %>%
  group_by(ICD.Chapter, ICD.Sub.Chapter)%>%
  summarise(Deaths = sum(Deaths)) %>%
  arrange(desc(Deaths))

Home_ICD_Chapter <- Home_all %>%
  group_by(ICD.Chapter) %>%
  summarise(Deaths = sum(Deaths)) %>%
  arrange(desc(Deaths))
summarise(Home_ICD_Chapter[11:19,], sum(Deaths))

Chapter_Age <- data_clean %>%
  group_by(ICD.Chapter, Ten.Year.Age.Groups) %>%
  summarise(Deaths = sum(Deaths))%>%
  na.omit() %>%
  filter(Ten.Year.Age.Groups != 'Not Stated')

b <- ggplot(Chapter_Age, aes(x = Ten.Year.Age.Groups, y = Deaths)) +
  geom_col(color = 'black', fill = 'dodgerblue4') + facet_wrap(~ICD.Chapter, scales = 'free', ncol = 4) 
b <- b + theme_bw() +xlab('Age') + ggtitle('People in different age dies in disease') +
  scale_x_discrete(breaks = c("< 1 year","15-24 years","45-54 years",
                              "75-84 years")) + theme(axis.title=element_text(size=14,face="bold"))
b
