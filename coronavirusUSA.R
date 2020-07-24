library(foreign) 
library(dplyr)
library(tidyverse)
library(rcompanion)
library(scales)
library(lubridate)
library(nycflights13)
library(zoo)
##library(plyr)
datas <- read.csv("C:/Users/SALEM/Downloads/covid_by_state.csv") 
View(datas)
datas2 <- select(datas, date:state, cases:deaths) 
##Counts of the variables
View(datas2)
View(datas2 %>% count(date))
View(datas2 %>% count(state))
View(datas2 %>% count(cases))
View(datas2 %>% count(deaths))


##basic Visulizations
ggplot(data = datas2)+ geom_bar(mapping = aes(x = cases)) + coord_cartesian(xlim = c(0,900), ylim = c(0,100)) + theme(axis.text.x = element_text(angle = 90))
ggplot(data = datas2)+ geom_bar(mapping = aes(x = deaths)) +coord_cartesian(xlim = c(0,50), ylim = c(0,1000)) + theme(axis.text.x = element_text(angle = 90))
bar <- ggplot(data = datas2) + geom_bar(mapping = aes(x = state, fill = state), show.legend = FALSE, width = 1) + theme(aspect.ratio = 4)+ labs(x = NULL, y = NULL)  
bar + coord_flip()


##Massachusetts
datas3 <- filter(datas2, state == "Massachusetts") 
View(datas3)
View(datas3 %>% count(cases))
View(datas3 %>% count(cases,state))
View(datas3 %>% arrange(desc(date)))
summarise(datas3, mean = mean(cases, na.rm = TRUE))
summarise(datas3, mean2 = mean(deaths, na.rm = TRUE))
ggplot(data = datas3, aes(x = date, y = cases, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
ggplot(data = datas3, aes(x = date, y = deaths, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
ggplot(data = datas3, aes(x = deaths, y = cases, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
##New York
datas4 <- filter(datas2, state == "New York") 
View(datas4)
View(datas4 %>% arrange(desc(date)))
summarise(datas4, means = mean(cases, na.rm = TRUE))
summarise(datas4, means2 = mean(deaths, na.rm = TRUE))
ggplot(data = datas4, aes(x = date, y = cases, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
ggplot(data = datas4, aes(x = date, y = deaths, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
ggplot(data = datas4, aes(x = deaths, y = cases, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
x = datas4$cases
plotNormalHistogram(x)
y = datas4$deaths
plotNormalHistogram(y)
##California
datas6 <- filter(datas2, state == "California") 
View(datas6)
View(datas6 %>% arrange(desc(date)))
ggplot(data = datas6, aes(x = date, y = cases, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 
ggplot(data = datas6, aes(x = date, y = deaths, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 100)) 

##Massachusetts + New York together
datas8 <- union(datas3, datas4) 
View(datas8)
summarise(datas8, means = mean(cases, na.rm = TRUE))
summarise(datas8, means = mean(deaths, na.rm = TRUE))
ggplot(datas8, aes(x = date, y = cases, colour=state, group=state)) + geom_point(size=2) + geom_line(size=1.3)
ggplot(datas8, aes(x = date, y = deaths, colour=state, group=state)) + geom_point(size=2) + geom_line(size=1.3)
View(datas8 %>% count(cases))
datas9 <- filter(datas8, date > "2020/02/29") 
View(datas9)
summarise(datas9, means4 = mean(cases, na.rm = TRUE))
summarise(datas9, means5 = mean(deaths, na.rm = TRUE))
ggplot(datas9, aes(x = date, y = cases, colour=state, group=state)) + geom_point(size=2) + geom_line(size=1.3)
ggplot(datas9, aes(x = date, y = deaths, colour=state, group=state)) + geom_point(size=2) + geom_line(size=1.3)
ggplot(datas9, aes(x = deaths, y = cases, colour=state, group=state)) + geom_point(size=2) + geom_line(size=1.3)
z = datas8$cases
plotNormalHistogram(z)

