bike <- read.csv("~/Desktop/730_R/lec13/hw13/bikeshare.csv")
library(dplyr)
#1a
bike$year=format(as.Date(bike$dteday, format="%m/%d/%Y"),"%Y")
p1=bike %>% filter(., year=="2012")
bike_temp=p1 %>% select(temp, season, weekday, cnt, yr, windspeed, hum)
                   
#1b
bike_temp_b=bike %>% filter(., season=="1" & temp<30 &temp>5)
bike_temp_b=bike_temp_b %>% select(temp, season, weekday, cnt, yr, windspeed, hum)

#1c
p2=bike_temp %>%
  select(temp) %>%
  mutate(tempf = temp*(9/5) + 32)
bike_temp$tempf=p2$tempf

#1d
p3=bike %>%
  filter(.,windspeed<10 | windspeed==10) %>%
  mutate(idealday = 1)
p4=bike %>%
  filter(.,windspeed>10) %>%
  mutate(idealday = 0)
p5=full_join(p3,p4)
p3_0=p3[p3$weekday==0,] %>% select(.,c(obsn, dteday))
p3_1=p3[p3$weekday==1,] %>% select(.,c(obsn, dteday))
#p4=as.Date(as.character(p3_0[2,2]), format="%m/%d/%Y")-as.Date(as.character(p3_1[2,2]), format="%m/%d/%Y")

pointer=vector( mode = "list" )
for (i in p3_0$dteday){
  for (j in p3_1$dteday) {
    if (as.Date(as.character(j), format="%m/%d/%Y")-
        as.Date(as.character(i), format="%m/%d/%Y")==1){
      pointer=append(pointer,j)}}}

p3_2=p3[p3$weekday==2,] %>% select(.,c(obsn, dteday))
p3_3=p3[p3$weekday==3,] %>% select(.,c(obsn, dteday))
pointer1=list()
for (i in pointer){
  for (j in p3_2$dteday) {
    if (as.Date(as.character(j), format="%m/%d/%Y")-
        as.Date(as.character(i), format="%m/%d/%Y")==1){
      pointer1=append(pointer1,j)}}}

pointer2=list()
for (i in pointer1){
  for (j in p3_3$dteday) {
    if (as.Date(as.character(j), format="%m/%d/%Y")-
        as.Date(as.character(i), format="%m/%d/%Y")==1){
      pointer2=append(pointer2,j)}}}

p3_4=p3[p3$weekday==4,] %>% select(.,c(obsn, dteday))
p3_5=p3[p3$weekday==5,] %>% select(.,c(obsn, dteday))
pointer3=list()
for (i in pointer2){
  for (j in p3_4$dteday) {
    if (as.Date(as.character(j), format="%m/%d/%Y")-
        as.Date(as.character(i), format="%m/%d/%Y")==1){
      pointer3=append(pointer3,j)}}}

pointer4=list()
for (i in pointer3){
  for (j in p3_5$dteday) {
    if (as.Date(as.character(j), format="%m/%d/%Y")-
        as.Date(as.character(i), format="%m/%d/%Y")==1){
      pointer4=append(pointer4,j)}}}

p3_6=p3[p3$weekday==6,] %>% select(.,c(obsn, dteday))
pointer5=list()
for (i in pointer4){
  for (j in p3_6$dteday) {
    if (as.Date(as.character(j), format="%m/%d/%Y")-
        as.Date(as.character(i), format="%m/%d/%Y")==1){
      pointer5=append(pointer5,j)}}}

lenweek=length(pointer5)
perscent=100*lenweek/52
