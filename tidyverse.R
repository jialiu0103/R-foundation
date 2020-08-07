library(tidyverse)
iris %>% head(5)
head(iris,5)
dim(iris)

###filter 
iris %>% filter(Species=='setosa')
iris[iris$Species=='setosa',]
iris %>% filter(Species=='setosa',Sepal.Length>5)
iris[iris$Species=='setosa' & iris$Sepal.Length>5,]

###select
iris %>% select(Species, Sepal.Length)
iris[,c('Sepal.Length','Species')]
iris %>% select(-Sepal.Width)
head(iris[,-2])

###filter -- select
iris %>% filter(Species=='setosa',Sepal.Length>5.5) %>% select(Sepal.Width)
iris[iris$Species=='setosa' & iris$Sepal.Length>5.5,'Sepal.Width']

###mutate
iris %>% mutate(space=Sepal.Length*Sepal.Width) %>% select(-Sepal.Length,-Sepal.Width)
head(iris)
iris$space=iris$Sepal.Length*iris$Sepal.Width
iris[,c(-1,-2)]

###summarise space that >15
iris %>% mutate(space=Sepal.Length*Sepal.Width) %>% filter(space>15) %>% summarise(count=n(),mean(space))
iris$space=iris$Sepal.Length*iris$Sepal.Width
mean(iris$space[iris$space>15])
length(iris$space[iris$space>15])

###summarise by group that space >15
iris %>% 
  mutate(space=Sepal.Length*Sepal.Width) %>% 
  filter(space>15) %>% 
  group_by(Species) %>% 
  summarise(n(),mean(space))

###gather & spread
long_iris=iris %>% gather(length_iris,value,c(Sepal.Length,Petal.Length))
long_iris=long_iris[1:4,]
long_iris %>% spread(Sepal.Width,value)
