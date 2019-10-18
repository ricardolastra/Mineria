library(ggplot2movies)
library(dplyr)
library(scales)
library(plotly)

data(movies)
head(movies)
str(movies)

###### EJERCICIO 1 #########

movies_antes <- movies %>%
  group_by(year) %>%
  filter(year < 1980)

movies_despues <- movies %>%
  group_by(year) %>%
  filter(year > 1980)

ggplot(movies, aes(x=length)) +  
  scale_x_continuous(breaks = seq(from = 7, to = 90, by = 7), lim = c(7,91)) +
  geom_histogram(data=subset(movies %>%
                               group_by(year) %>%
                               filter(year < 1980)),binwidth = 1, fill = "blue", alpha = 0.5) +
  geom_histogram(data = subset(movies %>%
                                 group_by(year) %>%
                                 filter(year > 1980)), binwidth = 1, fill = "red", alpha = 0.5) +
  xlab("Duración de películas en minutos") + 
  scale_fill_gradient("movies", low = "blue", high = "red") +
  theme_bw() 


movies %>%
  group_by(length) %>%
  arrange(desc(count)) %>%
  filter(Short==1)

library(RColorBrewer)
movies2 <- movies %>%
  mutate(Short2= ifelse(Short==1,'Pelicula corta','Pelicula larga'))
ggplot(movies2, aes(x = length, fill = Short2)) +
  geom_histogram(aes(y = ..count..), binwidth = 1,
                 position="identity", alpha=0.8) +
  xlim(0,180) +
  theme_bw() +
  scale_fill_brewer(palette="Accent") +
  xlab("Duración en minutos de películas cortas y largas")

ggplot(movies2, aes(x = length, fill = Short2)) +
  geom_dotplot(method="histodot", binwidth = 2) +
  xlim(0,100)



movies2 <- movies %>%
  mutate(Short2= ifelse(Short==1,'Pelicula corta','Pelicula larga'))


str(movies2)

movies %>%
  filter(Short== 0 ) %>%
  summarise(count = n())

ggplot(movies, aes(x=votes, y=rating)) +
  geom_point(alpha=0.2) +
  ylim(1,10) + 
  scale_x_continuous(label=comma) + 
  theme_bw()  

ggplot(movies, aes(x=votes, y=rating)) +
  geom_point(alpha=0.2) +
  ylim(1,10) + 
  scale_x_continuous(breaks = seq(from = 100, to = 150000, by = 20000), lim = c(101,150000)) + 
  theme_bw()

### Ejericicio 3
library("MASS")
data("Cars93")

ggplot(Cars93, aes(x=Weight, y=MPG.city)) +
  geom_smooth(colour="green") +
  ylim(0, 50) + 
  scale_x_continuous(label=comma) +
  geom_point() +
  theme_bw() 

library(plotly)
p <- qplot(Weight, MPG.city, data=Cars93)
p <- p + geom_smooth(method = "glm", colour="green", formula = y~x, family = gaussian(link = 'log'))

p <- ggplotly(p)
p

# Ejercicio 4
install.packages("reshape")
install.packages("progress")
library(GGally)


dplyr::select(Boston, -rad, -chas) %>% 
  ggpairs(title="Boston dataset", diag=list(continuous="density", axisLabels='none'))

dplyr::select(Boston, -rad, -chas) %>% 
  ggpairs(title="Boston dataset", diag=list(continuous="density", axisLabels='none'))

ggplot(Boston, aes(x=crim, y=age)) +
  geom_point(color="blue",alpha=0.2) +
  ylim(18,70) +
  xlim(0,5) +
  theme_bw()

ggplot(Boston, aes(x=black, y=medv)) +
  geom_point(color="red",alpha=0.2) +
  xlim(350,400) +
  theme_bw()


ggplot(Boston, aes(x=medv, y=tax)) +
  geom_point(color="purple",alpha=0.2) +
  theme_bw()

ggplot(Boston, aes(x=crim, y=rm)) +
  geom_point(alpha=0.2) +
  theme_bw()


ggplot(Boston, aes(x=dis, y=nox)) +
  geom_point(color="orange",alpha=0.8) +
  theme_bw()


Boston1 <- Boston
names(Boston1) <- c(abbreviate(names(Boston)[1:5]), "rad")
str(Boston1)
a1 <- ggparcoord(Boston, columns = 1:5, 
                 alphaLines = 0.7,  
                 groupColumn = "rad") + 
  ggtitle("General")
a2 <- ggparcoord(Boston, columns = 1:5, 
                 scale="uniminmax", 
                 alphaLines=0.7, 
                 groupColumn = "rad") + 
  ggtitle("Esc1")
a3 <- ggparcoord(Boston, columns = 1:5, 
                 scale="globalminmax", 
                 alphaLines=0.7, 
                 groupColumn = "rad") + 
  ggtitle("Esc2")
a4 <- ggparcoord(Boston, columns = 1:5, 
                 scale="center", 
                 scaleSummary="median", 
                 alphaLines=0.7, 
                 groupColumn = "rad") +
  ggtitle("Esc3")

gridExtra::grid.arrange(a1, a2, a3, a4)
