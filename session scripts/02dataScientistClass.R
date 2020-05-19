####################################################
## Data Scientist with R
## Doctorate course in Chemical Sciences - 2019-2020
## Università degli Studi di Perugia
## 
## Gianandrea La Porta - gianandrea.laporta@unipg.it
## Dipartimento di Chimica Biologia e Biotecnologie
####################################################


# Lesson #2
# 16 March 2020

# Little recap
# assignment
p <- 10

# combine values
m <- c(1,2)
m*p

# <- alt+-
assign('p',10)

m <- c(1,2)

#Alt+Shift+k

# new session
library(tidyverse)

data('airquality')
?airquality
airquality
View(airquality)

# as_tibble
airq <- as_tibble(airquality)
airq

#wrangling functions

# filter function ---------------------------------------------------------

filter(airq,Month==6) # is equal to 6

filter(airq,Month!=6) # is not equal to 6

p <- 10
p = 10

filter(airq,Month=6) #ERROR

# 6 possibile operators
# >, >=, <, <=, !=, ==

filter(airq, Month>=8)
filter(airq, Month<=5)
new <- filter(airq, Month<=5)

filter(airq, Month==6, Day==8)

filter(airq, Ozone>100, Temp>70)
filter(airq, Ozone>100 & Temp>70) # AND condition
filter(airq, Ozone>100 | Temp>70) # OR condition

data(iris)
library(tidyverse)
iristb <- as_tibble(iris)
# hist()
# Petal.length > 5
# AND Petal.length < 2

iris25 <- filter(iristb, Petal.Length < 2 | Petal.Length > 5)
hist(iris25$Petal.Length, col=3, xlab='Petal Length (mm)', ylab='Number',
     main='My histogram')

hist(iris$Petal.Length)

# filter(airq, Ozone>100, Temp>70)
# histogram of Ozone var
# and compare with hist of the whole Ozone dataset
ozone1 <- filter(airq, Ozone>100, Temp>70)

par(mfrow=c(1,2))
hist(airq$Ozone, col= 5)
hist(ozone1$Ozone, col=4)

# SELECT ------
# 
select(airq,Ozone, Temp)
select(airq,Ozone, Temp, Wind)
select(airq, Temp, Ozone)
plot(select(airq, Temp, Ozone), pch=16)
plot(select(airq, Ozone, Temp), pch=16)

# helper functions starts_with, contains
select(airq, contains('o'))
select(airq, starts_with('o'))

select(airq, Temp, everything())


# ARRANGE -----------------------------------------------------------------

arrange(airq, Temp)
arrange(airq, Temp, Ozone)

arrange(airq, desc(Temp))


# SUMMARISE ---------------------------------------------------------------

mean(airq$Temp)
mean(airq$Ozone)
mean(airq$Ozone, na.rm = TRUE)

summarise(airq, ozone.avg=mean(Ozone, na.rm=TRUE))

# group_by
airq.month <- group_by(airq, Month)
summarise(airq.month, ozone.avg=mean(Ozone, na.rm=TRUE))
airq.summary <- summarise(airq.month, ozone.avg=mean(Ozone, na.rm=TRUE))
plot(airq.summary)
plot(airq.summary, type='b', pch=5, cex=2, col=2)

# plot average values calculated by Species of Sepal.Length
# 1. group_by
# 2. summarise
# 3. plot

iristb
iris.species <- group_by(iristb, Species)
iris.summ <- summarise(iris.species, sepal.avg=mean(Sepal.Length))
plot(iris.summ)

# ----
byMonth <- group_by(airq, Month)
tempWind <- summarise(byMonth,
                      count=n(),
                      wind=mean(Wind, na.rm=T),
                      temp=mean(Temp, na.rm=T))
plot(tempWind$temp,tempWind$wind, pch=16, cex=3)

# pipe %>% Ctrl+Shit+M

select(airq, Ozone)

airq %>% 
  select(Ozone)


tempWind <- airq %>% 
  group_by(Month) %>% 
  summarise(
    count=n(),
    temp=mean(Temp),
    wind=mean(Wind, na.rm=T)
  )
plot(tempWind$temp,tempWind$wind, pch=16, cex=3)


# IRIS
iris.avg <- iris %>% 
  group_by(Species) %>% 
  summarise(avg=mean(Sepal.Length)) %>% 
  arrange(desc(avg))


# MUTATE ------------------------------------------------------------------

airq %>% 
  mutate(tempC=(Temp-32)*5/9)


# EXPORT DATA -------------------------------------------------------------

write_csv(iris.avg,'irisAvg.csv')

library(xlsx)
write.xlsx(iris.avg,'irisAvg.xlsx')

# IMPORT DATA -------------------------------------------------------------

newImportedFile <- read_csv('irisAvg.csv')



