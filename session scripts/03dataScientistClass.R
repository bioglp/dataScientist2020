####################################################
## Data Scientist with R
## Doctorate course in Chemical Sciences - 2019-2020
## Università degli Studi di Perugia
## 
## Gianandrea La Porta - gianandrea.laporta@unipg.it
## Dipartimento di Chimica Biologia e Biotecnologie
####################################################

# Lesson #3
# 23 March 2020

# DATA VISUALIZATION -------------------------------

# two basic types:
# 1. exploratory visualization
# 2. explanatory visualization

library('MASS')
install.packages('MASS')
# 
str(whiteside)

library(tidyverse)
insul.bef <- filter(whiteside, Insul=='Before')
insul.aft <- filter(whiteside, Insul=='After')

plot(insul.bef$Temp, insul.bef$Gas, pch='b')
points(insul.aft$Temp, insul.aft$Gas, pch='a')

plot(whiteside)

plot(whiteside$Temp, whiteside$Gas,
     xlab = 'Outside temperature',
     ylab = 'Heating gas consumption',
     main = 'Effect of insulation on gas consumption',
     col=whiteside$Insul,
     pch=as.numeric(whiteside$Insul)
     )

plot(whiteside$Insul) 


Cars93
str(Cars93)

car93 <- as_tibble(Cars93)
detach('package:MASS',unload = TRUE) # unload a package

car93

# first plot Max.Price vs Price as red triangles
plot(car93$Price,car93$Max.Price, pch=17, col='red')

# add Min.Price vs Price as blue circles (16)
points(car93$Price,car93$Min.Price, pch=16, col='blue')

# abline() y-intercept=0 and slope=1
abline(a=0,b=1, lty=2)

# tidyverse and pipe version
car93 %>% 
  select(Price,Max.Price) %>% 
  plot(pch=17,col='red')
car93 %>% 
  select(Price,Min.Price) %>% 
  points(pch=16,col='blue')
abline(a=0,b=1, lty=2)
title('Max and min prices vs Price')

# add a secondary axis
axis(side=4)

# margin option
par(mar=c(5,4,4,4))

car93 %>% 
  select(Price,Max.Price) %>% 
  plot(pch=17,col='red')
car93 %>% 
  select(Price,Min.Price) %>% 
  points(pch=16,col='blue')
abline(a=0,b=1, lty=2)
title('Max and min prices vs Price')

# add a secondary axis
axis(side=4)
mtext('Min.Price', side=4, line=3)

# multiple plot 
par(mfrow=c(1,2)) # 1 row, 2 cols

library(robustbase)
# install.packages(robustbase)
data("Animals2")
str(Animals2)

plot(Animals2$body,Animals2$brain)

Animals2 %>% 
  select(body,brain) %>% 
  plot()

Animals2 %>% 
  select(body,brain) %>% 
  plot(log='xy')
# add a title
title('Log-log plot')

# other options are log='x' or log='y'

options(scipen = 999)

Animals2 %>% 
  select(body,brain) %>% 
  plot(log='xy')
# add a title
title('Log-log plot')

# col='forestgreen'
# cex=2 ; character expansion
# panel.first=grid() # secondary grid

Animals2 %>% 
  select(body,brain) %>% 
  plot(log='xy', col='forestgreen',
       cex=2, panel.first=grid())
title('Log-log plot')
abline(a=0,b=1,lty=2)

Animals3 <- as_tibble(rownames_to_column(Animals2))

points(1,1,cex=2,col=4,pch=16)

anim.below <- Animals3 %>% 
  mutate(ratio=brain/body) %>% 
  filter(ratio<1)

points(anim.below$body,anim.below$brain,pch=16,col='forestgreen',cex=2)
text(anim.below$body,anim.below$brain,  # position of the text
     labels = anim.below$rowname, # name of the labels
     adj=1.2) #

text(anim.below$body,anim.below$brain,  # position of the text
     labels = anim.below$rowname,
     col='red') #

?text

# PIE CHART -------
# 
library(insuranceData)
data('dataCar')
dataCar
str(dataCar)

dataCar$veh_body

par(mfrow=c(1,2))

tbl <- sort(table(dataCar$veh_body))

pie(tbl)
title('Pie chart')

barplot(tbl, las=2)
title('Bar plot')

# vector function
max.temp <- c(22,27,26,24,23,26,28)
max.temp <- combine(22,27,26,24,23,26,28) # it is the same

barplot(max.temp)

day.names <- c('Sun','Mon','Tue','Wed','Thu','Fri','Sat')
barplot(max.temp, names.arg = day.names, las=1,
        xlab='Degree celcius',
        ylab='Day',
        col='orange', horiz = TRUE)
title('Barplot')
# las: 1- horizontal, 2- perpendicular; 3- vertical

# SAVE PLOT --
# 
jpeg(file = 'barplot.jpg')
barplot(max.temp, names.arg = day.names, las=1,
        xlab='Degree celcius',
        ylab='Day',
        col='orange', horiz = TRUE)
dev.off()

tiff(file = 'barplot.tiff', res=100)
barplot(max.temp, names.arg = day.names, las=1,
        xlab='Degree celcius',
        ylab='Day',
        col='orange', horiz = TRUE)
dev.off()

pdf(file = 'barplot.pdf')
barplot(max.temp, names.arg = day.names, las=1,
        xlab='Degree celcius',
        ylab='Day',
        col='orange', horiz = TRUE)
dev.off()
