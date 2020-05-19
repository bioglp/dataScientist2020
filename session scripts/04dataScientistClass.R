####################################################
## Data Scientist with R
## Doctorate course in Chemical Sciences - 2019-2020
## Università degli Studi di Perugia
## 
## Gianandrea La Porta - gianandrea.laporta@unipg.it
## Dipartimento di Chimica Biologia e Biotecnologie
####################################################

# Lesson #4
# 30 March 2020

# Descriptive statistics --- 
# 
# Population
# Sample
# Observations - rows are obs
# Variables - cols are var

p <- c(5, 2, 7, 4, 1, 8, 5, 6, 3, 5, 4)
p

# Mode - highest frequency ----
table(p)

library(tidyverse)
tbl <- tibble(x=p)
tbl

airq <- as_tibble(airquality)
airq

# Median ----

sort(p)
median(p)

median(airq$Temp)

airq$Temp %>% 
  median()

# Mean (average) ----

p
sum(p)/length(p)

mean(p)

airq$Temp %>% 
  mean()

airq %>% 
  summarise(
    median.T=median(Temp),
    mean.T=mean(Temp),
    median.O=median(Ozone, na.rm = T), #NA ?
    mean.O=mean(Ozone, na.rm = T)
  )

# Trimmed mean ----
mean(p, trim = 0.3) # 10%
median(p)

# Olympic scoring

# Range ---
range(p)
diff(range(p))

max(p)-min(p)

# Interquartile range ----

x <- 0:100
y <- rep(1,length(x))

plot(x,y,
     type='l',col='grey',yaxt='n',
     main='Vector x distribution')
text(seq(0,100,10),1,labels=as.character(sort(p)), font=4)

#median
points(50,1,cex=4,col='red')
abline(v=50, col=2, lty=2)

points(c(25,75), c(1,1), cex=4, col=3)

quantile(p)

quantile(p, 0.25)
text(25,1,'Q1',col='grey')

quantile(p, 0.75)
text(75,1,'Q3',col='grey')

quantile(p,0.75)-quantile(p,0.25)
IQR(p)

arrows(25,1.1,75,1.1,angle = 90, code=3,col=3)
text(50,1.2,'IQR',col='green')

# Variance ----

p-mean(p)
sum(p-mean(p)) # zero

num <- sum((p-mean(p))^2)
den <- length(p)-1
num/den

var(p) # variance

# stardard deviation / spread of the data
sqrt(var(p))
sd(p)

# Boxplot ----

boxplot(airq$Ozone, col='yellow', notch = T)
# points(..., airq$Ozone)

# ... are equal to = rep(1,length(airq$Ozone))
points(rep(1,length(airq$Ozone)), airq$Ozone)

# add random noise - jitter() ----
points(jitter(rep(1,length(airq$Ozone)), 3), airq$Ozone)

airq$Ozone %>% 
  boxplot(main='Median ozone',
          xlab='Parts per million',
          ylab='Ozone',
          col='orange',
          border = 'brown',
          horizontal = TRUE,
          notch = T)

points(airq$Ozone, jitter(rep(1,length(airq$Ozone)), 3))
points(mean(airq$Ozone, na.rm = T),1, cex=2, pch=16, col='brown') #?

bx <- boxplot(airq$Ozone)
bx

# How to generate a normal distribution of Ozone values,
# starting form mean and sd of real data

airq$Ozone

ozone_norm <- rnorm(bx$n,mean = mean(airq$Ozone,na.rm = T),
                    sd=sd(airq$Ozone,na.rm = T))
ozone_norm

boxplot(airq$Ozone, ozone_norm,
        main='Multiple boxplot',
        names=c('ozone','normal'),
        horizontal = T,
        notch = T,
        col=c('yellow','green'),
        las=1
        )

airq

boxplot(Temp~Month, #ALT+5 for mac, ALT+126 for win
        data=airq,
        col=rainbow(5),
        notch = T,
        las=1,
        main='Different boxplot for each month'
        ) 

# Exercise - boxplot for the three different species of the var Sepal.Length
data("iris")
iris

boxplot(Sepal.Length~Species,
        data=iris,
        col=rainbow(3),
        notch=T,
        las=1
        )

# Violin plot ----
library(vioplot)
vioplot(Sepal.Length~Species,
        data=iris,
        col=rainbow(3),
        notch=T,
        las=1
)

# how to create a report ----
# press 'Compile Report
# 