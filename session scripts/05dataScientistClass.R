####################################################
## Data Scientist with R
## Doctorate course in Chemical Sciences - 2019-2020
## Università degli Studi di Perugia
## 
## Gianandrea La Porta - gianandrea.laporta@unipg.it
## Dipartimento di Chimica Biologia e Biotecnologie
####################################################

# Lesson #5
# 6 April 2020
# 
# Font ----
# https://www.jetbrains.com/lp/mono/#font-family
# 
# ChemoSpec ----
# A collection of functions for top-down exploratory data analysis of spectral 
# data obtained via nuclear magnetic resonance (NMR), infrared (IR) or Raman spectroscopy.

install.packages('ChemoSpec')
library('ChemoSpec')

# frequency distribution
library(tidyverse)

airq <- as_tibble(airquality)
airq

# create a vector
temperature <- airq %>% 
  mutate(C=(Temp-32)*5/9) %>% 
  pull(C)

temperature
hist(temperature)

hist(temperature,
     main = 'Max daily temperature',
     xlab='Temperature (°C)',
     xlim=c(10,40),
     col='orange'
     )

hist.tab <- hist(temperature,
                 main = 'Max daily temperature',
                 xlab='Temperature (°C)',
                 xlim=c(10,40),
                 ylim=c(0,35),
                 col='orange'
)

hist.tab

?text

hist.tab$counts

text(hist.tab$mids,
     hist.tab$counts,
     labels = hist.tab$counts, 
     col='orange',
     adj=c(.5,-1))

# density line ---------------------------------------------------
# 

hist(temperature,
     main = 'Max daily temperature',
     xlab='Temperature (°C)',
     xlim=c(10,40),
     #ylim=c(0,35),
     col='orange',
     freq = F)

density(temperature)
lines(density(temperature))

summary(temperature)

polygon(density(temperature), col='yellow', border='blue')

polygon(density(temperature), col=rgb(1,1,0,0.5), border='blue')

install.packages('sm')
library(sm)

# plot densities
# sm.density.compare()

airq
sm.density.compare(temperature,airq$Month,
                   xlab='temperature (°C)',
                   lwd=2)
title(main='Monthly temperature')
legend()

as.factor(airq$Month)
levels(as.factor(airq$Month))

legend('topright', legend = levels(as.factor(airq$Month)),
       lty=1:5, col=2:7)

month.labs <- c('May','Jun','Jul','Aug','Sep')
legend('topright', legend = month.labs,
       lty=1:5, col=2:7)

# challenge ---- add 5 lines with the monthly mean 
# values of temperature
# function required: abline(v=...)

#1 step - calculation of 5 means
temp.mean <- airq %>% 
  mutate(C=(Temp-32)*5/9) %>% 
  group_by(Month) %>% 
  summarise(meanT=mean(C),
            sdT=sd(C))

#2 step - abline
abline(v=temp.mean$meanT, lty=1:5, col=2:7)

# 
par(mfrow=c(1,2))
hist(temperature, breaks = 5, col='orange')
hist(temperature, breaks = 20, col='orange')

hist(temperature, breaks = c(11,20,25,27,38), col='lightgreen')

# bivariate analysis

airq

# scatterplot
airq %>% 
  select(Wind,Temp) %>% 
  plot(pch=16)

cor(x=airq$Wind, y=airq$Temp)
cor(airq$Wind,airq$Temp)

title(cor(airq$Wind,airq$Temp))

#round()
title(round(cor(airq$Wind,airq$Temp),3))
title(str_c('R = ',round(cor(airq$Wind,airq$Temp),3)))

cor.test(x=airq$Wind, y=airq$Temp)

# 0.05 first level of significance
# 
airq %>% 
  group_by(Month) %>% 
  summarise(corr=cor(Wind,Temp))

# Regression analysis ----

data("Formaldehyde")

Formaldehyde

Formaldehyde[3,]
Formaldehyde[,2]
Formaldehyde[4,1]

cor(Formaldehyde$optden, Formaldehyde$carb)

plot(Formaldehyde, pch=16)

# linear model -> Y = bX + a

lm(optden~carb, data=Formaldehyde)

#carb is the regressor / predictor / independent var
#optden is the response / criterion / dependent var

mod1 <- lm(optden~carb, data=Formaldehyde)
abline(mod1, col=2, lty=2)

intercept <- round(mod1$coefficients[1],3)
slope <- round(mod1$coefficients[2],3)

title(str_c('Y = ',slope, ' X + ', intercept))

summary(mod1)

# linear model -> Y = bX + a + err

plot(mod1, which = 1)

plot(Formaldehyde, pch=16, xlim=c(0,1.5), ylim=c(0,1.5))
abline(mod1, col=2, lty=2)

newset <- data.frame(carb=c(1.2,1.4, 1.5))
predict(mod1, newdata = newset)

newset$pred <- predict(mod1, newdata = newset)
points(newset, pch=17, col=4)

install.packages('pls')

library(pls)
gasoline

matplot(t(gasoline$NIR),
        type = "l", lty = 1,
        ylab = "log(1/R)")
#, xaxt = "n")
# axis(3,at = c(0,100,200,300,400),
#     labels = c('900 nm','1100 nm','1300 nm','1500 nm','1700 nm'))

axis(3,at = c(50,150,250,350),
     labels = c('1000 nm','1200 nm','1400 nm','1600 nm'))

ms <- c('ter','Hap','Eas','py ')
text(100,1.1,str_c(ms[2],ms[4],ms[3],ms[1]), cex=3, col=5)
