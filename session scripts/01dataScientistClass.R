####################################################
## Data Scientist with R
## Doctorate course in Chemical Sciences - 2019-2020
## Università degli Studi di Perugia
## 
## Gianandrea La Porta - gianandrea.laporta@unipg.it
## Dipartimento di Chimica Biologia e Biotecnologie
####################################################

# prompt

# calculator
2+3

2+3- # press ESC

2*3

12/4
3^2 # power function
(10+2)/3

# this is a comment


# assignment, objects and functions ####

x<-4
x=4 # please, don't use

x

X # error!
x*2
x <- 6
x*2

b <- seq(1,10)
seq(1,10,2)

1:10

?c # combine values
y<-c(2,7,4,1)
y

(y<-c(2,7,4,1))
y<-c(2,7,4,1);y

5*y;2*y

z<-c("sample1","sample2","sample3","sample4")
z

# barplot ####
barplot(y)

barplot(y, names.arg = z)
barplot(y, names.arg = z, ylab='Units') # add label to y-axis 
barplot(y, names.arg = z, ylab='Units', main='My barplot') # add a title
barplot(y, names.arg = z, ylab='Units', main='My barplot', col='lightgreen')
barplot(y, names.arg = z, ylab='Units', main='My barplot', col=y)
barplot(y, names.arg = z, ylab='Units', main='My barplot', col=y)

# addins custom colors
barcolors <- c("#FF3030", "#228B22", "#FFD700", "#00BFFF")
barplot(y, names.arg = z, ylab='Units', main='My barplot', col=barcolors)

library(gplots)

barplot2(y, names.arg = z, ylab='Units', 
         main='My barplot', col=y, 
         plot.grid = T) # T is TRUE

barplot2(y, names.arg = z, ylab='Units', 
         main='My barplot', col=y, 
         plot.grid = TRUE)

# create a data.frame
data.frame(sample=z,value=y)

df <- data.frame(sample=z,value=y)
df
df$sample
df$value

# display the internal structure of a dataframe
str(df)

# add a new colum
df$newColumn <- c(8,6,3,4)


# change the values
df[1]
df[2]
df[3]
df[1,]
df[2,]
df[1,1]
df[1,2]
df

new_variable <- 'snake case'
newVariable <- 'camel case'
new.variable <- 'point case'

demo(graphics)

# histogram
data(iris)
str(iris)

head(iris)
?head

hist(iris$Sepal.Length, main='')
#mean value
mean(iris$Sepal.Length)
abline(v=mean(iris$Sepal.Length), col='red', lwd=3)

jackHist <- hist(iris$Sepal.Length, main='')
jackHist