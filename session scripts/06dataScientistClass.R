####################################################
## Data Scientist with R
## Doctorate course in Chemical Sciences - 2019-2020
## Università degli Studi di Perugia
## 
## Gianandrea La Porta - gianandrea.laporta@unipg.it
## Dipartimento di Chimica Biologia e Biotecnologie
####################################################

# Multivariate analysis (MVA) - PCA
# 20 April 2020
# 
# Data from:
# https://swampthingecology.org
# 
# import data
dat <- read.csv('lake_data.csv')

library(tidyverse)
dat <- as_tibble(dat)
dat

# 1. Principal component methods (PCA) 
# 2. Cluster analysis
# 
# PCA
dat1 <- dat %>% 
  select(Station.ID,LAKE,Date.EST,param,HalfMDL)
View(dat1)

dat1$param

# Long-format table
# Wide-format table
year <- c('2019','2020','2019','2020')
param <- c('DO','DO','pH','pH')
meas <- c(10,11,6,7)
df <- tibble(year,param,meas)
df

df %>% 
pivot_wider(names_from = param, values_from = meas)
# pivot_longer()

#lake case
dat2 <- dat1 %>% 
  pivot_wider(names_from = param, values_from = HalfMDL,
              values_fn = list(HalfMDL=mean))
dat2

# create new var
dat3 <- dat2 %>% 
  mutate(TN=ifelse(is.na(TN),TKN+TP,TN),
         DIN=NOx+NH4)
dat3

# remove var from dataset
dat4 <- dat3 %>% 
  select(-c(NOx,NH4,TKN,TP))
dat4

# remove NAs cases
dat5 <- na.omit(dat4)

dat5 %>% 
  plot(pH~DO, data=.)

# correlation matrix
cor(dat5[,-c(1:3)])

library(corrr)
correlate(dat5[,-c(1:3)])

# plot of correlation matrix
dat5 %>% 
  select(DO:DIN) %>% 
  correlate() %>% 
  rplot(shape = 15)

# PCA assumptions
# 1. Multiple var, measured at the continuous level / ordinal
# 2. Sample adequacy / 5 - 10 cases for var
# 3. Linear relationships

# REdaS - test assumptions
install.packages('REdaS')
library(REdaS)
?KMOS
KMOS(dat5[,-c(1:3)])

# difference from an identity matrix
bart_spher(dat5[,-c(1:3)])

# PCA stat
# prcomp()
# princomp() ----

# dubi.pca()
# rda - vegan ----

princomp(dat5[,-c(1:3)])

# data standardization
p <- c(3,5,7)
scale(p, center = T, scale = F)
scale(p, center = TRUE, scale = TRUE)
scale(p)

princomp(scale(dat5[,-c(1:3)]))

library(vegan)
# rda function for PCA
dat.pca <- rda(dat5[,-c(1:3)], scale = T)
dat.pca

# eigenvalues
eig <- dat.pca$CA$eig
variance <- eig/sum(eig)

# Scree plot
barplot(variance*100)

cumsum(variance)
plot(cumsum(variance), type='b')
abline(h=0.8, col=2)

# Importance of PCs
summary(dat.pca)$cont

# Biplot
biplot(dat.pca)

# Scores and Loadings
scrs <- scores(dat.pca,display = c('sites','species'), 
       choices = c(1:3))

as_tibble(scrs$sites, rownames='sites') %>% 
  arrange(desc(PC2))

# outlier?
dat5[21,]

# PCA function
dat.pca <- rda(dat5[-21,-c(1:3)], scale = TRUE)
scrs <- scores(dat.pca,display = c('sites','species'), 
               choices = c(1:3))
biplot(dat.pca)

# loadings for the variables
scrs$species
text(scrs$species[,-3],
     labels = rownames(scrs$species),
     col=2, cex = 0.6, adj=c(.5,-.5))

dat6 <- cbind(dat5[-21,],scrs$sites)
as_tibble(dat6)

# create labels
labLake <- dat6 %>% 
  group_by(LAKE)%>% 
  summarise(centX=mean(PC1),
            centY=mean(PC2))
labLake

summary(dat.pca)$cont

# Final biplot
plot(dat6$PC1,dat6$PC2,
       col=factor(dat6$LAKE), pch=16,
     xlab = 'PC1 (46.3%)', ylab = 'PC2 (20.5%)')
legend('topleft', legend = labLake$LAKE, fill=1:6)
abline(h=0,v=0,lty=2)
arrows(0,0,scrs$species[,1]/3,scrs$species[,2]/3, 
       col=2, length = .1)
text(scrs$species[,-3]/3,
     labels = rownames(scrs$species),
     col=2, cex = 0.6, adj=c(.5,-.5))
