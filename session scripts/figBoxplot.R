# Data Scientist with R
# Gianandrea La Porta

fig.boxplot <- function(){
  a <- c(1,1,1,3,3,3,3,5,5,5,5,5,5,7,7,7,7,9,9,9)
  bx <- boxplot(a, notch = T, col='yellow', names='Variable', show.names=T)
  text(1.2,1,'min')
  text(1.2,9,'max')
  text(1.25,5,'median (50%)')
  text(1.30,quantile(a,0.25),'Q1 (25%)')
  text(1.30,quantile(a,0.75),'Q3 (75%)')
  arrows(0.7,quantile(a,0.25),0.7,quantile(a,0.75), code = 3, angle = 90)
  text(0.75,5,'IQR')
  title('Box and whiskers plot')
}