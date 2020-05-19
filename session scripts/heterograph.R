heterograph <- function() {
  set.seed(1)
  x <- runif(500, 0, 1)
  y <- 5 * rnorm(500, x, x)
  plot(y ~ x, col = "gray", pch = 19)
  abline(lm(y ~ x), col = "blue")
  title('Heteroskedasticity')
  arrows(.1,-.5,.1,1.5,length = 0, col=2, lty=2)
  arrows(.9,-8,.9,13.5,length = 0, col=2, lty=2)
}
