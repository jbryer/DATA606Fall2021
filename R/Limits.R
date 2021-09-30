library(tidyverse)

f <- function(x, mean = 0, sigma = 1) {
	1 / (sigma * sqrt(2 * pi)) * exp(1)^(-1/2 * ( (x - mean) / sigma )^2)
}

df <- tibble(x = seq(-3, 3, by = 0.01),
			 y = f(x))

ggplot() + stat_function(fun = f) + xlim(c(-4, 4))

# ggplot(df, aes(x = x, y = y)) + geom_path()

min <- 0
max <- 2

integrate(f, min, max)

riemann <- function(f, min, max, n = 2) {
	width <- (max - min) / n
	boxes <- tibble(
		xmin = seq(min, min + (n-1) * width, by = width),
		height = f(xmin),
		area = height * width
	)
	return(boxes)
}

n <- 3
boxes <- riemann(f, min, max, n = n)
width <- (max-min)/n
ggplot() +
	geom_rect(data = boxes, aes(xmin = xmin, ymin = 0, xmax = xmin + width, ymax = height),
			  alpha = 0.5, color = 'black') +
	geom_path(data = df, aes(x = x, y = y)) +
	ggtitle(paste0('Aera ≈ ', prettyNum(sum(boxes$area)), digits = 4))

integrate(f, min, max)

##### Derivatives

xlimits <- c(-5, 15)
f <- function(x) { 0.015 * x^3 - 0.25 * x^2 + 0.49 * x + 0.47 }
ggplot() + stat_function(fun = f) + xlim(xlimits)

x <- 5
ggplot() + stat_function(fun = f) + xlim(xlimits) +
	geom_point(aes(x = x, y = f(x)), color = 'blue', size = 3)


library(Deriv)
dx <- Deriv(f)
dx(x)

ggplot() + stat_function(fun = f) + xlim(xlimits) +
	geom_point(aes(x = x, y = f(x)), color = 'blue', size = 3) +
	geom_abline(slope = dx(x)[1], intercept = f(x) - dx(x)[1] * x, color = 'blue')




