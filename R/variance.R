library(tidyverse)

set.seed(5)
n <- 5
min <- 85
max <- 115
df <- data.frame(x = runif(n, m = min, max = max),
				 y = rep(0, n)) %>%
	mutate(deviance = abs(x - mean(x)),
		   squared = deviance^2)
df

ggplot(df, aes(x = x, y = y)) + 
	geom_point(size = 3) +
	geom_vline(xintercept = mean(df$x), linetype = 2) +
	ylim(0, (max - min)) + xlim(min, max) +
	ylab('') + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
	coord_equal()

mean(df$squared)
var(df$x)
