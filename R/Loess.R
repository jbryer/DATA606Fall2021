# Adapted from http://varianceexplained.org/files/loess.html
library(tidyverse)

center <- -0.2
span <- .5

# data(ethanol, package = 'lattice')
# df <- data.frame(x = ethanol$E, y = ethanol$NOx)

df <- tibble(
	x = seq(-1, 1, by = 0.01),
	y = x^3 + rnorm(length(x), mean = 0, sd = 0.05) - x
)

loess.out <- loess(y ~ x, data = df, degree = 1, span = span)
df <- df %>% mutate(fitted = fitted(loess.out))

df.points <- df %>%
	mutate(dist = abs(x - center)) %>%
	filter(rank(dist) / n() <= span) %>%
	mutate(weight = (1 - (dist / max(dist)) ^ 3) ^ 3)

ggplot(df.points, aes(x = x, y = y)) +
	geom_vline(xintercept = center, linetype = 2) +
	geom_point(data = df, alpha = 0.5, shape = 1) +
	geom_point(aes( color = weight)) +
	geom_smooth(method = 'lm', formula = y ~ x, aes(weight = weight),
				se = FALSE, color = 'blue', size = 0.5) +
	scale_color_gradient2(low = '#ece7f2', high = '#2b8cbe') +
	geom_line(data = df, aes(y = fitted), color = 'black', size = 0.5)

summary(loess.out)
