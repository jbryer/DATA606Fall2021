library(tidyverse)
library(cowplot)

library(MASS)
data(anorexia, package = 'MASS')
head(anorexia)
granova.ds(anorexia[,2:3])

Y <- mtcars$mpg
X <- mtcars$wt

lm.out <- lm(Y ~ X)
lm.out

df <- data.frame(x = X,
			 y = Y,
			 resid = resid(lm.out))

p_scatter <- ggplot(df, aes(x = x, y = y)) + geom_point() + geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)
p_resid <- ggplot(df, aes(x = x, y = resid)) + geom_point() + geom_hline(yintercept = 0)
p_hist <- ggplot(df, aes(x = resid)) + geom_histogram() + coord_flip()

plot_grid(p_scatter, NULL, p_resid, p_hist)

