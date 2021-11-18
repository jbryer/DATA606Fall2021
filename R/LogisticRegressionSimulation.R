library(ggplot2)

a.mean <- 1.5
a.sd <- 1
a.n <- 20
b.mean <- 0
b.sd <- 1
b.n <- 20

df <- data.frame(
	y = c(rep(1, a.n), rep(0, b.n)),
	x = c(rnorm(a.n, a.mean, a.sd), rnorm(b.n, b.mean, b.sd))
)

ggplot(df, aes(x = x, y = y)) +
	geom_point() +
	geom_smooth(method = glm, se = FALSE, formula = y ~ x,
				method.args = list(family = binomial(link = logit)))
