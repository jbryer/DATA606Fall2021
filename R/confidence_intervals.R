library(psych)
library(ggplot2)
library(cowplot)

# Does the conclusion of overlapping confidence intervals match p < 0.05?

plots <- list()

group.size <- 50
standard_deviation <- 1

results <- data.frame(
	mean.a = numeric(),
	mean.b = numeric(),
	sd.a = numeric(),
	sd.b = numeric(),
	se.a = numeric(),
	se.b = numeric(),
	overlap = logical(),
	sig = logical()
)
means <- seq(0.05, 4, by = 0.05)

i <- 0.5

df <- data.frame(
	group = rep(c('A','B'), each = group.size),
	value = c(
		rnorm(group.size, mean = 0, sd = standard_deviation),
		rnorm(group.size, mean = i, sd = standard_deviation)
	)
)
describeBy(df$value, group = df$group, mat = TRUE, skew = FALSE)
t.test(value ~ group, data = df)
