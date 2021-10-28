library(tidyverse)

n <- 40
df <- data.frame(#a = runif(n, min = 0, 5),
	a = rnorm(n),
	b = rep(0, n))
df[df$a == min(df$a),]$b <- 1
df$p <- NA_real_

ggplot(df, aes(x = b, y = a)) + geom_point() + 
	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE)

test <- cor.test(df$a, df$b)
test

for(i in 1:nrow(df)) {
	df$b <- 0
	df[i,]$b <- 1
	test <- cor.test(df$a, df$b)
	df[i,]$p <- test$p.value
}

ggplot(df, aes(x = a, y = p, color = p < 0.05)) + 
	geom_point() + geom_hline(yintercept = 0.05)

