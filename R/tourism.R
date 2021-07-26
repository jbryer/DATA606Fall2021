library(openintro)
library(ggplot2)

data("tourism", package = openintro) # Question 8.21

ggplot(tourism, aes(x = visitor_count_tho)) +
	geom_histogram()

ggplot(tourism, aes(x = tourist_spending)) +
	geom_histogram()

ggplot(tourism, aes(x = visitor_count_tho, y = tourist_spending)) +
	geom_point() +
	geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
	scale_x_log10() + scale_y_log10()

lm.out <- lm(visitor_count_tho ~ tourist_spending, data = tourism)
summary(lm.out)

lm.out.log <- lm(log(visitor_count_tho) ~ log(tourist_spending), data = tourism)
summary(lm.out.log)

tourism$residual <- resid(lm.out)
tourism$residual.log <- resid(lm.out.log)

ggplot(tourism, aes(x = tourist_spending, y = residual)) +
	geom_hline(yintercept = 0) +
	geom_point()

ggplot(tourism, aes(x = tourist_spending, y = residual.log)) +
	geom_hline(yintercept = 0) +
	geom_point()

hist(tourism$residual)
ggplot(tourism, aes(x = residual)) + geom_histogram(bins = 7)

ggplot(tourism, aes(x = residual)) + geom_density()




