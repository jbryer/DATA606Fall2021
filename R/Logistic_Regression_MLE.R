library(ggplot2)
library(viusalMLE)

# From Wikipedia: https://en.wikipedia.org/wiki/Logistic_regression
study <- data.frame(
	Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
			3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
	Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

ggplot(study, aes(x = Hours, y = Pass)) +
	geom_point() +
	geom_smooth(method = 'glm', 
				formula = y ~ x,
				method.args = list(family = binomial(link = 'logit')),
				se = FALSE, )

lr.out <- glm(Pass ~ Hours, data = study, family = binomial(link='logit'))
summary(lr.out)

lr.out$coefficients
b0 <- lr.out$coefficients[1]
b1 <- lr.out$coefficients[2]


logit <- function(x, beta0, beta1) {
	return( 1 / (1 + exp(-beta0 - beta1 * x)) )
}

min_resid <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	resid <- outcome - p
	return(sum(abs(resid)))
}

ols_linear <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	predicted <- a + b * predictor
	residuals <- outcome - predicted
	ss <- sum(residuals^2)
	return(ss)
}

ols_logistic <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	resid <- outcome - p
	return(sum(resid^2))
}


ggplot(data = study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	geom_function(fun = logit, args = list(beta0 = b0, beta1 = b1)) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

# Why not use OLS? The assumptions of homoskedasticity and normality
# of residuals are violated.
ggplot(study, aes(x = predict)) + geom_density()

optim.resid <- optim_save(
	c(0, 1), # Initial values
	min_resid,
	method = "L-BFGS-B",
	# control = list(fnscale = -1),
	predictor = study$Hours,
	outcome = study$Pass
)
optim.resid$par

optim.ols_linear <- optim_save(
	c(0, 1), # Initial values
	ols_logistic,
	method = "L-BFGS-B",
	# control = list(fnscale = -1),
	predictor = study$Hours,
	outcome = study$Pass
)
optim.ols_linear$par

optim.ols_lostic <- optim_save(
	c(0, 1), # Initial values
	ols_logistic,
	method = "L-BFGS-B",
	# control = list(fnscale = -1),
	predictor = study$Hours,
	outcome = study$Pass
)
optim.ols_lostic$par

loglikelihood.binomial <- function(parameters, predictor, outcome) {
	a <- parameters[1] # Intercept
	b <- parameters[2] # beta coefficient
	p <- logit(predictor, a, b)
	ll <- sum( outcome * log(p) + (1 - outcome) * log(1 - p))
	return(ll)
}

optim.binomial <- optim_save(
	c(0, 1), # Initial values
	loglikelihood.binomial,
	method = "L-BFGS-B",
	control = list(fnscale = -1),
	predictor = study$Hours,
	outcome = study$Pass
)
optim.binomial$par

ggplot(data = study, aes(x = Hours, y = Pass)) + 
	# geom_smooth(formula = y ~ x, method = 'glm', se = FALSE,
	# 			method.args = list(family = binomial(link='logit')), color = 'blue') +
	geom_smooth(formula = y ~ x, method = 'lm', se = FALSE, color = 'black', size = .5) +
	geom_function(fun = logit, args = list(beta0 = optim.ols$par[1], beta1 = optim.ols$par[2]), size = .5) +
	geom_function(fun = logit, args = list(beta0 = optim.resid$par[1], beta1 = optim.resid$par[2]), size = .5) +
	geom_function(fun = logit, args = list(beta0 = optim.binomial$par[1], beta1 = optim.binomial$par[2]), size = .5) +
	geom_point(aes(color = factor(Pass))) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)


b0 <- optim.binomial$par[1]
b1 <- optim.binomial$par[2]


study$predict <- logit(study$Hours, beta0 = b0, beta1 = b1)
study$log_likelihood <- study$Pass * log(study$predict) + (1 - study$Pass) * log(1 - study$predict)

ggplot(data = study, aes(x = Hours, y = Pass)) + 
	geom_hline(yintercept = 0) +
	geom_function(fun = logit, color = 'grey70',
				  args = list(beta0 = optim.binomial$par[1], beta1 = optim.binomial$par[2])) +
	geom_segment(aes(xend = Hours, yend = predict)) +
	geom_point(aes(y = predict, color = factor(Pass), shape = 'Predicted'), size = 3) +
	geom_point(aes(color = factor(Pass), shape = 'Observed')) +
	geom_point(aes(y = log_likelihood, color = factor(Pass), shape = 'Log Likelihood'), size = 3) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

