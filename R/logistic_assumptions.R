library(car)
library(ggplot2)

study <- data.frame(
	Hours=c(0.50,0.75,1.00,1.25,1.50,1.75,1.75,2.00,2.25,2.50,2.75,3.00,
			3.25,3.50,4.00,4.25,4.50,4.75,5.00,5.50),
	Pass=c(0,0,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,1,1,1)
)

ggplot(study, aes(x = Hours, y = Pass)) + 
	geom_point(aes(color = factor(Pass))) +
	scale_color_brewer('Pass', type = 'qual', palette = 6)

lr.out <- glm(Pass ~ Hours, data = study, family = binomial(link='logit'))
summary(lr.out)

boxTidwell(Pass ~ Hours, data = study)

study$predicted <- lr.out$linear.predictors

ggplot(study, aes(x = Hours, y = predicted)) + geom_point()

