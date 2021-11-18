library(ggplot2)
library(gganimate)
library(magick)

sat <- read.csv('course_data/SAT_scores.csv', stringsAsFactors=FALSE)
names(sat) <- c('Verbal','Math','Sex')
sat$Verbal <- as.integer(sat$Verbal)
sat$Math <- as.integer(sat$Math)
sat <- sat[complete.cases(sat),]

mathMean <- mean(sat$Math)
verbalMean <- mean(sat$Verbal)
mathSD <- sd(sat$Math)
verbalSD <- sd(sat$Verbal)

results <- data.frame(r=seq(-1, 1, by=.05),
					  m=as.numeric(NA),
					  b=as.numeric(NA),
					  sumsquares=as.numeric(NA))
for(i in 1:nrow(results)) {
	results[i,]$m <- results[i,]$r * (mathSD / verbalSD)
	results[i,]$b <-  mathMean - results[i,]$m * verbalMean
	predicted <- results[i,]$m * sat$Verbal + results[i,]$b
	residual <- sat$Math - predicted
	sumsquares <- sum(residual^2)
	results[i,]$sumsquares <- sum(residual^2)
}

# head(results)

p1 <- ggplot(sat, aes(x = Verbal, y = Math)) +
	geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
	geom_abline(data = results, aes(intercept = b, slope = m)) +
	geom_point(data = sat, aes(x = Verbal, y = Math)) +
	transition_time(r) +
	labs(title = "r: {round(frame_time, digits = 2)}")
p1_gif <- animate(p1, width = 480, height = 480)

results2 <- results
results2$r_path <- results2$r
results2$r <- NULL
p2 <- ggplot(results2) +
	geom_path(data = results2, aes(x = r_path, y = sumsquares)) +
	geom_point(data = results, aes(x = r, y = sumsquares), color = 'red', size = 3) +
	transition_time(r) +
	xlab('r') + ylab('Total Sum of Square Residual (RSS)')
p2_gif <- animate(p2, width = 480, height = 480)

new_gif <- image_append(c(p1_gif[1], p2_gif[1]))
for(i in 2:100){
	combined <- image_append(c(p1_gif[i], p2_gif[i]))
	new_gif <- c(new_gif, combined)
}
new_gif

anim_save('Slides/images/ols_animation.gif', animation = new_gif)
