library(psych)
library(ggplot2)
library(tidyverse)

hand <- read.csv('./course_data/Hand_washing.csv', stringsAsFactors = FALSE)

desc <- describeBy(hand$Bacterial.Counts, group = hand$Method, mat = TRUE, skew = FALSE)
names(desc)[2] <- 'Group'
desc$Var <- desc$sd^2
desc$contrast <- (desc$mean - mean(desc$mean))
mean(desc$contrast) # Should be 0!
desc

df <- merge(hand, desc[,c('Group', 'contrast', 'mean')],
			by.x = 'Method', by.y = 'Group', all.x = TRUE)

( k <- length(unique(df$Method)) )
( n <- nrow(df) )

( grand_mean <- mean(df$Bacterial.Counts) )
( grand_var <- var(df$Bacterial.Counts) )
( pooled_var <- mean(desc$Var) )

( ss_total <- sum((df$Bacterial.Counts - grand_mean)^2) )
( df_between <- k - 1 )
( ss_between <- sum(desc$n * (desc$mean - grand_mean)^2) )
( MS_between <- ss_between / df_between )
 
( df_within <- n - k )
( ss_within <- ss_total - ss_between )
( MS_within <- ss_within / df_within )

( F_stat <- MS_between / MS_within )

( p <- 1 - pf(F_stat, df_between, df_within) )

df_rect <- data.frame(
	group = c('Between', 'Within'),
	xmin = c(-1* sqrt(MS_between) / sd(df$Bacterial.Counts),
			 -1 *sqrt(MS_within) / sd(df$Bacterial.Counts)),
	xmax = c(    sqrt(MS_between) / sd(df$Bacterial.Counts),
				 sqrt(MS_within) / sd(df$Bacterial.Counts)),
	ymin = c(grand_mean - 1 * sqrt(MS_between) / sd(df$Bacterial.Counts),
			 grand_mean - 1 * sqrt(MS_within) / sd(df$Bacterial.Counts)),
	ymax = c(grand_mean +     sqrt(MS_between) / sd(df$Bacterial.Counts),
			 grand_mean +     sqrt(MS_within) / sd(df$Bacterial.Counts))
)

df_rect <- data.frame(
	`Mean Square` = c('Between', 'Within'),
	# contrast = NA,
	# Value = NA,
	xmin = c(-1* sqrt(MS_between),
			 -1 *sqrt(MS_within)),
	xmax = c(    sqrt(MS_between),
				 sqrt(MS_within)),
	ymin = c(grand_mean - 1 * sqrt(MS_between),
			 grand_mean - 1 * sqrt(MS_within)),
	ymax = c(grand_mean +     sqrt(MS_between),
			 grand_mean +     sqrt(MS_within)) )

slope <- (desc[1,]$mean - desc[2,]$mean) / (desc[1,]$contrast - desc[2,]$contrast)
intercept <- desc[1,]$mean - slope * desc[1,]$contrast

df_rect_within <- df %>%
	mutate(square = (Bacterial.Counts - mean)^2) %>%
	group_by(Method) %>%
	summarize(contrast = mean(Bacterial.Counts) - grand_mean,
			  mean = mean(Bacterial.Counts),
			  MS = sum(square) / (n() - 1)) %>%
	mutate(xmin = contrast - sqrt(MS),
		   xmax = contrast + sqrt(MS),
		   ymin = mean - sqrt(MS),
		   ymax = mean + sqrt(MS))

df_subscript <- paste0(df_between, ', ', df_within)
title <- bquote(F[.(df_subscript)] == .(prettyNum(F_stat, digits = 3)) ~ '; p' ~ .(ifelse(p < 0.01, ' < 0.01', paste0(' = ', prettyNum(p, digits = 3)))))

ggplot() +
	geom_rect(data = df_rect_within, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, color = Method),
			  alpha = 0.05, linetype = 2) +
	geom_rect(data = df_rect[1,], aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
			  alpha = 0.1, fill = '#7fc97f') +
	geom_rect(data = df_rect[2,], aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
			  alpha = 0.4, fill = '#fdc086') +
	geom_hline(yintercept = c(grand_mean - sd(df$Bacterial.Counts), grand_mean + sd(df$Bacterial.Counts)),
			   linetype = 5, color = 'maroon', alpha = 0.5) +
	geom_text(x = c(-100, 100),
			  y = c(grand_mean - sd(df$Bacterial.Counts), grand_mean + sd(df$Bacterial.Counts)),
			  label = c('mean - SD', 'mean + SD'),
			  color = 'maroon') +
	geom_hline(yintercept = mean(df$Bacterial.Counts), alpha = 0.5, linetype = 2, size = 1) +
	geom_abline(slope = slope, intercept = intercept, color = 'grey70') +
	geom_segment(data = desc, aes(x = contrast, xend = contrast, y = mean - sd, yend = mean + sd), alpha = 0.6) +
	geom_point(data = df, aes(x = contrast, y = Bacterial.Counts, group = Method, color = Method),
			   alpha = 0.75, shape = 1, size = 2) +
	geom_point(aes(x = 0, y = grand_mean), color = 'blue', size = 4) +
	geom_point(data = desc, aes(x = contrast, y = mean, color = Group), size = 3) +
	geom_text(data = desc, aes(label = Group, x = contrast, y = min(df$Bacterial.Counts)),
			  angle = 90, hjust = 0, vjust = -0.8) +
	ggtitle(title) +
	scale_color_brewer('Method', type = 'qual', palette = 2) + #scale_fill_brewer('Method', type = 'qual', palette = palette) +
	xlab('Contrast Coefficient') + ylab('Dependent Variable') + coord_equal() +
	theme_minimal() + theme(panel.grid.major = element_line(color = 'grey90', size = 0.3),
							panel.grid.minor = element_blank(),
							legend.position = 'bottom')
p

ggExtra::ggMarginal(p, groupColour = TRUE, groupFill = TRUE, type = 'boxplot', margins = 'y', yparams = list(alpha = 0.05))

ggExtra::ggMarginal(p, groupColour = TRUE, groupFill = TRUE, type = 'density', margins = 'y', yparams = list(alpha = 0.05))




#######

k <- 3
n_k <- 10
group_means <- c(1, 2, 5)
grand_sd <- 2

sim_results <- data.frame(F = numeric(1000),
						  p = numeric(1000))

for(i in 1:nrow(sim_results)) {
	df <- data.frame(
		Group = rep(LETTERS[1:k], each = n),
		Value = as.numeric(sapply(group_means, FUN = function(x) { rnorm(n_k, mean = x, sd = grand_sd) }))
	)

	desc <- describeBy(df$Value, group = df$Group, mat = TRUE, skew = FALSE)
	names(desc)[2] <- 'Group'
	desc$Var <- desc$sd^2
	desc$contrast <- (desc$mean - mean(desc$mean))

	grand_mean <- mean(df$Value)
	grand_var <- var(df$Value)
	pooled_var <- mean(desc$Var)
	
	ss_total <- sum((df$Value - grand_mean)^2)
	df_between <- k - 1
	ss_between <- sum(desc$n * (desc$mean - grand_mean)^2)
	MS_between <- ss_between / df_between
	
	df_within <- k * n_k - k
	ss_within <- ss_total - ss_between
	MS_within <- ss_within / df_within
	
	sim_results[i,]$F <- MS_between / MS_within
	sim_results[i,]$p <- 1 - pf(sim_results[i,]$F, df_between, df_within)
}

cv <- qf(0.95, k - 1, k * n_k - k)

ggplot(sim_results, aes(x = F)) +
	geom_vline(xintercept = cv, linetype = 2, alpha = 0.5) +
	geom_density()

ggplot(sim_results, aes(x = p)) +
	geom_vline(xintercept = 0.05, linetype = 2, alpha = 0.5) +
	geom_density()

mean(sim_results$p)
sum(sim_results$p < 0.05) / nrow(sim_results)
sum(sim_results$F > cv) / nrow(sim_results)

