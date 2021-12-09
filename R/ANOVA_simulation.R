library(psych)
library(tidyverse)
library(egg)
library(ggExtra)


k <- 3
n <- 8

grand_sd <- 3
group_means <- c(16, 5, 5)

df <- data.frame(
	Group = rep(LETTERS[1:k], each = n),
	Value = as.numeric(sapply(group_means, FUN = function(x) { rnorm(n, mean = x, sd = grand_sd) }))
)

hand <- read.csv('course_data/Hand_washing.csv')
df <- data.frame(Group = hand$Method,
				 Value = hand$Bacterial.Counts)

# df[df$Group == 'Alcohol Spray',]$Value <- df[df$Group == 'Alcohol Spray',]$Value + 50

# colors <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')

#####

desc <- describeBy(df$Value, group = df$Group, mat = TRUE, skew = FALSE)
names(desc)[2] <- 'Group'
desc$Var <- desc$sd^2
desc$contrast <- (desc$mean - mean(desc$mean)) #/ sd(desc$mean)
desc

df <- merge(df, desc[,c('Group', 'contrast', 'mean')],
			by = 'Group', all.x = TRUE)

k <- length(unique(df$Group))
n <- nrow(df)

( grand_mean <- mean(df$Value) )
( grand_var <- var(df$Value) )
( pooled_var <- mean(desc$Var) )

(ss_total <- sum((df$Value - grand_mean)^2))
(df_between <- k - 1)
(ss_between <- sum(desc$n * (desc$mean - grand_mean)^2))
(MS_between <- ss_between / df_between)

(df_within <- n - k)
(ss_within <- ss_total - ss_between)
(MS_within <- ss_within / df_within)

( F_stat <- MS_between / MS_within )

( p <- 1 - pf(F_stat, df_between, df_within) )

# df_rect <- data.frame(
# 	`Mean Square` = c('Between', 'Within'),
# 	xmin = c(-1* sqrt(MS_between) / sd(df$Value),
# 			 -1 *sqrt(MS_within) / sd(df$Value)),
# 	xmax = c(    sqrt(MS_between) / sd(df$Value),
# 				 sqrt(MS_within) / sd(df$Value)),
# 	ymin = c(grand_mean - 1 * sqrt(MS_between) / sd(df$Value),
# 			 grand_mean - 1 * sqrt(MS_within) / sd(df$Value)),
# 	ymax = c(grand_mean +     sqrt(MS_between) / sd(df$Value),
# 			 grand_mean +     sqrt(MS_within) / sd(df$Value))
# )

df_rect <- data.frame(
	`Mean Square` = c('Between', 'Within'),
	# contrast = NA,
	# Value = NA,
	xmin = c(-1* sqrt(MS_between)/2,
			 -1 *sqrt(MS_within)/2),
	xmax = c(    sqrt(MS_between)/2,
				 sqrt(MS_within)/2),
	ymin = c(grand_mean - 1 * sqrt(MS_between)/2,
			 grand_mean - 1 * sqrt(MS_within)/2),
	ymax = c(grand_mean +     sqrt(MS_between)/2,
			 grand_mean +     sqrt(MS_within)/2) )

slope <- (desc[1,]$mean - desc[2,]$mean) / (desc[1,]$contrast - desc[2,]$contrast)
intercept <- desc[1,]$mean - slope * desc[1,]$contrast

df_rect_within <- df %>%
	mutate(square = (Value - mean)^2) %>%
	group_by(Group) %>%
	summarize(contrast = mean(Value) - grand_mean,
			  mean = mean(Value),
			  MS = sum(square) / (n() - 1)) %>%
	mutate(xmin = contrast - sqrt(MS),
		   xmax = contrast + sqrt(MS),
		   ymin = mean - sqrt(MS),
		   ymax = mean + sqrt(MS))

df_subscript <- paste0(df_between, ', ', df_within)
title <- bquote(F[.(df_subscript)] == .(prettyNum(F_stat, digits = 3)) ~ '; p' ~ .(ifelse(p < 0.01, ' < 0.01', paste0(' = ', prettyNum(p, digits = 3)))))

# df_rect_within
# df_rect_within$MS

# tmp <- (df_rect$xmax - df_rect$xmin) * (df_rect$ymax - df_rect$ymin)
# tmp[1] / tmp[2]
# df_rect

# library(granova)
# granova.1w(df$Value, df$Group)

p <- ggplot() +
	# geom_rect(data = df_rect_within, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, color = Group),
	# 		  alpha = 0.05, linetype = 2) +
	# geom_rect(data = df_rect, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square, color = Mean.Square, fill = Mean.Square),
	# 		  alpha = 0.05) +
	geom_rect(data = df_rect[1,], aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
			  alpha = 0.1, fill = '#7fc97f') +
	geom_rect(data = df_rect[2,], aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax, group = Mean.Square),
			  alpha = 0.4, fill = '#fdc086') +
	# TODO: use 1/2 sqrt(ms_within)
	geom_hline(yintercept = c(grand_mean - sd(df$Value), grand_mean + sd(df$Value)),
			   linetype = 5, color = 'maroon', alpha = 0.5) +
	geom_text(x = c(-100, 100),
			  y = c(grand_mean - sd(df$Value), grand_mean + sd(df$Value)),
			  label = c('mean - SD', 'mean + SD'),
			  color = 'maroon') +
	# geom_hline(yintercept = c(grand_mean - mean(desc$sd), grand_mean + mean(desc$sd)),
	# 		   linetype = 2, color = 'darkgreen', alpha = 0.5) +
	geom_hline(yintercept = mean(df$Value), alpha = 0.5, linetype = 2, size = 1) +
	geom_abline(slope = slope, intercept = intercept, color = 'grey70') +
	geom_segment(data = desc, aes(x = contrast, xend = contrast, y = mean - sd, yend = mean + sd), alpha = 0.6) +
	geom_point(data = df, aes(x = contrast, y = Value, group = Group, color = Group),
			   alpha = 0.75, shape = 1, size = 2) +
	geom_point(aes(x = 0, y = grand_mean), color = 'blue', size = 4) +
	geom_point(data = desc, aes(x = contrast, y = mean, color = Group), size = 3) +
	geom_text(data = desc, aes(label = Group, x = contrast, y = min(df$Value)),
			  angle = 90, hjust = 0, vjust = -0.8) +
	# geom_rug(data = df, aes(y = Value), alpha = 0.3, sides = 'r') +
	# ggtitle(paste0('F = ', prettyNum(F_stat, digits = 3), '; p = ', prettyNum(p, digits = 2))) +
	ggtitle(title) +
	# scale_color_hue('Mean Square') + scale_fill_hue('Mean Square') +
	scale_color_brewer('Group', type = 'qual', palette = 2) + #scale_fill_brewer('Group', type = 'qual', palette = palette) +
	# xlim(c(min(df$Value) - grand_mean, max(df$Value) + grand_mean)) +
	xlab('Contrast Coefficient') + ylab('Dependent Variable') + coord_equal() +
	theme_minimal() + theme(panel.grid.major = element_line(color = 'grey90', size = 0.3),
							panel.grid.minor = element_blank(),
							legend.position = 'bottom') +
	xlim(range(df$Value) - grand_mean)
p

ggExtra::ggMarginal(p, groupColour = TRUE, groupFill = TRUE, type = 'boxplot', margins = 'y', yparams = list(alpha = 0.05))

ggExtra::ggMarginal(p, groupColour = TRUE, groupFill = TRUE, type = 'density', margins = 'y', yparams = list(alpha = 0.05))


# hand <- read.csv('course_data/Hand_washing.csv')

# library(granova)
# granova.1w(df$Value, group = df$Group)

# out <- aov(Value ~ Group, data = df)
# ls(out)
# out$terms
# summary(out)
