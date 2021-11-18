library(tidyverse)
library(granova)
library(psych)

hand <- read.csv('./course_data/Hand_washing.csv')

tab <- describeBy(hand$Bacterial.Counts, hand$Method, mat = TRUE)
names(tab)[2] <- 'Method'
tab

# tab$contrast <- (tab$mean - mean(hand$Bacterial.Counts)) / sd(hand$Bacterial.Counts)
tab$contrast <- tab$mean - grand.mean
(tab$mean - mean(tab$mean)) / sd(tab$mean)

hand <- merge(hand, tab[,c('Method', 'contrast')], by = 'Method', all.x = TRUE)


hand.anova <- granova.1w(hand$Bacterial.Counts, group=hand$Method)
hand.anova

(grand.mean <- mean(hand$Bacterial.Counts))
(n <- nrow(hand))

(ss.total <- sum((hand$Bacterial.Counts - grand.mean)^2))

# Between Groups
(df.between <- k - 1)
(k <- length(unique(hand$Method)))
(ss.between <- sum(tab$n * (tab$mean - grand.mean)^2))
(MS.between <- ss.between / df.between)

# Within Groups
(df.within <- n - k)
(ss.within <- ss.total - ss.between)
(MS.within <- ss.within / df.within)

(F <- MS.between / MS.within)


ggplot(tab, aes(x = contrast, y = mean)) +
	geom_hline(yintercept = c(grand.mean + sqrt(MS.within),
							  grand.mean - sqrt(MS.within)),
			   color = 'blue', linetype = 3) +
	geom_vline(xintercept = c(sqrt(MS.within),
							  -1 * sqrt(MS.within)),
			   color = 'blue', linetype = 3) +
	geom_path(color = 'blue') +
	geom_hline(yintercept = mean(hand$Bacterial.Counts), color = 'green', linetype = 2) +
	geom_point(data = hand, aes(x = contrast, y = Bacterial.Counts), alpha = 0.5) +
	geom_point(color = 'red', shape = 2, size = 4) +
	ylab('Bacterial Count') + xlab("Contrast Coefficient") #+ coord_equal()



granovagg.1w(hand$Bacterial.Counts, hand$Method)


ggplot(hand, aes(x = Bacterial.Counts, y = Bacterial.Counts, color = Method)) +
	geom_rect(aes(xmin = Bacterial.Counts, ymin = Bacterial.Counts,
				  xmax = mean(Bacterial.Counts), ymax = mean(Bacterial.Counts), color = Method)) +
	geom_vline(xintercept = mean(hand$Bacterial.Counts)) +
	geom_vline(data = tab, aes(xintercept = mean, color = Method)) +
	geom_hline(yintercept = mean(hand$Bacterial.Counts)) +
	geom_hline(data = tab, aes(yintercept = mean, color = Method)) +
	geom_point() +
	facet_wrap(~ Method) + coord_equal()
