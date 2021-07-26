library(tidyverse)
library(cowplot)

#' Plot the four distributions functions.
#'
plot_distributions <- function(dist, xvals, xmin, xmax,
							   args = list(),
							   palette = c(d = '#1b9e77', r = '#d95f02', p = '#7570b3', q = '#e7298a')) {
	functions <- c(d = get(paste0('d', dist)),
				   r = get(paste0('r', dist)),
				   p = get(paste0('p', dist)),
				   q = get(paste0('q', dist)))

	df <- tibble(
		x = xvals,
		d = sapply(xvals, FUN = function(x) { args$x <- x; do.call(functions[['d']], args = args) }),
		p = sapply(xvals, FUN = function(x) { args$q <- x; do.call(functions[['p']], args = args) })
	)

	args_str <- ifelse(length(args) > 0,
					   paste0(names(args), ' = ', args, collapse = ', '),
					   '')

	d_plot <- ggplot(df, aes(x = x, y = d)) +
		xlim(xmin, xmax) +
		geom_function(fun = functions[['d']], args = args) +
		geom_segment(aes(x = x, xend = x, y = 0, yend = d), color = palette['d']) +
		geom_segment(aes(x = x, xend = xmin, y = d, yend = d), color = palette['d'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		geom_point(aes(x = x, y = 0), color = palette['d'], size = 2) +
		xlab('z-score / quantile') + ylab('Probability Density') +
		ggtitle(paste0('d', dist, '(', args_str, ')'))

	r_plot <- ggplot(df, aes(x = x, y = d)) +
		xlim(xmin, xmax) +
		geom_function(fun = functions[['d']], args = args) +
		geom_segment(aes(x = x, xend = x, y = d, yend = 0), color = palette['r'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		xlab('z-score / quantile') + ylab('Probability Density') +
		ggtitle(paste0('r', dist, '(', args_str, ')'))

	p_plot <- ggplot(df, aes(x = x, y = d)) +
		xlim(xmin, xmax) +
		geom_function(fun = functions[['p']], args = args) +
		geom_segment(aes(x = x, xend = x, y = 0, yend = p), color = palette['p']) +
		geom_segment(aes(x = x, xend = xmin, y = p, yend = p), color = palette['p'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		geom_point(aes(x = x, y = 0), color = palette['p'], size = 2) +
		xlab('z-score / quantile') + ylab('Cumulative Probability') +
		ggtitle(paste0('p', dist, '(', args_str, ')'))

	q_plot <- ggplot(df, aes(x = x, y = d)) +
		xlim(xmin, xmax) +
		geom_function(fun = functions[['p']], args = args) +
		geom_segment(aes(x = x, xend = x, y = p, yend = 0), color = palette['q'],
					 arrow = arrow(length = unit(0.5, "cm"))) +
		geom_segment(aes(x = x, xend = xmin, y = p, yend = p), color = palette['q']) +
		geom_point(aes(x = xmin, y = p), color = palette['q'], size = 2) +
		xlab('z-score / quantile') + ylab('Cumulative Probability') +
		ggtitle(paste0('q', dist, '(', args_str, ')'))

	plot_grid(d_plot, r_plot, p_plot, q_plot)
}

if(FALSE) {
	plot_distributions(dist = 'norm',
					   xvals = c(-1, 0, 0.5),
					   xmin = -4, xmax = 4)

	plot_distributions(dist = 'norm',
					   xvals = c(80, 90, 105),
					   xmin = 55, xmax = 145,
					   args = list(mean = 100, sd = 15))

	plot_distributions(dist = 'chisq',
					   xvals = c(1, 2, 5),
					   xmin = 0, xmax = 10,
					   args = list(df = 3))

	plot_distributions(dist = 'f',
					   xvals = c(0.5, 1, 2),
					   args = list(df1 = 3, 12),
					   xmin = 0, xmax = 10)

	plot_distributions(dist = 't',
					   xvals = c(-1, 0, 0.5),
					   xmin = -4, xmax = 4,
					   args = list(df = 5))
}
