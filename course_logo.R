remotes::install_github("GuangchuangYu/hexSticker")

library(hexSticker)
library(showtext)
library(ggplot2)

course <- 'DATA 606'
semester <- 'Fall 2021'
url <- 'https://fall2021.data606.net'


font_add_google(name = "Open Sans", family = "opensans")

# CUNY Blue: 005DAC
# CUNY Orange: F99D32

color1 <- '#005DAC'
color2 <- '#F99D32'
bg.color <- '#FFFFFF'

# Use Logo
p <- "website/static/images/CUNY_SPS_Logo.png"
# p <- "website/static/images/CUNY_SPS_Logo_Wide.png"
p <- 'website/static/images/ex_4.png'

# Or ggplot2
# p <- ggplot(data = mtcars, aes(x = -mpg, y = wt)) +
# 	geom_point(size = 1, color = '#FFFFFF') +
# 	# geom_smooth(method = loess, size = .5, color = color1, se = FALSE) +
# 	geom_smooth(formula = y ~ x, se = FALSE, method = lm, size = .5, color = color1) +
# 	theme_void() + theme_transparent()
# p


sticker <- sticker(p,
		package = paste0(course, '\n', semester),
		p_x = 1, p_y = 1.45,
		p_size = 5,
		p_color = color1,
		p_family = 'opensans',
		white_around_sticker = FALSE,
		s_x = 1, s_y = .85, s_width=.9, s_height = .7,
		h_color = color1,
		h_fill = bg.color,
		spotlight = FALSE,
		url = url,
		u_color = color2,
		filename = paste0(sub(' ', '-', course), '-', sub(' ', '-', semester), '.png') )

# Save a square version to use as an icon for Slack
ggsave(filename = paste0(sub(' ', '-', course), '-', sub(' ', '-', semester), '-square.png'),
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', bg = 'transparent', dpi = 300)

# Copy file for use on the website
file.copy(paste0(sub(' ', '-', course), '-', sub(' ', '-', semester), '.png'),
		  'website/static/images/course_logo.png',
		  overwrite = TRUE)

# Save Website icons
# Can create site favicon here using the square output: https://favicon.io/favicon-converter/
# To create a w x h image, use this formula: w * 300 / 600
ggsave(filename = 'website/static/apple-touch-icon.png',
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', dpi = 90)

ggsave(filename = 'website/static/android-chrome-192x192.png',
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', dpi = 96)

ggsave(filename = 'website/static/android-chrome-512x512.png',
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', dpi = 256)

# NOTE: The favicon.io seems to do a better job creating the small images
ggsave(filename = 'website/static/favicon-16x16.png',
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', dpi = 8)

ggsave(filename = 'website/static/favicon-32x32.png',
	   plot = sticker,
	   width = 50.8, height = 50.8, units = 'mm', dpi = 16)
