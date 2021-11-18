library(flametree)
library(ggforce)
library(paletteer)

devtools::install_github("riatelab/cartography")
library(cartography)

dat <- flametree_grow(seed = 2112,
					  time = 11,
					  split = 2,
					  angle = c(-10, 10, 20, -20),
					  prune = 0.1)
img <- flametree_plot(tree = dat,
					  palette = 'grDevices::Geyser',
					  background = 'white'
					  )
plot(img)


paletteer_dynamic("cartography::blue.pal", 5)

theme_mono <- function(color = "black") {
	ggplot2::theme_void() +
		ggplot2::theme(
			panel.background = ggplot2::element_rect(
				fill = color,
				colour = color
			)
		)
}

my_flametree_plot <- function (tree, background = "antiquewhite4", palette = "viridis::inferno") {
	mapping <- ggplot2::aes(x = coord_x, y = coord_y, group = id_path,
							size = seg_wid, color = seg_col)
	picture <- ggplot2::ggplot(data = tree, mapping = mapping) +
		ggforce::geom_bezier2(show.legend = FALSE, lineend = "round") +
		paletteer::scale_color_paletteer_c(palette = palette) +
		theme_mono(color = background)
	return(picture)
}

img <- flametree_plot(dat, background = 'white', palette = "pals::ocean.deep")
plot(img)

paletteer::scale_color_paletteer_c('viridis::inferno')

